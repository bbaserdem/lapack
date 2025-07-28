# UV boilerplate abstraction
# We want to take flake inputs, and the current system
{
  inputs,
  system,
  pythonProject,
  ...
}: rec {
  # Explicitly name our inputs that we'll use
  inherit (inputs) nixpkgs uv2nix pyproject-nix pyproject-build-systems;

  # Pull lib into scope
  inherit (nixpkgs) lib;

  # Create pkgs set from our current system
  pkgs = nixpkgs.legacyPackages.${system};
  inherit (pkgs) stdenv;

  # We use python 3.13
  python = pkgs.python313;
  baseSet = pkgs.callPackage pyproject-nix.build.packages {inherit python;};

  # Helper function to sanitize project names for directory names
  sanitizeName = name: builtins.replaceStrings ["-"] ["_"] name;

  # Helper function to recursively collect all workspaces
  collectWorkspaces = project: let
    subWorkspaces = lib.flatten (map collectWorkspaces (project.workspaces or []));
  in
    [project] ++ subWorkspaces;

  # Collect all workspaces from the pythonProject
  allWorkspaces = collectWorkspaces pythonProject;

  # Create a map of project names to their directories
  projectDirs = lib.listToAttrs (map (ws: {
    name = ws.name;
    value = sanitizeName ws.name;
  }) allWorkspaces);

  # Load the root workspace (which includes all nested packages)
  workspace = uv2nix.lib.workspace.loadWorkspace {
    workspaceRoot = pythonProject.directory;
  };

  # Load individual workspaces for granular access
  workspaces = lib.listToAttrs (map (ws: {
    name = ws.name;
    value = uv2nix.lib.workspace.loadWorkspace {
      workspaceRoot = ws.directory;
    };
  }) allWorkspaces);

  # Create package overlay from root workspace (includes all packages)
  overlay = workspace.mkPyprojectOverlay {
    # Prefer prebuilt binary wheels as a package source
    sourcePreference = "wheel";
  };

  # Extend generated overlay with build fixups for all packages
  pyprojectOverrides = final: prev: let
    # Apply overrides to each workspace package
    overridePackage = name: prev.${name}.overrideAttrs (old: {
      passthru =
        old.passthru
        // {
          tests = let
            virtualenv = final.mkVirtualEnv "${name}-pytest-env" {
              ${name} = [];
            };
          in
            (old.tests or {})
            // {
              pytest = stdenv.mkDerivation {
                name = "${final.${name}.name}-pytest";
                inherit (final.${name}) src;
                nativeBuildInputs = [
                  virtualenv
                ];
                dontConfigure = true;
                buildPhase = ''
                  runHook preBuild
                  pytest --cov tests --cov-report html
                  runHook postBuild
                '';
                installPhase = ''
                  runHook preInstall
                  mv htmlcov $out
                  runHook postInstall
                '';
              };
            };
        };
    });
  in
    # Apply overrides to all workspace packages
    lib.listToAttrs (map (ws: {
      name = ws.name;
      value = if prev ? ${ws.name} then overridePackage ws.name else null;
    }) (lib.filter (ws: prev ? ${ws.name}) allWorkspaces));

  # Construct package set with all overlays
  pythonSet = baseSet.overrideScope (
    lib.composeManyExtensions [
      pyproject-build-systems.overlays.default
      overlay
      pyprojectOverrides
    ]
  );

  # Create editable overlay for all packages
  editableOverlay = workspace.mkEditablePyprojectOverlay {
    root = "$REPO_ROOT";
  };

  # Editable python set with fixups for all packages
  editablePythonSet = pythonSet.overrideScope (
    lib.composeManyExtensions [
      editableOverlay

      # Apply fixups for building editable packages
      (final: prev: let
        # Apply editable fixups to each workspace package
        makeEditable = ws: {
          name = ws.name;
          value = prev.${ws.name}.overrideAttrs (old: {
            src = lib.fileset.toSource {
              root = ws.directory;
              fileset = lib.fileset.unions [
                (ws.directory + "/pyproject.toml")
                (ws.directory + "/README.md")
                (ws.directory + "/src/${projectDirs.${ws.name}}")
              ];
            };

            # Hatchling (build system) has a dependency on the editables package when building editables
            # In normal python flows this dependency is dynamically handled, in PEP660
            # With Nix, the dependency needs to be explicitly declared
            nativeBuildInputs =
              old.nativeBuildInputs
              ++ final.resolveBuildSystem {
                editables = [];
              };
          });
        };
      in
        lib.listToAttrs (map makeEditable (lib.filter (ws: prev ? ${ws.name}) allWorkspaces)))
    ]
  );

  # Single virtualenv with all packages from root workspace
  virtualenv =
    editablePythonSet.mkVirtualEnv
    "${pythonProject.name}-dev-env"
    workspace.deps.all;

  # Shell configuration
  uvShellSet = {
    packages = [virtualenv pkgs.uv];
    env = {
      UV_NO_SYNC = "1";
      UV_PYTHON = python;
      UV_PYTHON_DOWNLOADS = "never";
    };
    shellHook = ''
      # Undo dependency propagation by nixpkgs.
      unset PYTHONPATH

      # Get repository root using git. This is expanded at runtime by the editable `.pth` machinery.
      export REPO_ROOT=$(git rev-parse --show-toplevel)
    '';
  };
  
}