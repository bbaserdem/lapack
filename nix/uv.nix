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
    })
    allWorkspaces);

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
    })
    allWorkspaces);

  # Filter workspaces to only include those with src directories for building
  buildableWorkspaces = lib.filter (ws: builtins.pathExists (ws.directory + "/src")) allWorkspaces;

  # Create a filtered workspace that only includes packages with src directories
  filteredWorkspaceMembers = map (ws: ws.name) buildableWorkspaces;

  # Create package overlay from root workspace (includes all packages)
  baseOverlay = workspace.mkPyprojectOverlay {
    # Prefer prebuilt binary wheels as a package source
    sourcePreference = "wheel";
  };

  # Create a filtered overlay that excludes workspace packages without src directories
  overlay = final: prev: let
    # Get all packages from the base overlay
    basePackages = baseOverlay final prev;

    # Get names of all workspace packages (including root)
    allWorkspaceNames = map (ws: ws.name) allWorkspaces;

    # Filter out only the workspace packages that don't have src directories
    filteredPackages =
      lib.filterAttrs (
        name: value:
        # Keep the package if:
        # 1. It's not a workspace package (external dependency like matplotlib)
        # 2. OR it's a workspace package that has a src directory
          !(lib.elem name allWorkspaceNames) || (lib.any (ws: ws.name == name && builtins.pathExists (ws.directory + "/src")) allWorkspaces)
      )
      basePackages;
  in
    filteredPackages;

  # Extend generated overlay with build fixups for all packages
  pyprojectOverrides = final: prev: let
    inherit (final) resolveBuildSystem;

    # Build system dependencies for problematic packages
    buildSystemOverrides = {
      pygraphviz = {
        setuptools = [];
        wheel = [];
      };
    };

    # Apply build system overrides
    buildSystemOverridePackages =
      lib.mapAttrs (
        name: spec:
          if prev ? ${name}
          then
            prev.${name}.overrideAttrs (old: {
              nativeBuildInputs =
                old.nativeBuildInputs
                ++ resolveBuildSystem spec
                ++ lib.optionals (name == "pygraphviz") [
                  pkgs.graphviz
                  pkgs.pkg-config
                ];
            })
          else null
      )
      buildSystemOverrides;

    # Apply overrides to each workspace package
    overridePackage = name:
      prev.${name}.overrideAttrs (old: {
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

    # Apply overrides only to buildable workspace packages
    workspaceOverrides = lib.listToAttrs (map (ws: {
      name = ws.name;
      value =
        if prev ? ${ws.name}
        then overridePackage ws.name
        else null;
    }) (lib.filter (ws: prev ? ${ws.name}) buildableWorkspaces));
  in
    # Combine build system overrides and workspace overrides
    buildSystemOverridePackages // workspaceOverrides;

  # Construct package set with all overlays
  pythonSet = baseSet.overrideScope (
    lib.composeManyExtensions [
      pyproject-build-systems.overlays.default
      overlay
      pyprojectOverrides
    ]
  );

  # Create editable overlay only for packages with src directories
  # Filter out packages without src directories (like the root workspace)
  packagesWithSrc = lib.filter (ws: builtins.pathExists (ws.directory + "/src")) allWorkspaces;
  editableMembers = map (ws: ws.name) packagesWithSrc;

  editableOverlay = workspace.mkEditablePyprojectOverlay {
    root = "$REPO_ROOT";
    members = editableMembers;
  };

  # Editable python set with fixups for all packages
  editablePythonSet = pythonSet.overrideScope (
    lib.composeManyExtensions [
      editableOverlay

      # Apply fixups for building editable packages
      (final: prev: let
        # Check if a package has a src directory
        hasSrcDir = ws: builtins.pathExists (ws.directory + "/src");

        # Apply editable fixups to each workspace package that has src
        makeEditable = ws:
          if hasSrcDir ws
          then {
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
          }
          else {
            # For packages without src, don't make them editable
            name = ws.name;
            value = prev.${ws.name};
          };

        # Handle the root package - check if it has src directory
        rootPackageOverride =
          if prev ? ${pythonProject.name}
          then
            if hasSrcDir pythonProject
            then {
              # Root has src, make it editable
              ${pythonProject.name} = prev.${pythonProject.name}.overrideAttrs (old: {
                src = lib.fileset.toSource {
                  root = pythonProject.directory;
                  fileset = lib.fileset.unions [
                    (pythonProject.directory + "/pyproject.toml")
                    (pythonProject.directory + "/README.md")
                    (pythonProject.directory + "/src/${sanitizeName pythonProject.name}")
                  ];
                };

                nativeBuildInputs =
                  old.nativeBuildInputs
                  ++ final.resolveBuildSystem {
                    editables = [];
                  };
              });
            }
            else {
              # Root has no src, don't make it editable
              ${pythonProject.name} = prev.${pythonProject.name};
            }
          else {};
      in
        # Combine workspace and root package overrides
        lib.listToAttrs (map makeEditable (lib.filter (ws: prev ? ${ws.name}) allWorkspaces))
        // rootPackageOverride)
    ]
  );

  # Filter dependencies to only include buildable packages
  buildableDeps =
    lib.filterAttrs (
      name: deps:
        lib.any (ws: ws.name == name) buildableWorkspaces
    )
    workspace.deps.all;

  # Single virtualenv with only buildable packages
  virtualenv =
    editablePythonSet.mkVirtualEnv
    "${pythonProject.name}-dev-env"
    buildableDeps;

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

