# UV boilerplate abstraction
# We want to take flake inputs, nd the current system
{
  inputs,
  system,
  projectName,
  projectDir,
  ...
}: rec {
  # Explicitly name our inputs that we'll use
  inherit (inputs) nixpkgs uv2nix pyproject-nix pyproject-build-systems;

  # Pull lib into scope
  inherit (nixpkgs) lib;

  # Create pkgs set from our current system
  pkgs = nixpkgs.legacyPackages.${system};
  inherit (pkgs) stdenv;

  # We use python 3.12
  python = pkgs.python313;
  baseSet = pkgs.callPackage pyproject-nix.build.packages {inherit python;};

  # We load a uv workspace from a workspace root
  workspace = uv2nix.lib.workspace.loadWorkspace {workspaceRoot = ../.;};

  # Create package overlay from workspace
  overlay = workspace.mkPyprojectOverlay {
    # Prefer prebuilt binary wheels as a package source
    sourcePreference = "wheel";
  };

  # Extend generated overlay with build fixups
  pyprojectOverrides = final: prev: {
    ${projectName} = prev.${projectName}.overrideAttrs (old: {
      passthru =
        old.passthru
        // {
          tests = let
            virtualenv = final.mkVirtualEnv "${projectName}-pytest-env" {
              ${projectName} = ["test"];
            };
          in
            (old.tests or {})
            // {
              pytest = stdenv.mkDerivation {
                name = "${final.${projectName}.name}-pytest";
                inherit (final.${projectName}) src;
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
  };

  # Construct package set
  pythonSet = baseSet.overrideScope (
    lib.composeManyExtensions [
      pyproject-build-systems.overlays.default
      overlay
      pyprojectOverrides
    ]
  );

  # Python stuff
  editableOverlay = workspace.mkEditablePyprojectOverlay {
    root = "$REPO_ROOT";
  };
  editablePythonSet = pythonSet.overrideScope (
    lib.composeManyExtensions [
      editableOverlay

      # Apply fixups for building an editable package of your workspace packages
      (final: prev: {
        ${projectName} = prev.${projectName}.overrideAttrs (old: {
          src = lib.fileset.toSource {
            root = old.src;
            fileset = lib.fileset.unions [
              (old.src + "/pyproject.toml")
              (old.src + "/README.md")
              (old.src + "/src/${projectDir}/__init__.py")
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
      })
    ]
  );
  virtualenv =
    editablePythonSet.mkVirtualEnv
    "${projectName}-dev-env"
    workspace.deps.all;

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
