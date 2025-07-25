# UV boilerplate abstraction
# We want to take flake inputs, nd the current system
{
  inputs,
  system,
  projectName,
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
  python = pkgs.python312;
  baseSet = pkgs.callPackage pyproject-nix.build.packages {inherit python;};

  # We load a uv workspace from a workspace root
  workspace = uv2nix.lib.workspace.loadWorkspace {workspaceRoot = ../.;};

  # Create package overlay from workspace
  overlay = workspace.mkPyprojectOverlay {
    # Prefer prebuilt binary wheels as a package source
    sourcePreference = "wheel";
  };

  # Extend generated overlay with build fixups
  #
  # Uv2nix can only work with what it has, and uv.lock is missing essential metadata to perform some builds.
  # This is an additional overlay implementing build fixups.
  # See:
  # - https://pyproject-nix.github.io/uv2nix/FAQ.html
  pyprojectOverrides = final: prev: {
    # Implement build fixups here.
    # Note that uv2nix is _not_ using Nixpkgs buildPythonPackage.
    # It's using https://pyproject-nix.github.io/pyproject.nix/build.html
    ${projectName} = prev.${projectName}.overrideAttrs (old: {
      passthru =
        old.passthru
        // {
          # Put all tests in the passthru.tests attribute set.
          # Nixpkgs also uses the passthru.tests mechanism for ofborg test discovery.
          #
          # For usage with Flakes we will refer to the passthru.tests attributes
          # to construct the flake checks attribute set.
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

                # Because this package is running tests, and not actually building the main package
                # the build phase is running the tests.
                #
                # In this particular example we also output a HTML coverage report, which is used as the build output.
                buildPhase = ''
                  runHook preBuild
                  pytest --cov tests --cov-report html
                  runHook postBuild
                '';

                # Install the HTML coverage report into the build output.
                #
                # If you wanted to install multiple test output formats such as TAP outputs
                # you could make this derivation a multiple-output derivation.
                #
                # See https://nixos.org/manual/nixpkgs/stable/#chap-multiple-output for more information on multiple outputs.
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
      (import ./pysideOverride.nix {inherit pkgs;})
    ]
  );

  # Construct a set to deep merge into shell spec

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
          # It's a good idea to filter the sources going into an editable build
          # so the editable package doesn't have to be rebuilt on every stage
          src = lib.fileset.toSource {
            root = old.src;
            fileset = lib.fileset.unions [
              (old.src + "/pyproject.toml")
              (old.src + "/README.md")
              (old.src + "/src/${projectName}/__init__.py")
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
