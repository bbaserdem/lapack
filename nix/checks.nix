{
  pkgs,
  uvBoilerplate,
  pythonProject,
  ...
}: let
  inherit (uvBoilerplate) lib stdenv pythonSet;

  # Create a pytest check directly for a package
  createPackageCheck = packageName:
    if (pythonSet ? ${packageName})
    then let
      pkg = pythonSet.${packageName};
      # Create a test environment with the package and pytest
      testEnv =
        {
          ${packageName} = [];
        }
        // (lib.optionalAttrs (pythonSet ? pytest) {pytest = [];})
        // (lib.optionalAttrs (pythonSet ? pytest-cov) {pytest-cov = [];});

      virtualenv = pythonSet.mkVirtualEnv "${packageName}-pytest-env" testEnv;
    in {
      "${packageName}-pytest" = stdenv.mkDerivation {
        name = "${pkg.name}-pytest";
        inherit (pkg) src;
        nativeBuildInputs = [virtualenv];

        # Set SSL certificates for httpx
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        # Force SQLite for tests in Nix environment
        USE_TEST_SQLITE = "1";
        dontConfigure = true;

        buildPhase = ''
          runHook preBuild
          # Run pytest in the package's test directory if it exists
          if [ -d "tests" ]; then
            pytest --cov ${packageName} tests --cov-report html
          else
            echo "No tests directory found for ${packageName}"
            exit 1
          fi
          runHook postBuild
        '';

        installPhase = ''
          runHook preInstall
          if [ -d "htmlcov" ]; then
            mv htmlcov $out
          else
            mkdir -p $out
            echo "No coverage report generated" > $out/no-coverage.txt
          fi
          runHook postInstall
        '';
      };
    }
    else {};

  # Get all package names (including root if not empty)
  allPackageNames =
    (
      if pythonProject.emptyRoot
      then []
      else [pythonProject.projectName]
    )
    ++ (map (ws: ws.projectName) pythonProject.workspaces);

  # Create checks for all packages
  packageChecks =
    lib.foldl' (
      acc: packageName:
        acc // (createPackageCheck packageName)
    ) {}
    allPackageNames;

  # Add integration tests for empty root if tests directory exists
  integrationTests =
    if pythonProject.emptyRoot
    then let
      # Create test environment with all workspace packages
      testEnv =
        lib.listToAttrs (map (name: {
          name = name;
          value = [];
        }) (map (ws: ws.projectName) pythonProject.workspaces))
        // (lib.optionalAttrs (pythonSet ? pytest) {pytest = [];})
        // (lib.optionalAttrs (pythonSet ? pytest-cov) {pytest-cov = [];});

      virtualenv = pythonSet.mkVirtualEnv "integration-tests-env" testEnv;
    in {
      "integration-tests" = stdenv.mkDerivation {
        name = "workspace-integration-tests";
        src = ../.;
        nativeBuildInputs = [virtualenv];

        # Set SSL certificates for httpx
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        # Force SQLite for tests in Nix environment
        USE_TEST_SQLITE = "1";
        dontConfigure = true;

        buildPhase = ''
          runHook preBuild
          # Check if tests directory exists and has actual test files
          if [ -d "tests" ] && find tests -name "test_*.py" -type f | grep -q .; then
            pytest tests -v
          else
            echo "No integration tests found in root tests directory"
            # This is not an error - integration tests are optional
          fi
          runHook postBuild
        '';

        installPhase = ''
          runHook preInstall
          mkdir -p $out
          echo "Integration tests passed" > $out/test-results.txt
          runHook postInstall
        '';
      };
    }
    else {};
in
  packageChecks // integrationTests

