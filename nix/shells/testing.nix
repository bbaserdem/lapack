# Testing shell - uv2nix environment with testing dependency group
{
  pkgs,
  baseConfig,
  uvBoilerplate,
  ...
}: let
  inherit (baseConfig) python;
  inherit (uvBoilerplate) workspace workspaces editablePythonSet uvShellSet;

  # Create testing virtualenv with testing dependencies
  testingVirtualenv = editablePythonSet.mkVirtualEnv
    "workspace-testing-env"
    workspace.deps.all;

  # Additional system tools for testing
  testingPackages = with pkgs; [
    html-tidy # For HTML validation
    xmlstarlet # For XML testing
  ];

  # Testing environment variables
  testingEnv = {
    PYTEST_DISABLE_PLUGIN_AUTOLOAD = "1"; # More predictable test runs
    COVERAGE_CORE = "sysmon"; # Better coverage tracking
  };

  # Testing shell hooks
  testingHook = ''
    echo "🧪 Testing environment loaded with testing dependencies"
    echo "Run 'pytest --cov' for coverage reports"
  '';

in {
  packages = uvShellSet.packages ++ [testingVirtualenv] ++ testingPackages;
  env = uvShellSet.env // testingEnv;
  shellHook = uvShellSet.shellHook + "\n" + testingHook;
}
