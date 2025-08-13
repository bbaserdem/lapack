# Full shell - development + testing + documentation tools
{
  pkgs,
  baseConfig,
  uvBoilerplate,
  ...
}: let
  # Get testing configuration
  testingConfig = import ./testing.nix { inherit pkgs baseConfig uvBoilerplate; };
  
  # Get documentation configuration  
  docsConfig = import ./documentation.nix { inherit pkgs baseConfig uvBoilerplate; };
  
  # Merge unique packages (remove duplicates)
  allPackages = pkgs.lib.unique (testingConfig.packages ++ docsConfig.packages);
  
  # Merge environments
  allEnv = testingConfig.env // docsConfig.env;
  
  # Combine shell hooks
  allHooks = testingConfig.shellHook + "\n" + docsConfig.shellHook;

in {
  packages = allPackages;
  env = allEnv;
  shellHook = allHooks;
}
