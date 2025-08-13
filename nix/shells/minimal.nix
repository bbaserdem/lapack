# Minimal shell - just base tools, no Python integration
{
  pkgs,
  baseConfig,
  ...
}: {
  packages = baseConfig.packages;
  env = baseConfig.env;
  shellHook = baseConfig.shellHook;
}
