# Python-only shell - just Python environment, no base tools
{
  pkgs,
  baseConfig,
  ...
}: {
  packages = baseConfig.python.packages;
  env = baseConfig.python.env;
  shellHook = baseConfig.python.shellHook;
}
