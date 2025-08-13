# Development shell - base tools + Python integration (the default)
{
  pkgs,
  baseConfig,
  ...
}: {
  packages = baseConfig.packages ++ baseConfig.python.packages;
  env = baseConfig.env // baseConfig.python.env;
  shellHook = baseConfig.shellHook + "\n" + baseConfig.python.shellHook;
}
