# Documentation shell - uv2nix environment with docs dependency group
{
  pkgs,
  baseConfig,
  uvBoilerplate,
  ...
}: let
  inherit (baseConfig) python;
  inherit (uvBoilerplate) workspace workspaces editablePythonSet uvShellSet;

  # Create docs virtualenv with docs dependencies
  docsVirtualenv = editablePythonSet.mkVirtualEnv
    "workspace-docs-env"
    workspace.deps.all;

  # Additional system tools for documentation
  docsPackages = with pkgs; [
    # Markdown tools
    pandoc
    # Diagram generation
    graphviz
    plantuml
  ];

  # Documentation environment variables
  docsEnv = {
    SPHINXOPTS = "-W"; # Treat warnings as errors
  };

  # Documentation shell hooks
  docsHook = ''
    echo "📚 Documentation environment loaded with docs dependencies"
    echo "Use 'sphinx-build' to build docs"
  '';

in {
  packages = uvShellSet.packages ++ [docsVirtualenv] ++ docsPackages;
  env = uvShellSet.env // docsEnv;
  shellHook = uvShellSet.shellHook + "\n" + docsHook;
}
