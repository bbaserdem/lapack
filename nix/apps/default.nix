# Application definitions - modular app management
{
  pkgs,
  inputs,
  system,
  uvBoilerplate,
  pythonProject,
  outputs,
  ...
}: let
  inherit (pkgs) lib;
  claudeFlowDeploy = import ./../claudeFlow/deploy.nix {inherit pkgs;};

  # Import app modules
  pythonApps = import ./python.nix {
    inherit pkgs uvBoilerplate pythonProject outputs;
  };

  scriptApps = import ./scripts.nix {
    inherit pkgs outputs;
  };
in
  {
    deployClaudeFlow = {
      type = "app";
      program = "${claudeFlowDeploy}";
    };
  }
  // pythonApps # Python workspace applications
  // scriptApps
# Custom runnable scripts

