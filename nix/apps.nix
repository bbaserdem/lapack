# Scripts for different types of swarms
{pkgs, ...}: let
  claudeFlowDeploy = import ./claudeFlow/deploy.nix {inherit pkgs;};
in {
  deploy-claudeFlow = {
    type = "app";
    program = "${claudeFlowDeploy}";
  };
}
