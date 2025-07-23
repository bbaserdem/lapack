# Scripts for different types of swarms
{pkgs, ...}: let
  claudeSwarmDeploy = import ./claudeSwarm/deploy.nix {inherit pkgs;};
in {
  deploy-claudeSwarm = {
    type = "app";
    program = "${claudeSwarmDeploy}";
  };
}
