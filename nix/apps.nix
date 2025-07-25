# Scripts for different types of swarms
{
  pkgs,
  uvBoilerplate,
  projectName,
  outputs,
  ...
}: let
  claudeFlowDeploy = import ./claudeFlow/deploy.nix {inherit pkgs;};
in {
  deploy-claudeFlow = {
    type = "app";
    program = "${claudeFlowDeploy}";
  };
  ${projectName} = {
    type = "app";
    # THIS NEEDS TO CHANGE
    program = "${outputs.packages.${system}.default}/bin/${projectName}";
  };
}
