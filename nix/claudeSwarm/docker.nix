{pkgs, ...}: let
  entrypoint = import ./entryPoint.nix {inherit pkgs;};
in
  pkgs.dockerTools.buildImage {
    name = "claude-flow-container-claudeSwarm";
    contents = [
      pkgs.nodejs-slim
      pkgs.git
      pkgs.bash
      entrypoint
    ];
    tag = "latest";
    config = {
      WorkingDir = "/repo";
      Env = [
        # add any fixed env vars here, if needed
        # "EXAMPLE_ENV=VALUE"
      ];
      Entrypoint = ["/bin/sh" entrypoint];
    };
  }
