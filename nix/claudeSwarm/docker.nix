{pkgs, ...}: let
  entrypoint = import ./entryPoint.nix {inherit pkgs;};
in
  pkgs.dockerTools.buildImage {
    name = "claude-flow-container-claudeSwarm";
    tag = "latest";
    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      paths = with pkgs; [
        nodejs-slim_24
        git
        bash
        coreutils
        gawk
        gnugrep
        sed
        entrypoint
      ];
      pathsToLink = ["/bin"];
    };
    config = {
      WorkingDir = "/repo";
      Env = [
        # add any fixed env vars here, if needed
        # "EXAMPLE_ENV=VALUE"
      ];
      Entrypoint = ["/bin/entrypoint"];
    };
  }
