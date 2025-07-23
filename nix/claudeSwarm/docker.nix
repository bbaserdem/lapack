{pkgs, ...}: let
  entrypoint = import ./entryPoint.nix {inherit pkgs;};
  initFirewall = import ./initFirewall.nix {inherit pkgs;};
  earlySetup = import ./earlySetup.nix {inherit pkgs;};
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
        gnused
        less
        procps
        sudo
        fzf
        man
        unzip
        gnupg
        jq
        wget
        # User apps
        earlySetup
        initFirewall
        entrypoint
      ];
      pathsToLink = ["/bin" "/etc"];
    };
    runAsRoot = ["/bin/earlysetup"];
    config = {
      User = "node";
      WorkingDir = "/workspace";
      Env = [
        # add any fixed env vars here, if needed
        "DEVCONTAINER=true"
        "NPM_CONFIG_PREFIX=/usr/local/share/npm-global"
        "PATH=/usr/local/share/npm-global/bin:$PATH"
        "NODE_OPTIONS=--max-old-space-size=4096"
        "CLAUDE_CONFIG_DIR=/home/node/.claude"
        "POWERLEVEL9K_DISABLE_GITSTATUS=true"
      ];
      Entrypoint = ["/bin/entrypoint"];
    };
  }
