{pkgs, ...}: let
  entrypoint = import ./entryPoint.nix {inherit pkgs;};
  initFirewall = import ./initFirewall.nix {inherit pkgs;};
  earlySetup = import ./earlySetup.nix {inherit pkgs;};
in
  pkgs.dockerTools.buildImage {
    name = "aicontainer-claudeswarm";
    tag = "latest";
    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      paths = with pkgs; [
        nodePackages_latest.nodejs
        git
        bash
        coreutils-full
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
        findutils
        ripgrep
        shadow
        busybox
        cacert  # CA certificates bundle for SSL verification
        # User apps
        earlySetup
        initFirewall
        entrypoint
      ];
      pathsToLink = ["/bin" "/etc" "/usr"];
    };
    runAsRoot = "/bin/earlysetup";
    config = {
      User = "node";
      WorkingDir = "/workspace";
      Env = [
        # add any fixed env vars here, if needed
        "DEVCONTAINER=true"
        "NPM_CONFIG_PREFIX=/usr/local/share/npm-global"
        #"PATH=/usr/local/share/npm-global/bin:$PATH"
        "NODE_OPTIONS=--max-old-space-size=4096"
        "CLAUDE_CONFIG_DIR=/home/node/.claude"
        "POWERLEVEL9K_DISABLE_GITSTATUS=true"
        "NODE_EXTRA_CA_CERTS=/etc/ssl/certs/ca-bundle.crt"
        "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
        "NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
      ];
      Entrypoint = ["/bin/entrypoint"];
    };
  }
