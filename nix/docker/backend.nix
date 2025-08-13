{
  pkgs,
  pythonPackages,
  ...
}: let
  entrypoint = pkgs.writeShellScriptBin "entrypoint" ''
    exec /bin/mathaide-backend "$@"
  '';
in
  pkgs.dockerTools.buildImage {
    name = "mathaide-backend";
    tag = "latest";
    copyToRoot = pkgs.buildEnv {
      name = "backend-root";
      paths =
        (with pkgs; [
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
          cacert # CA certificates bundle for SSL verification
          uv # Python package manager
          nodejs-slim # Node environment
          pnpm
        ])
        ++ [
          pythonPackages.mathaide-backend
          entrypoint
        ];
      pathsToLink = ["/bin" "/etc" "/usr"];
    };
    runAsRoot = ''
      #!${pkgs.bash}/bin/bash
      # Create user and group
      ${pkgs.shadow}/bin/groupadd -r mathaide
      ${pkgs.shadow}/bin/useradd -r -g mathaide -d /home/mathaide -s ${pkgs.bash}/bin/bash mathaide
      mkdir -p /home/mathaide
      chown -R mathaide:mathaide /home/mathaide
    '';
    config = {
      User = "mathaide";
      WorkingDir = "/home/mathaide";
      Env = [
        # add any fixed env vars here, if needed
        "DEVCONTAINER=true"
        #"NPM_CONFIG_PREFIX=/usr/local/share/npm-global"
        #"PATH=/usr/local/share/npm-global/bin:$PATH"
        #"NODE_OPTIONS=--max-old-space-size=4096"
        "POWERLEVEL9K_DISABLE_GITSTATUS=true"
        #"NODE_EXTRA_CA_CERTS=/etc/ssl/certs/ca-bundle.crt"
        "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
        "NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
      ];
      Entrypoint = ["/bin/entrypoint"];
    };
  }
