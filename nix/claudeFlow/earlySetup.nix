# Entry point for docker images
{pkgs, ...}:
pkgs.writeShellApplication {
  name = "earlysetup";
  runtimeInputs = with pkgs; [
    coreutils-full
    shadow
    sudo
    bash
  ];
  text = let
    mkdir = "${pkgs.coreutils-full}/bin/mkdir";
    groupadd = "${pkgs.shadow}/bin/groupadd";
    useradd = "${pkgs.shadow}/bin/useradd";
    chown = "${pkgs.coreutils-full}/bin/chown";
    touch = "${pkgs.coreutils-full}/bin/touch";
    chmod = "${pkgs.coreutils-full}/bin/chmod";
    ln = "${pkgs.coreutils-full}/bin/ln";
  in ''
    # Create node user and group
    ${groupadd} -g 1000 node
    ${useradd} -m -u 1000 -g node node

    # Create required directories
    ${mkdir} -p /commandhistory
    ${mkdir} -p /workspace
    ${mkdir} -p /home/node/.claude
    ${mkdir} -p /usr/local/share/npm-global

    # Set ownership
    ${chown} -R node:node /commandhistory
    ${chown} -R node:node /workspace
    ${chown} -R node:node /home/node
    ${chown} -R node:node /usr/local/share/npm-global

    # Create bash history file
    ${touch} /commandhistory/.bash_history
    ${chown} node:node /commandhistory/.bash_history

    # Set up sudo for firewall script
    echo "node ALL=(root) NOPASSWD: /bin/initfirewall" > /etc/sudoers.d/node-firewall
    ${chmod} 0440 /etc/sudoers.d/node-firewall

    # Let us mimic FSH standards
    ${mkdir} -p /usr/bin
    ${ln} -s /bin/env /usr/bin/env
  '';
}
