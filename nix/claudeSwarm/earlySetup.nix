# Entry point for docker images
{pkgs, ...}:
pkgs.writeShellApplication {
  name = "earlysetup";
  runtimeInputs = with pkgs; [
    coreutils
    shadow
    sudo
  ];
  text = let
    mkdir = "${pkgs.coreutils}/bin/mkdir";
    groupadd = "${pkgs.shadow}/bin/groupadd";
    useradd = "${pkgs.shadow}/bin/useradd";
    chown = "${pkgs.coreutils}/bin/chown";
    touch = "${pkgs.coreutils}/bin/touch";
    chmod = "${pkgs.coreutils}/bin/chmod";
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
  '';
}
