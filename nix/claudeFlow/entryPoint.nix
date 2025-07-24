# Entry point for docker images
{pkgs, ...}:
pkgs.writeShellApplication {
  name = "entrypoint";
  runtimeInputs = with pkgs; [
    nodePackages_latest.nodejs
    git
  ];
  text = let
    node = pkgs.nodePackages_latest.nodejs;
    npm = "${pkgs.nodePackages_latest.nodejs}/lib/node_modules/npm/bin/npm-cli.js";
    npx = "${pkgs.nodePackages_latest.nodejs}/lib/node_modules/npm/bin/npx-cli.js";
    workspaceNode = "/workspace/node_modules/.bin";
    localNode = "/usr/local/share/npm-global/bin";
  in ''
    set -e
    cd /workspace

    # Set up path to include local node_modules binaries
    export PATH="${workspaceNode}:${localNode}:$PATH"
    # Also set NODE_PATH for module resolution
    export NODE_PATH="/workspace/node_modules:${node}/lib/node_modules"

    # Install deps
    ${npm} install

    # .env file is assumed present in /workspace already, picked up by npm packages
    if [ ! -f .env ]; then
      echo "ERROR: .env file missing in /workspace"
      exit 1
    fi

    # Configure claude
    # ${npx} claude --dangerously-skip-permissions

    # Configure claude-flow
    ${npx} claude-flow@alpha init --force

    # Run the requested command; pass through all arguments
    ${npx} claude-flow@alpha "$@"
  '';
}
