# Entry point for docker images
{pkgs, ...}:
pkgs.writeShellApplication {
  name = "entrypoint";
  runtimeInputs = with pkgs; [
    nodejs-slim_24
    git
    coreutils
  ];
  text = let
    npm = "${pkgs.nodejs-slim_24}/lib/node_modules/npm/bin/npm-cli.js";
    npx = "${pkgs.nodejs-slim_24}/lib/node_modules/npm/bin/npx-cli.js";
  in ''
    set -e
    cd /workspace

    # Install deps if needed (only if node_modules absent, for speed)
    if [ ! -d node_modules ]; then
      echo "node_modules missing, running npm install..."
      ${npm} install
    else
      echo "node_modules exists, skipping npm install"
    fi

    # .env file is assumed present in /workspace already, picked up by npm packages
    if [ ! -f .env ]; then
      echo "ERROR: .env file missing in /workspace"
      exit 1
    fi

    # Configure claude
    ${npx} claude --dangerously-skip-permissions

    # Configure claude-flow
    ${npx} claude-flow@alpha init --force

    # Run the requested command; pass through all arguments
    # ${npx} claude-flow@alpha hive-mind spawn "$@" --agents 8 --claude

    # Just do testing for now
    echo "Command to run: np-x claude-flow@alpha hive-mind spawn <PROMPT> --agents 8 --claude"
    echo "All args: $*"
  '';
}
