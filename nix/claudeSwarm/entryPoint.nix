# Entry point for docker images
{pkgs, ...}:
pkgs.writeShellScript "entrypoint.sh" ''
  set -e
  cd /repo

  # Install deps if needed (only if node_modules absent, for speed)
  if [ ! -d node_modules ]; then
    echo "node_modules missing, running npm install..."
    npm install
  else
    echo "node_modules exists, skipping npm install"
  fi

  # .env file is assumed present in /repo already, picked up by npm packages

  # Run the requested command; pass through all arguments
  # npx claude-flow@alpha hive-mind spawn "$@" --agents 8 --claude

  # Just do testing now;
  echo "Command to run: npx claude-flow@alpha hive-mind spawn <PROMPT> --agents 8 --claude"
  echo "All args: $@"
''
