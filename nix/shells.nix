# shell.nix
{
  pkgs,
  inputs,
  system,
  uvBoilerplate,
  projectName,
  ...
}: let
  # UV stuff
  inherit (uvBoilerplate) uvShellSet;
  # Input packages
  quaestor = inputs.quaestor.packages.${system}.default;

  # Import Neo4j scripts
  neo4jScripts = import ./neo4j-scripts.nix {inherit pkgs;};

  # Shell aliases
  # Function to create script
  mkScript = name: text: let
    script = pkgs.writeShellScriptBin name text;
  in
    script;

  # Define script; these are going to be functionally aliases
  scripts = [
    (mkScript "tm" ''npx --yes --package=task-master-ai task-master "$@"'')
  ];

  # Default shell
  defaultPackages = with pkgs; [
    git
    # Grab build tools
    gfortran
    cmake
    # Needed for mcp's and claude
    nodejs
    typescript
    quaestor
    # Shell aliases
    scripts
    # Visualizing computational graph
    neo4j
    # Neo4j management scripts
    neo4jScripts.neo4j-start
    neo4jScripts.neo4j-stop
    neo4jScripts.neo4j-status
    neo4jScripts.neo4j-console
  ];
  defaultHooks = ''
    # Make our local node packages available to our shell; for mcp's
    export PATH="./node_modules/.bin:$PATH"

    # Set Neo4j environment variables to use local project directory
    export NEO4J_CONF="$PWD/neo4j-data"
    export NEO4J_HOME="$PWD/neo4j-data"
  '';
  defaultEnv = {};

  # Devops branch
  devopsPackages = with pkgs; [
    podman
  ];
  devopsHooks = "";
  devopsEnv = {};

  # Organize branch
  organizePackages = with pkgs; [
    haskellPackages.fortran-src
    uv # For fortdepend
  ];
  organizeHooks = ''
    # Check Neo4j status when entering the shell
    if [ -f "$PWD/neo4j-data/neo4j.conf" ]; then
      if pgrep -f "neo4j.*''${PWD}/neo4j-data" > /dev/null; then
        echo "Neo4j is already running for this project"
        echo "Access the browser at: http://localhost:7474"
      else
        echo "Neo4j is not running. Start it with: neo4j-start"
      fi
    else
      echo "Neo4j config not found at $PWD/neo4j-data/neo4j.conf"
      echo "Set up Neo4j data directory first, then use 'neo4j-start'"
    fi

    echo ""
    echo "Neo4j commands available:"
    echo "  neo4j-start   - Start Neo4j server in background"
    echo "  neo4j-stop    - Stop Neo4j server"
    echo "  neo4j-status  - Check Neo4j status"
    echo "  neo4j-console - Run Neo4j in foreground (Ctrl+C to stop)"
  '';
  organizeEnv = {
  };
in {
  # Main dev shell
  default = pkgs.mkShell {
    packages = defaultPackages;
    # Shell hooks
    shellHook = defaultHooks;
  };

  # Devops dev shell, we do docker stuff here
  devops = pkgs.mkShell {
    packages = defaultPackages ++ devopsPackages;
    # Shell hooks
    shellHook = defaultHooks + "\n" + devopsHooks;
  };

  # Organize dev shell; generate code reports as well
  organize = pkgs.mkShell {
    packages = uvShellSet.packages ++ defaultPackages ++ organizePackages;
    env = defaultEnv // organizeEnv // uvShellSet.env;
    shellHook = defaultHooks + "\n" + organizeHooks + "\n" + uvShellSet.shellHook;
  };
}
