# neo4j-scripts.nix
{pkgs}: let
  mkScript = name: text: pkgs.writeShellScriptBin name text;
  neo4j = "${pkgs.neo4j}/bin/neo4j";
  pgrep = "${pkgs.procps}/bin/pgrep";
in {
  neo4j-start = mkScript "neo4j-start" ''
    if ${pgrep} -f "neo4j.*''${PWD}/neo4j-data" > /dev/null; then
      echo "Neo4j is already running for this project"
      echo "Access the browser at: http://localhost:7474"
    else
      echo "Starting Neo4j server..."
      ${neo4j}/bin/neo4j start
      sleep 3
      if ${neo4j}/bin/neo4j status > /dev/null 2>&1; then
        echo "Neo4j started successfully!"
        echo "Access the browser at: http://localhost:7474"
      else
        echo "Failed to start Neo4j. Check logs at: $PWD/neo4j-data/logs/"
      fi
    fi
  '';

  neo4j-stop = mkScript "neo4j-stop" ''
    if ${pgrep} -f "neo4j.*''${PWD}/neo4j-data" > /dev/null; then
      echo "Stopping Neo4j server..."
      ${neo4j}/bin/neo4j stop
    else
      echo "Neo4j is not running for this project"
    fi
  '';

  neo4j-status = mkScript "neo4j-status" ''
    if [ -f "$PWD/neo4j-data/neo4j.conf" ]; then
      if ${pgrep} -f "neo4j.*''${PWD}/neo4j-data" > /dev/null; then
        echo "Neo4j is running for this project"
        echo "Access the browser at: http://localhost:7474"
        ${neo4j}/bin/neo4j status
      else
        echo "Neo4j is not running for this project"
        echo "Start it with: neo4j-start"
      fi
    else
      echo "Neo4j config not found at $PWD/neo4j-data/neo4j.conf"
      echo "Set up Neo4j data directory first"
    fi
  '';

  neo4j-console = mkScript "neo4j-console" ''
    echo "Starting Neo4j in console mode (foreground)..."
    echo "Press Ctrl+C to stop"
    ${neo4j}/bin/neo4j console
  '';
}

