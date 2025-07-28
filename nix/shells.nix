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
    pnpm
    typescript
    uv
    quaestor
    # Shell aliases
    scripts
    # Visualizing computational graph
    neo4j
    haskellPackages.fortran-src
  ];
  defaultHooks = ''
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
      if pgrep -f "neo4j.*$PWD/neo4j-data" > /dev/null 2>&1; then
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
    echo "  ${projectName} neo4j start   - Start Neo4j server in background"
    echo "  ${projectName} neo4j stop    - Stop Neo4j server"
    echo "  ${projectName} neo4j status  - Check Neo4j status"
    echo "  ${projectName} neo4j console - Run Neo4j in foreground (Ctrl+C to stop)"
  '';
  organizeEnv = {
  };

  # Organize branch
  featurePackages = with pkgs; [
    # Rust toolchain
    rustc
    cargo
    rustfmt
    clippy
    rust-analyzer

    # For bindgen
    rust-bindgen
    clang

    # For linking with LAPACK
    pkg-config
    openblas

    # Documentation
    mdbook
  ];
  featureHooks = ''
    echo "Rust development environment loaded"
    echo "  cargo: $(cargo --version)"
    echo "  rustc: $(rustc --version)"
    echo "  bindgen: $(bindgen --version)"
    echo "  clang: $(clang --version | head -n1)"
  '';
  featureEnv = {
    # For bindgen to find libclang
    LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
    # For rust-bindgen
    BINDGEN_EXTRA_CLANG_ARGS = "-I${pkgs.openblas.dev}/include";
    # Add LAPACK libraries from result symlink to library path
    LD_LIBRARY_PATH = "$PWD/result/lib:${pkgs.openblas}/lib";
    # Set PKG_CONFIG_PATH to find LAPACK
    PKG_CONFIG_PATH = "$PWD/result/lib/cmake:${pkgs.openblas.dev}/lib/pkgconfig";
    # Add include path for LAPACK headers
    C_INCLUDE_PATH = "$PWD/result/include:${pkgs.openblas.dev}/include";
    CPLUS_INCLUDE_PATH = "$PWD/result/include:${pkgs.openblas.dev}/include";
  };
in {
  # Main dev shell, contain everything
  default = pkgs.mkShell {
    packages =
      uvShellSet.packages
      ++ defaultPackages
      ++ devopsPackages
      ++ organizePackages
      ++ featurePackages;
    env =
      defaultEnv
      // organizeEnv
      // uvShellSet.env
      // featureEnv;
    # Shell hooks
    shellHook =
      defaultHooks
      + "\n"
      + devopsHooks
      + "\n"
      + organizeHooks
      + "\n"
      + uvShellSet.shellHook
      + "\n"
      + featureHooks;
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

  # Features dev shell; generate code reports as well
  feature = pkgs.mkShell {
    packages = defaultPackages ++ featurePackages;
    env = defaultEnv // featureEnv;
    shellHook = defaultHooks + "\n" + featureHooks;
  };
}
