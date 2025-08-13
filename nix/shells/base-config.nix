# Base shell configuration that provides common functionality
# This is meant to be composed with other shell configurations
{
  pkgs,
  uvBoilerplate ? null,
  ...
}: {
  # Base development packages available in all shells
  packages = with pkgs; [
    git
    nodejs-slim
    pnpm
    uv
    # Fortran build tools
    gfortran
    cmake
    openblas
    # Docs
    mdbook
    # Project reqs
    neo4j
    haskellPackages.fortran-src
    # Rust
    rustc
    cargo
    rustfmt
    clippy
    rust-analyzer
    # For bindgen
    rust-bindgen
    clang
    # Basic tools for dev environment
    coreutils # Basic file, shell and text manipulation utilities
    findutils # Find, locate, and xargs commands
    gnugrep # GNU grep, egrep and fgrep
    gnused # GNU stream editor
    ripgrep # Fast line-oriented search tool
    fd # Simple, fast and user-friendly alternative to find
    bat # Cat clone with syntax highlighting
    eza # Modern replacement for ls
    htop # Interactive process viewer
    jq # Lightweight JSON processor
    watch # Execute a program periodically
    curl # Command line tool for transferring data
    wget # Internet file retriever
    tree # Display directories as trees
    unzip # Unzip utility
    zip # Zip utility
    # Extra utilities
    mermaid-cli # Mermaid diagrams
  ];

  # Base environment variables
  env = {
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

  # Base shell hooks
  shellHook = ''
    # Set Neo4j environment variables to use local project directory
    export NEO4J_CONF="$PWD/neo4j-data"
    export NEO4J_HOME="$PWD/neo4j-data"
  '';

  # Python environment integration (if uvBoilerplate is provided)
  python =
    if uvBoilerplate != null
    then {
      packages = uvBoilerplate.uvShellSet.packages;
      env = uvBoilerplate.uvShellSet.env;
      shellHook = uvBoilerplate.uvShellSet.shellHook;
    }
    else {
      packages = [];
      env = {};
      shellHook = "";
    };
}
