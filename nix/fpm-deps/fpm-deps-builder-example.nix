# Example of how fpm-deps would be packaged with our FPM builder
# if it didn't have circular dependencies
#
# NOTE: This doesn't work in practice because fpm-deps depends on fpm itself,
# and FPM tries to download dependencies during build, which Nix forbids.
# This is kept as an example of how to use the FPM builder for other packages.
{ lib
, callPackage
, fetchFromGitHub
, graphviz
, git
}:

let
  fpmBuilder = callPackage ./default.nix { };
in
fpmBuilder {
  pname = "fpm-deps";
  version = "0.2.1";

  src = fetchFromGitHub {
    owner = "ivan-pi";
    repo = "fpm-deps";
    rev = "2fa1ee77ae78c4b0d4b67d92da4b9a62ad6557ab";
    sha256 = "sha256-pK1W5ZttnuDyaeSvfvbMqhlvNyPdtmJFgjFFMXCkWDk=";
  };

  # Runtime dependencies for graph generation
  buildInputs = [ graphviz git ];

  # Build in release mode
  fpmBuildFlags = [ "--release" ];

  # Post-install: create convenience wrapper
  postInstallPhases = [
    ''
      # Add a help message when running fpm-deps --help
      wrapProgram $out/bin/fpm-deps \
        --prefix PATH : ${lib.makeBinPath [ graphviz git ]}
      
      wrapProgram $out/bin/fpm-tree \
        --prefix PATH : ${lib.makeBinPath [ git ]}
    ''
  ];

  meta = {
    description = "Generate dependency graphs of Fortran packages";
    longDescription = ''
      fpm-deps analyzes fpm.toml files in Fortran Package Manager projects
      and generates dependency graphs in various formats (DOT, Mermaid).
    '';
    homepage = "https://github.com/ivan-pi/fpm-deps";
    license = lib.licenses.mit;
    maintainers = [];
  };
}