# Example FPM package derivation
{ lib
, callPackage
, fetchFromGitHub
}:

let
  fpmBuilder = callPackage ./default.nix { };
in
fpmBuilder {
  pname = "fortran-stdlib";
  version = "0.2.1";

  src = fetchFromGitHub {
    owner = "fortran-lang";
    repo = "stdlib";
    rev = "v0.2.1";
    sha256 = "sha256-PLACEHOLDER"; # Replace with actual hash after first build
  };

  # Optional: Custom build flags
  fpmBuildFlags = [ "--release" "--flag" "-O3" ];

  # Optional: Runtime dependencies
  buildInputs = [ ];

  # Optional: Pre-build setup
  preBuildPhases = [
    ''
      echo "Setting up fortran-stdlib build..."
    ''
  ];

  meta = {
    description = "Fortran Standard Library";
    homepage = "https://github.com/fortran-lang/stdlib";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}