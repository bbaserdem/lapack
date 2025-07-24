# Test packaging a local FPM project
{ callPackage }:

let
  fpmBuilder = callPackage ./default.nix { };
in
fpmBuilder {
  pname = "local-fpm-test";
  version = "0.1.0";
  
  # For testing, you would point this to your local FPM project directory
  # src = /path/to/your/fpm/project;
  src = ./test-project; # This would be a local test project
  
  meta = {
    description = "Local FPM package test";
  };
}