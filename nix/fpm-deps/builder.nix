# Helper function to build FPM packages
{ lib, callPackage }:

let
  fpmBuilder = callPackage ./default.nix { };
in
{
  # Function to build an FPM package from GitHub
  buildFpmPackage = args: fpmBuilder args;
  
  # Function to build an FPM package from local source
  buildLocalFpmPackage = { src, ... }@args: fpmBuilder (args // {
    src = if builtins.isPath src then src else throw "src must be a path for local packages";
  });
  
  # Function to build an FPM package with custom dependencies
  buildFpmPackageWithDeps = { dependencies ? [ ], ... }@args: 
    fpmBuilder (args // {
      buildInputs = (args.buildInputs or [ ]) ++ dependencies;
      # Add dependency paths to FPM search paths
      preBuildPhases = (args.preBuildPhases or [ ]) ++ [
        ''
          # Set up dependency paths for FPM
          export FPM_LDFLAGS="-L${lib.makeLibraryPath dependencies}"
          export FPM_CFLAGS="-I${lib.makeIncludePath dependencies}"
        ''
      ];
    });
}