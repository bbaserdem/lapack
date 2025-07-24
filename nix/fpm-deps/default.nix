# Generic builder for Fortran packages using FPM (Fortran Package Manager)
{ lib
, stdenv
, fetchFromGitHub
, gfortran
, fortran-fpm
, makeWrapper
}:

{ pname
, version
, src
, fpmBuildFlags ? [ "--release" ]
, fpmInstallFlags ? [ ]
, nativeBuildInputs ? [ ]
, buildInputs ? [ ]
, preConfigurePhases ? [ ]
, preBuildPhases ? [ ]
, postInstallPhases ? [ ]
, meta ? { }
, ...
}@args:

let
  # Remove the custom arguments from args to pass the rest to mkDerivation
  cleanedArgs = builtins.removeAttrs args [
    "fpmBuildFlags"
    "fpmInstallFlags"
    "preConfigurePhases"
    "preBuildPhases"
    "postInstallPhases"
  ];
in
stdenv.mkDerivation (cleanedArgs // {
  inherit pname version src;

  nativeBuildInputs = [
    gfortran
    fortran-fpm
    makeWrapper
  ] ++ nativeBuildInputs;

  inherit buildInputs;

  # FPM needs these environment variables
  FC = "${gfortran}/bin/gfortran";
  FFLAGS = "-O3 -fPIC";

  # Configure phase - prepare the build environment
  configurePhase = ''
    runHook preConfigure
    
    # Set up FPM environment
    export FPM_FC=$FC
    export FPM_FFLAGS="$FFLAGS"
    
    # Check if fpm.toml exists
    if [ ! -f fpm.toml ]; then
      echo "Error: fpm.toml not found in source directory"
      exit 1
    fi
    
    ${lib.concatStringsSep "\n" preConfigurePhases}
    
    runHook postConfigure
  '';

  # Build phase - compile the package
  buildPhase = ''
    runHook preBuild
    
    ${lib.concatStringsSep "\n" preBuildPhases}
    
    echo "Building with FPM..."
    fortran-fpm build ${lib.concatStringsSep " " fpmBuildFlags}
    
    runHook postBuild
  '';

  # Install phase - install the built artifacts
  installPhase = ''
    runHook preInstall
    
    # FPM install command
    echo "Installing with FPM..."
    fortran-fpm install --prefix=$out ${lib.concatStringsSep " " fpmInstallFlags}
    
    # Ensure proper library paths if libraries were built
    if [ -d "$out/lib" ]; then
      # Add rpath to executables if they exist
      if [ -d "$out/bin" ]; then
        for exe in $out/bin/*; do
          if [ -f "$exe" ] && [ -x "$exe" ]; then
            patchelf --set-rpath "$out/lib:${lib.makeLibraryPath buildInputs}" "$exe" || true
          fi
        done
      fi
    fi
    
    ${lib.concatStringsSep "\n" postInstallPhases}
    
    runHook postInstall
  '';

  # Default meta attributes
  meta = {
    platforms = lib.platforms.all;
    description = "Fortran package built with FPM";
  } // meta;
})