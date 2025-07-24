{
  lib,
  stdenv,
  fetchFromGitHub,
  gfortran,
  fortran-fpm,
  git,
  makeWrapper,
}:
stdenv.mkDerivation rec {
  pname = "fpm-deps";
  version = "0.2.1-unstable-2025-01-22";

  src = fetchFromGitHub {
    owner = "ivan-pi";
    repo = "fpm-deps";
    rev = "2fa1ee77ae78c4b0d4b67d92da4b9a62ad6557ab";
    sha256 = "sha256-pK1W5ZttnuDyaeSvfvbMqhlvNyPdtmJFgjFFMXCkWDk=";
  };

  nativeBuildInputs = [gfortran fortran-fpm git makeWrapper];
  
  buildInputs = [fortran-fpm git];

  # Skip the normal fpm build process due to network dependency issues
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    
    # Create a wrapper script that will use fortran-fpm at runtime
    mkdir -p $out/bin
    
    # Copy the source to the output
    mkdir -p $out/share/fpm-deps
    cp -r . $out/share/fpm-deps/
    
    # Create wrapper scripts for fpm-deps and fpm-tree
    makeWrapper ${fortran-fpm}/bin/fortran-fpm $out/bin/fpm-deps \
      --add-flags "run --runner" \
      --add-flags "--" \
      --add-flags "--app fpm-deps" \
      --add-flags "--" \
      --run "cd $out/share/fpm-deps"
    
    makeWrapper ${fortran-fpm}/bin/fortran-fpm $out/bin/fpm-tree \
      --add-flags "run --runner" \
      --add-flags "--" \
      --add-flags "--app fpm-tree" \
      --add-flags "--" \
      --run "cd $out/share/fpm-deps"
    
    runHook postInstall
  '';

  meta = with lib; {
    description = "Generate dependency graphs of Fortran packages";
    homepage = "https://github.com/ivan-pi/fpm-deps";
    license = licenses.mit;
    maintainers = [];
    platforms = platforms.all;
  };
}
