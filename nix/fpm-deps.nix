{
  lib,
  stdenv,
  fetchFromGitHub,
  gfortran,
  fortran-fpm,
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

  nativeBuildInputs = [gfortran fortran-fpm];

  buildPhase = ''
    runHook preBuild
    fortran-fpm build --profile release
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp -r build/*/app/* $out/bin/
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

