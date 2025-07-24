# fpm-deps tool - generates dependency graphs for Fortran packages
# 
# Note: This package has a circular dependency issue:
# - fpm-deps depends on fpm as a library (not just as a build tool)
# - We can't build it normally because FPM would try to download dependencies
# - Solution: Use runtime execution with wrapper scripts
{
  lib,
  stdenv,
  fetchFromGitHub,
  gfortran,
  fortran-fpm,
  git,
  makeWrapper,
  graphviz,  # For generating visual graphs
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

  nativeBuildInputs = [ makeWrapper ];
  
  buildInputs = [ fortran-fpm git graphviz ];

  # We skip the build phase and use FPM's run command at runtime
  # This avoids the network dependency issue during build
  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall
    
    # Copy the source to the output directory
    mkdir -p $out/share/fpm-deps
    cp -r . $out/share/fpm-deps/
    
    # Create wrapper script for fpm-deps
    mkdir -p $out/bin
    makeWrapper ${fortran-fpm}/bin/fortran-fpm $out/bin/fpm-deps \
      --add-flags "run" \
      --add-flags "--runner" \
      --add-flags "--" \
      --add-flags "--app" \
      --add-flags "fpm-deps" \
      --add-flags "--" \
      --run "cd $out/share/fpm-deps" \
      --prefix PATH : ${lib.makeBinPath [ git graphviz ]}
    
    # Create wrapper script for fpm-tree  
    makeWrapper ${fortran-fpm}/bin/fortran-fpm $out/bin/fpm-tree \
      --add-flags "run" \
      --add-flags "--runner" \
      --add-flags "--" \
      --add-flags "--app" \
      --add-flags "fpm-tree" \
      --add-flags "--" \
      --run "cd $out/share/fpm-deps" \
      --prefix PATH : ${lib.makeBinPath [ git ]}
    
    runHook postInstall
  '';

  meta = with lib; {
    description = "Generate dependency graphs of Fortran packages managed by FPM";
    longDescription = ''
      fpm-deps is a tool for visualizing dependencies in Fortran Package Manager (FPM) projects.
      It can output dependency graphs in Graphviz DOT format or Mermaid diagram format.
      
      This package uses a wrapper approach to avoid circular dependency issues, as fpm-deps
      itself depends on FPM as a library. The tool runs via 'fpm run' at runtime.
    '';
    homepage = "https://github.com/ivan-pi/fpm-deps";
    license = licenses.mit;
    maintainers = [];
    platforms = platforms.all;
  };
}
