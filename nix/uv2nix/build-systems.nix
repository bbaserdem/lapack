# Build system overrides for problematic packages
{
  pkgs,
  lib,
  ...
}: let
  # Build system dependencies for problematic packages
  buildSystemOverrides = {
    pygraphviz = {
      setuptools = [];
      wheel = [];
    };
    numpy = {
      meson-python = [];
      cython = [];
    };
    numba = {
      setuptools = [];
      wheel = [];
    };
  };
in {
  inherit buildSystemOverrides;

  # Create build system override packages
  mkBuildSystemOverrides = final: prev:
    lib.mapAttrs (
      name: spec:
        if prev ? ${name}
        then
          prev.${name}.overrideAttrs (old: {
            nativeBuildInputs =
              old.nativeBuildInputs
              ++ final.resolveBuildSystem spec
              ++ lib.optionals (name == "pygraphviz") [
                pkgs.graphviz
                pkgs.pkg-config
              ]
              ++ lib.optionals (name == "numba") [
                pkgs.tbb_2021_11
              ]
              ++ lib.optionals (name == "numpy") [
                pkgs.ninja
                pkgs.meson
                pkgs.pkg-config
              ];
          })
        else null
    )
    buildSystemOverrides;
}
