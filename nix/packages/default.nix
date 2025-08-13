# Package definitions - modular package management
{
  pkgs,
  inputs,
  system,
  uvBoilerplate,
  pythonProject,
  # Linux-specific inputs for Docker images
  linuxPkgs ? null,
  linuxUvBoilerplate ? null,
  linuxSystem ? null,
  ...
}: let
  inherit (pkgs) lib stdenv callPackage;

  lapackDerivation = import ./lapack.nix {inherit pkgs;};

  # Import package modules
  pythonPackages = import ./python.nix {
    inherit pkgs uvBoilerplate pythonProject;
  };

  customPackages = import ./custom.nix {
    inherit pkgs lib stdenv;
  };

  # For Docker images, use the Linux-specific packages if available
  dockerPkgs =
    if linuxPkgs != null
    then linuxPkgs
    else pkgs;
  dockerUvBoilerplate =
    if linuxUvBoilerplate != null
    then linuxUvBoilerplate
    else uvBoilerplate;

  # Import Python packages using the appropriate package set
  dockerPythonPackages =
    if linuxPkgs != null && linuxUvBoilerplate != null
    then
      import ./python.nix {
        pkgs = dockerPkgs;
        uvBoilerplate = dockerUvBoilerplate;
        inherit pythonProject;
      }
    else pythonPackages;

  dockerImages = import ../docker {
    pkgs = dockerPkgs;
    pythonPackages = dockerPythonPackages;
    inherit inputs;
  };
in
  {
    # Default package (if needed)
    default = callPackage lapackDerivation {};
    lapack-reference = lapackDerivation;
    # Our claude flow
    docker-claudeFlow = import ./../claudeFlow/docker.nix {inherit pkgs;};
  }
  // pythonPackages # Python workspace packages
  // customPackages
  // dockerImages
# Custom utility packages

