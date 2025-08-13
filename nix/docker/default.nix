# Custom docker containers for the project
{
  inputs,
  pkgs,
  pythonPackages,
  ...
}: {
  backendContainer = import ./backend.nix {inherit pkgs pythonPackages;};
}
