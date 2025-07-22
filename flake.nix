{
  description = "Custom fork of LAPACK using lapack-reference derivation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
    quaestor = {
      url = "github:jeanluciano/quaestor";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
    in {
      packages = import ./nix/packages.nix {inherit pkgs;};
      devShells = import ./nix/shells.nix {inherit pkgs inputs system;};
    });
}
