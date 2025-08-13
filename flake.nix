{
  description = "Custom fork of LAPACK using lapack-reference derivation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
    quaestor = {
      url = "github:jeanluciano/quaestor";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Python project setup
    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    uv2nix = {
      url = "github:pyproject-nix/uv2nix";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.uv2nix.follows = "uv2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    uv2nix,
    pyproject-nix,
    pyproject-build-systems,
    ...
  } @ inputs: let
    outputs = self;
    pythonProject = import ./nix/python.nix;
  in
    flake-utils.lib.eachDefaultSystem (system: let
      inherit (nixpkgs) lib;

      # Load workspace directly from root
      workspace = uv2nix.lib.workspace.loadWorkspace {workspaceRoot = ./.;};

      # Grab UV stuff
      pkgs = import nixpkgs {inherit system;};
      uvBoilerplate = import nix/uv2nix {
        inherit inputs system pythonProject pkgs workspace;
      };

      # For Docker images on Darwin, we need Linux packages
      # Create a separate output for x86_64-linux docker images
      linuxSystem = "x86_64-linux";
      linuxPkgs = import nixpkgs {system = linuxSystem;};
      linuxWorkspace = uv2nix.lib.workspace.loadWorkspace {workspaceRoot = ./.;};
      linuxUvBoilerplate = import nix/uv2nix {
        inherit inputs pythonProject workspace;
        system = linuxSystem;
        pkgs = linuxPkgs;
      };
    in {
      checks = import ./nix/checks.nix {
        inherit pkgs uvBoilerplate pythonProject;
      };
      apps = import ./nix/apps {
        inherit outputs pkgs inputs system uvBoilerplate pythonProject;
      };
      packages = import ./nix/packages {
        inherit pkgs inputs system uvBoilerplate pythonProject;
        inherit linuxPkgs linuxUvBoilerplate linuxSystem;
      };
      devShells = import ./nix/shells {
        inherit pkgs inputs system uvBoilerplate;
      };
    });
}
