# shell.nix
{
  pkgs,
  inputs,
  system,
  uvBoilerplate,
  projectName,
  ...
}: let
  # UV stuff
  inherit (uvBoilerplate) uvShellSet;
  # Input packages
  quaestor = inputs.quaestor.packages.${system}.default;

  # Default shell
  defaultPackages = with pkgs; [
    git
    # Grab build tools
    gfortran
    cmake
    # Needed for mcp's and claude
    nodejs
    typescript
    quaestor
  ];
  defaultHooks = ''
    # Make our local node packages available to our shell; for mcp's
    export PATH="./node_modules/.bin:$PATH"
  '';
  defaultEnv = {};

  # Devops branch
  devopsPackages = with pkgs; [
    podman
  ];
  devopsHooks = "";
  devopsEnv = {};

  # Organize branch
  organizePackages = with pkgs; [
    haskellPackages.fortran-src
    uv # For fortdepend
    neo4j
  ];
  organizeHooks = ''
  '';
  organizeEnv = {};
in {
  # Main dev shell
  default = pkgs.mkShell {
    packages = defaultPackages;
    # Shell hooks
    shellHook = defaultHooks;
  };

  # Devops dev shell, we do docker stuff here
  devops = pkgs.mkShell {
    packages = defaultPackages ++ devopsPackages;
    # Shell hooks
    shellHook = defaultHooks + "\n" + devopsHooks;
  };

  # Organize dev shell; generate code reports as well
  organize = pkgs.mkShell {
    packages = defaultPackages ++ organizePackages ++ uvShellSet.packages;
    env = defaultEnv // organizeEnv // uvShellSet.env;
    shellHook = defaultHooks + "\n" organizeHooks + "\n" + uvShellSet.shellHook;
  };
}
