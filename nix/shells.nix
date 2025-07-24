# shell.nix
{
  pkgs,
  inputs,
  system,
  ...
}: let
  quaestor = inputs.quaestor.packages.${system}.default;
in rec {
  # Main dev shell
  default = pkgs.mkShell {
    packages = with pkgs; [
      git
      # Grab build tools
      gfortran
      cmake
      # Needed for mcp's
      nodejs
      typescript
      # Some ai stuff
      claude-code
      quaestor
      # Probe docker if needed
      podman
    ];
    # Shell hooks
    shellHook = ''
      # Make our local node packages available to our shell; for mcp's
      export PATH="./node_modules/.bin:$PATH"
    '';
  };

  # Organize dev shell; generate code reports as well
  organize = default.overrideAttrs (old: {
    packages =
      old.packages
      ++ (with pkgs; [
        haskellPackages.fortran-src
        uv # For neo4j and fortdepend
        fortran-fpm # For fpm-modules
      ]);
  });
}
