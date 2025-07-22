# shell.nix
{
  pkgs,
  inputs,
  system,
  ...
}: let
  quaestor = inputs.quaestor.packages.${system}.default;
in {
  # Main dev shell
  default = pkgs.mkShell {
    packages = with pkgs; [
      git
      # Grab build tools
      gfortran
      cmake
      # Some ai stuff
      claude-code
      quaestor
    ];
    # Shell hooks
    shellHook = ''
      # Make our local node packages available to our shell; for mcp's
      export PATH="./node_modules/.bin:$PATH"
    '';
  };
}
