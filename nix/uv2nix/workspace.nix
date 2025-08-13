# Workspace loading and management
{
  inputs,
  pythonProject,
  lib,
  workspace,
  ...
}: let
  inherit (inputs) uv2nix;

  # Helper function to sanitize project names for directory names
  sanitizeName = name: builtins.replaceStrings ["-"] ["_"] name;

in {
  inherit 
    sanitizeName
    workspace;
}
