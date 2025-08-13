# Overlay creation and management
{
  inputs,
  pkgs,
  lib,
  stdenv,
  pythonProject,
  workspaceData,
  ...
}: let
  inherit (inputs) pyproject-build-systems;
  inherit (workspaceData) workspace;

  # Create package overlay from root workspace (includes all packages)
  baseOverlay = workspace.mkPyprojectOverlay {
    # Prefer prebuilt binary wheels as a package source
    sourcePreference = "wheel";
  };

  # Use the base overlay directly
  overlay = baseOverlay;

  # Import override modules
  buildSystemOverrides = import ./build-systems.nix {inherit pkgs lib;};

  # Extend generated overlay with build fixups for all packages
  pyprojectOverrides = final: prev:
    # Combine build system overrides
    (buildSystemOverrides.mkBuildSystemOverrides final prev);

  # Create editable overlay for the main project and all workspace packages
  editableOverlay = workspace.mkEditablePyprojectOverlay {
    root = "$REPO_ROOT";
    # Include the main project (if not empty root) and all workspace packages
    members = 
      (if pythonProject.emptyRoot then [] else [ pythonProject.projectName ])
      ++ (map (ws: ws.projectName) pythonProject.workspaces);
  };

in {
  inherit 
    baseOverlay
    overlay
    pyprojectOverrides
    editableOverlay;
}
