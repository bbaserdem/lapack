# Python package set construction
{
  inputs,
  pkgs,
  lib,
  stdenv,
  pythonProject,
  python,
  overlayData,
  workspaceData,
  ...
}: let
  inherit (inputs) pyproject-nix pyproject-build-systems;
  inherit (overlayData) overlay pyprojectOverrides editableOverlay;

  # Import workspace overrides for test support
  workspaceOverrides = import ./workspace-overrides.nix {
    inherit lib stdenv pythonProject;
  };

  # Base python package set
  baseSet = pkgs.callPackage pyproject-nix.build.packages {inherit python;};

  # Construct package set with all overlays
  pythonSet = baseSet.overrideScope (
    lib.composeManyExtensions [
      pyproject-build-systems.overlays.default
      overlay
      pyprojectOverrides
    ]
  );

  # Editable python set with fixups for the main package
  editablePythonSet = pythonSet.overrideScope (
    lib.composeManyExtensions [
      editableOverlay
    ]
  );

in {
  inherit
    baseSet
    pythonSet
    editablePythonSet;
}
