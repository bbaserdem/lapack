# UV boilerplate abstraction - modular version
# We want to take flake inputs, and the current system
{
  inputs,
  system,
  pythonProject,
  pkgs,
  workspace,
  ...
}: let
  # Explicitly name our inputs that we'll use
  inherit (inputs) nixpkgs uv2nix pyproject-nix pyproject-build-systems;

  # Pull lib into scope
  inherit (nixpkgs) lib;

  # Create pkgs set from our current system
  inherit (pkgs) stdenv;

  # We use python 3.13
  python = pkgs.python313;

  # Load workspace data
  workspaceData = import ./workspace.nix {
    inherit inputs pythonProject lib workspace;
  };

  # Load overlay data
  overlayData = import ./overlays.nix {
    inherit inputs pkgs lib stdenv pythonProject workspaceData;
  };

  # Load python sets
  pythonSets = import ./python-sets.nix {
    inherit inputs pkgs lib stdenv pythonProject python overlayData workspaceData;
  };

  # Load virtualenv configuration
  virtualenvData = import ./virtualenv.nix {
    inherit lib pkgs python pythonProject pythonSets workspaceData;
  };

in {
  # Re-export everything that was in the original uv.nix
  # Basic configuration
  inherit (inputs) nixpkgs uv2nix pyproject-nix pyproject-build-systems;
  inherit lib stdenv python;
  
  # Workspace data
  inherit (workspaceData) 
    sanitizeName
    workspace;

  # Python sets
  inherit (pythonSets)
    baseSet
    pythonSet
    editablePythonSet;

  # Virtual environment
  inherit (virtualenvData)
    buildableDeps
    virtualenv
    uvShellSet;

  # Overlay data (for compatibility if needed)
  inherit (overlayData)
    overlay
    editableOverlay;
}
