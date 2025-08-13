# Virtual environment creation and shell configuration
{
  lib,
  pkgs,
  python,
  pythonProject,
  pythonSets,
  workspaceData,
  ...
}: let
  inherit (pythonSets) editablePythonSet;
  inherit (workspaceData) workspace;

  # Use all dependencies (simplified since we only have one workspace)
  buildableDeps = workspace.deps.all;

  # Single virtualenv with only buildable packages
  virtualenv =
    editablePythonSet.mkVirtualEnv
    "${
      if pythonProject.emptyRoot
      then "workspace"
      else pythonProject.projectName
    }-dev-env"
    buildableDeps;

  # Shell configuration
  uvShellSet = {
    packages = [virtualenv pkgs.uv];
    env = {
      UV_NO_SYNC = "1";
      UV_PYTHON = python;
      UV_PYTHON_DOWNLOADS = "never";
    };
    shellHook = ''
      # Undo dependency propagation by nixpkgs.
      unset PYTHONPATH

      # Get repository root using git. This is expanded at runtime by the editable `.pth` machinery.
      export REPO_ROOT=$(git rev-parse --show-toplevel)

      # Create symlink to virtualenv at .venv for ty compatibility
      ln -snf ${virtualenv} $REPO_ROOT/.venv
    '';
  };
in {
  inherit
    buildableDeps
    virtualenv
    uvShellSet
    ;
}
