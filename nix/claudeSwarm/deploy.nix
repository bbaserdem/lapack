# Basic deploy-swarm script
{pkgs, ...}: let
  docker = "${pkgs.podman}/bin/podman";
  git = "${pkgs.git}/bin/git";
in
  pkgs.writeShellScript "deploy-claudeSwarm.sh" ''
    set -euo pipefail

    # Usage: deploy-swarm <worktree-branch-name> "<long-string>"
    if [[ $# -ne 2 ]]; then
      echo "Usage: $0 <worktree-branch-name> \"<long-string>\""
      exit 1
    fi

    TARGET_BRANCH="$1"
    LONGSTRING="$2"

    # Find repo base
    REPO_BASE=$(${git} rev-parse --show-toplevel 2>/dev/null || true)
    if [[ -z "$REPO_BASE" ]]; then
      echo "Error: not inside a git repository."
      exit 2
    fi

    # Use git worktree list --porcelain for reliable, scriptable output
    TARGET_WORKTREE_DIR=""
    current_path=""
    current_branch=""

    WORKTREES_DIR="$REPO_BASE/worktrees"
    TARGET_WORKTREE="$WORKTREES_DIR/$TARGET_BRANCH"

    while IFS= read -r line; do
      case "$line" in
        "worktree "*)
          current_path="''${line#"worktree "}"
          current_branch="" # reset for each new worktree
          ;;
        "branch "*)
          # Output in format "branch refs/heads/branchname"
          ref="''${line#branch }"
          branch="''${ref#refs/heads/}"
          current_branch="$branch"
          ;;
        "")
          # End of worktree entry, check if branch matches
          if [[ "$current_branch" == "$TARGET_BRANCH" ]]; then
            TARGET_WORKTREE_DIR="$current_path"
          fi
          ;;
      esac
    done < <(${git} worktree list --porcelain; echo)

    if [[ -z "$TARGET_WORKTREE_DIR" ]]; then
      echo "Error: No worktree found for branch '$TARGET_BRANCH'."
      echo "Currently active worktrees:"
      ${git} worktree list
      exit 3
    fi

    if [[ ! -f "$TARGET_WORKTREE_DIR/.env" ]]; then
      echo "Warning: .env does not exist in $TARGET_WORKTREE_DIR"
    fi

    # This command depends on the script at docker.nix
    ${docker} run --rm \
      --mount type=bind,source="$TARGET_WORKTREE_DIR",target=/workspace \
      claude-flow-container-claudeSwarm:latest \
      "$LONGSTRING"
  ''
