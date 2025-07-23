# User Documentation

User documentation for development

## Swarm Container

To isolate the claude swarm from the rest of the computer,
follow this workflow.
The type is the directory name in the `./nix` folder.
Currently available;
- claudeSwarm

### Authentication

Put the .env file with the Claude code authentication token
in the target worktree branch.

(For safety, only do this on the working directory)

### Docker Image

If not already done so, follow the commands to load the swarm to podman.

- From the root of the repo, run `nix build .#docker-<NAME>`
- From the root of the repo, run `docker load < result`

### Deploy Swarm

Just run this command
`nix run .#deploy-<NAME> -- <branch.name> <prompt>`
