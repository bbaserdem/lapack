# GitHub MCP Server Setup Guide

This guide explains how to set up and use the GitHub MCP server with the LAPACK project.

## Overview

The GitHub MCP (Model Context Protocol) server allows Claude to interact with GitHub repositories, issues, pull requests, and more. This integration enables:

- Creating and managing issues
- Working with pull requests
- Accessing repository information
- Managing releases and workflows
- Analyzing code and repository statistics

## Setup Instructions

### 1. Generate a GitHub Personal Access Token

1. Go to GitHub → Settings → Developer settings → Personal access tokens → Tokens (classic)
2. Click "Generate new token" → "Generate new token (classic)"
3. Give your token a descriptive name (e.g., "Claude MCP Access")
4. Set expiration as needed
5. Select the following scopes:
   - `repo` (Full control of private repositories)
   - `workflow` (Update GitHub Action workflows)
   - `read:org` (Read org and team membership)
   - `gist` (Create gists)
6. Click "Generate token"
7. Copy the token immediately (you won't be able to see it again)

### 2. Configure the Token

Add your GitHub token to the `.mcp.json` file:

```json
"github": {
    "type": "stdio",
    "command": "npx",
    "args": [
        "@modelcontextprotocol/server-github"
    ],
    "env": {
        "GITHUB_PERSONAL_ACCESS_TOKEN": "ghp_your_token_here"
    }
}
```

**Security Note**: Never commit your token to version control. Consider using environment variables:

```json
"env": {
    "GITHUB_PERSONAL_ACCESS_TOKEN": "${GITHUB_TOKEN}"
}
```

Then set the environment variable in your shell:
```bash
export GITHUB_TOKEN="ghp_your_token_here"
```

### 3. Restart Claude Desktop

After updating `.mcp.json`, restart Claude Desktop to load the new MCP server.

## Available GitHub MCP Tools

Once configured, you'll have access to these tools:

### Repository Operations
- `create_or_update_file` - Create or update files in a repository
- `search_repositories` - Search for repositories
- `get_file_contents` - Read file contents from a repository
- `push_files` - Push multiple files to a repository

### Issues and Pull Requests
- `create_issue` - Create a new issue
- `create_pull_request` - Create a new pull request
- `list_issues` - List issues with filtering options
- `list_pull_requests` - List pull requests
- `update_issue` - Update an existing issue
- `add_issue_comment` - Add a comment to an issue

### Branches and Commits
- `create_branch` - Create a new branch
- `get_branches` - List repository branches
- `get_commits` - Get commit history

### Workflows and Actions
- `list_workflow_runs` - List GitHub Actions workflow runs
- `get_workflow_run_logs` - Get logs from workflow runs

### Other Operations
- `fork_repository` - Fork a repository
- `create_repository` - Create a new repository

## Usage Examples

### Example 1: Create an Issue
```
Claude, please create a GitHub issue in this repository about adding error tracking to the Neo4j schema.
```

### Example 2: Search for Related Issues
```
Claude, search for existing issues related to "parser errors" or "fortran parsing" in this repository.
```

### Example 3: Create a Pull Request
```
Claude, create a pull request for the current branch with the title "Add error tracking to Neo4j schema".
```

### Example 4: Analyze Repository
```
Claude, analyze the recent commits and tell me what areas of the codebase are most active.
```

## Best Practices

1. **Token Security**
   - Never commit tokens to version control
   - Use environment variables for sensitive data
   - Rotate tokens regularly
   - Use minimal required permissions

2. **Repository Access**
   - The token owner must have appropriate access to target repositories
   - For organization repos, ensure you have the necessary permissions

3. **Rate Limits**
   - GitHub API has rate limits (5000 requests/hour for authenticated requests)
   - The MCP server handles rate limiting automatically

4. **Error Handling**
   - If operations fail, check token permissions
   - Verify repository access rights
   - Check GitHub API status

## Troubleshooting

### Common Issues

1. **"Authentication failed"**
   - Verify your token is correct
   - Check token hasn't expired
   - Ensure token has required scopes

2. **"Repository not found"**
   - Verify repository name and owner
   - Check token has access to private repos if needed

3. **"MCP server not available"**
   - Restart Claude Desktop
   - Check `.mcp.json` syntax
   - Verify npm/npx is installed

### Debug Mode

To enable debug logging, add to your `.mcp.json`:

```json
"github": {
    "type": "stdio",
    "command": "npx",
    "args": [
        "@modelcontextprotocol/server-github",
        "--debug"
    ],
    "env": {
        "GITHUB_PERSONAL_ACCESS_TOKEN": "${GITHUB_TOKEN}"
    }
}
```

## Integration with LAPACK Project

The GitHub MCP server can be particularly useful for:

1. **Issue Tracking**
   - Track parsing errors as GitHub issues
   - Link Neo4j error nodes to GitHub issues
   - Generate error reports as issues

2. **Documentation**
   - Update README files automatically
   - Generate API documentation
   - Create release notes

3. **Collaboration**
   - Create PRs for schema updates
   - Review and comment on changes
   - Track project milestones

4. **Automation**
   - Trigger workflows for database updates
   - Generate reports from Neo4j queries
   - Automate release processes

## Security Considerations

1. **Token Scope**: Only grant necessary permissions
2. **Environment**: Use `.env` files for local development
3. **CI/CD**: Use GitHub Secrets for automated workflows
4. **Audit**: Regularly review token usage in GitHub settings

## Next Steps

1. Configure your GitHub token
2. Test basic operations (list issues, read files)
3. Integrate with existing LAPACK workflows
4. Consider automation opportunities

For more information, see:
- [GitHub MCP Server Documentation](https://github.com/modelcontextprotocol/servers/tree/main/src/github)
- [GitHub API Documentation](https://docs.github.com/en/rest)
- [Personal Access Tokens Guide](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token)