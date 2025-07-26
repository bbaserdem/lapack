#!/bin/bash
# Setup script for GitHub MCP server

echo "=== GitHub MCP Server Setup ==="
echo

# Check if .env file exists
if [ ! -f .env ]; then
    echo "Creating .env file from .env.example..."
    cp .env.example .env
    echo "✓ Created .env file"
else
    echo "✓ .env file already exists"
fi

# Check if GITHUB_TOKEN is already set in .env
if grep -q "^GITHUB_TOKEN=" .env; then
    echo "✓ GITHUB_TOKEN already configured in .env"
    echo
    echo "To update your token, edit the .env file and change the GITHUB_TOKEN value."
else
    echo
    echo "❌ GITHUB_TOKEN not found in .env"
    echo
    echo "To configure the GitHub MCP server:"
    echo "1. Generate a GitHub Personal Access Token at:"
    echo "   https://github.com/settings/tokens/new"
    echo
    echo "2. Required scopes:"
    echo "   - repo (Full control of private repositories)"
    echo "   - workflow (Update GitHub Action workflows)"
    echo "   - read:org (Read org and team membership)"
    echo
    echo "3. Add the token to your .env file:"
    echo "   GITHUB_TOKEN=\"ghp_your_token_here\""
    echo
    read -p "Would you like to add your GitHub token now? (y/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        read -p "Enter your GitHub Personal Access Token: " token
        echo >> .env
        echo "# GitHub MCP Server" >> .env
        echo "GITHUB_TOKEN=\"$token\"" >> .env
        echo "✓ GitHub token added to .env"
    fi
fi

echo
echo "=== Next Steps ==="
echo "1. Ensure your token is configured in .env"
echo "2. Restart Claude Desktop to load the GitHub MCP server"
echo "3. Test with: 'Claude, list my GitHub repositories'"
echo
echo "For more information, see: docs/GITHUB_MCP_SETUP.md"