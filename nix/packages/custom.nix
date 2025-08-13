# Custom packages for the project
{
  pkgs,
  lib,
  stdenv,
  ...
}: let
  # Helper function to create simple shell script packages
  mkShellScript = name: script: pkgs.writeShellScriptBin name script;

  # Helper function to create custom derivations
  mkCustomPackage = name: attrs: stdenv.mkDerivation (attrs // {
    pname = name;
    name = "${name}-${attrs.version or "1.0.0"}";
  });

in {
  # Example custom shell scripts
  project-init = mkShellScript "project-init" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    echo "🚀 Initializing project environment..."
    
    # Setup git hooks if not already done
    if [ ! -d ".git/hooks" ]; then
      echo "No .git directory found"
      exit 1
    fi
    
    # Install pre-commit hooks (example)
    echo "Setting up development environment..."
    echo "✅ Project initialized successfully!"
  '';

  # Example utility for project maintenance
  project-clean = mkShellScript "project-clean" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    echo "🧹 Cleaning project artifacts..."
    
    # Clean Python cache
    find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
    find . -name "*.pyc" -delete 2>/dev/null || true
    
    # Clean build artifacts
    rm -rf build/ dist/ *.egg-info/ .coverage htmlcov/ .pytest_cache/ 2>/dev/null || true
    
    # Clean Nix build artifacts
    rm -f result result-* 2>/dev/null || true
    
    echo "✅ Project cleaned successfully!"
  '';

  # Example development utility
  dev-status = mkShellScript "dev-status" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    echo "📊 Development Environment Status"
    echo "================================="
    
    # Python version
    echo "🐍 Python: $(python --version 2>&1)"
    
    # UV version
    echo "📦 UV: $(uv --version 2>&1)"
    
    # Git status
    if [ -d ".git" ]; then
      echo "📝 Git branch: $(git branch --show-current 2>/dev/null || echo 'unknown')"
      echo "📝 Git status: $(git status --porcelain | wc -l) files changed"
    fi
    
    # Project dependencies
    if [ -f "pyproject.toml" ]; then
      echo "📋 Project: $(grep '^name = ' pyproject.toml | cut -d'"' -f2)"
      echo "📋 Version: $(grep '^version = ' pyproject.toml | cut -d'"' -f2)"
    fi
    
    # Virtual environment
    if [ -n "''${VIRTUAL_ENV:-}" ]; then
      echo "🌍 Virtual env: $(basename "$VIRTUAL_ENV")"
    else
      echo "🌍 Virtual env: none"
    fi
  '';

  # Example: Custom package for project documentation
  # project-docs = mkCustomPackage "project-docs" {
  #   version = "1.0.0";
  #   src = ./.;
  #   buildInputs = with pkgs; [ sphinx ];
  #   buildPhase = ''
  #     sphinx-build docs $out/share/doc
  #   '';
  # };
}
