# Custom runnable scripts and utilities
{
  pkgs,
  outputs,
  ...
}: let
  inherit (pkgs) lib;

  # Helper to create app from package
  mkApp = program: {
    type = "app";
    inherit program;
  };

  # Helper to create app from package with specific binary
  mkAppFromPackage = pkg: bin: {
    type = "app";
    program = "${pkg}/bin/${bin}";
  };

in {
  # Project initialization script
  init = mkAppFromPackage outputs.packages.${pkgs.system}.project-init "project-init";

  # Project cleanup script
  clean = mkAppFromPackage outputs.packages.${pkgs.system}.project-clean "project-clean";

  # Development status script
  status = mkAppFromPackage outputs.packages.${pkgs.system}.dev-status "dev-status";

  # Template generation apps (when implemented)
  # generate-cli = mkApp "${outputs.packages.${pkgs.system}.template-generator}/bin/generate-cli";
  # generate-library = mkApp "${outputs.packages.${pkgs.system}.template-generator}/bin/generate-library";
  # generate-web-service = mkApp "${outputs.packages.${pkgs.system}.template-generator}/bin/generate-web-service";

  # Development workflow scripts
  test-all = {
    type = "app";
    program = toString (pkgs.writeShellScript "test-all" ''
      set -euo pipefail
      echo "🧪 Running all tests..."
      
      # Run flake checks
      echo "Running flake checks..."
      nix flake check
      
      # Run pytest if available
      if command -v pytest >/dev/null 2>&1; then
        echo "Running pytest..."
        pytest --cov
      fi
      
      echo "✅ All tests completed!"
    '');
  };

  lint-all = {
    type = "app";
    program = toString (pkgs.writeShellScript "lint-all" ''
      set -euo pipefail
      echo "🔍 Running all linters..."
      
      # Run ruff if available
      if command -v ruff >/dev/null 2>&1; then
        echo "Running ruff..."
        ruff check src/ tests/ || true
        ruff format --check src/ tests/ || true
      fi
      
      # Run mypy if available
      if command -v mypy >/dev/null 2>&1; then
        echo "Running mypy..."
        mypy src/ || true
      fi
      
      echo "✅ Linting completed!"
    '');
  };

  format-all = {
    type = "app";
    program = toString (pkgs.writeShellScript "format-all" ''
      set -euo pipefail
      echo "🎨 Formatting all code..."
      
      # Run ruff formatter if available
      if command -v ruff >/dev/null 2>&1; then
        echo "Running ruff format..."
        ruff format src/ tests/
      fi
      
      # Run black if available (fallback)
      if command -v black >/dev/null 2>&1 && ! command -v ruff >/dev/null 2>&1; then
        echo "Running black..."
        black src/ tests/
      fi
      
      echo "✅ Code formatting completed!"
    '');
  };

  # Development server (useful for web services)
  dev-server = {
    type = "app";
    program = toString (pkgs.writeShellScript "dev-server" ''
      set -euo pipefail
      echo "🚀 Starting development server..."
      
      # Check if this is a web service project
      if grep -q "fastapi\|flask\|django" pyproject.toml 2>/dev/null; then
        # Try to run with uvicorn for FastAPI
        if command -v uvicorn >/dev/null 2>&1; then
          echo "Starting with uvicorn..."
          uvicorn main:app --reload --host 0.0.0.0 --port 8000
        else
          echo "No uvicorn found, trying python -m"
          python -m src.main
        fi
      else
        echo "This doesn't appear to be a web service project"
        echo "Try running the main application instead"
        exit 1
      fi
    '');
  };
}
