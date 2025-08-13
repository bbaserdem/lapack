# Template variant generator
{
  pkgs,
  lib,
  ...
}: let
  # Available template variants
  variants = {
    cli = import ./cli.nix;
    library = import ./library.nix;
    web-service = import ./web-service.nix;
  };

  # Generate a project structure from a variant
  generateProject = variant: name: description: let
    config = variant { inherit name description; };
    sanitizeName = name: builtins.replaceStrings ["-"] ["_"] name;
    packageName = sanitizeName name;
  in {
    # Generate pyproject.toml content
    pyproject = {
      project = config.project // {
        name = name;
        description = description;
      };
      
      # Standard build system
      build-system = {
        requires = ["hatchling" "editables"];
        build-backend = "hatchling.build";
      };

      # Standard tool configurations
      tool = {
        ruff = {
          target-version = "py313";
          line-length = 100;
          extend-include = ["*.pyi"];
          
          lint = {
            select = [
              "E"    # pycodestyle errors
              "F"    # pyflakes
              "I"    # isort
              "N"    # pep8-naming
              "UP"   # pyupgrade
              "B"    # flake8-bugbear
              "C4"   # flake8-comprehensions
              "SIM"  # flake8-simplify
              "RUF"  # ruff-specific rules
              "PL"   # pylint
              "PTH"  # flake8-use-pathlib
            ];
            ignore = ["PLR0913"]; # Too many arguments
          };
        };

        mypy = {
          python_version = "3.13";
          warn_return_any = true;
          warn_unused_configs = true;
          disallow_untyped_defs = true;
          check_untyped_defs = true;
          no_implicit_optional = true;
          strict_equality = true;
          warn_redundant_casts = true;
          warn_unused_ignores = true;
        };

        pytest.ini_options = {
          testpaths = ["tests"];
          python_files = ["test_*.py" "*_test.py"];
          addopts = [
            "-ra"
            "--strict-markers"
            "--cov=${packageName}"
            "--cov-report=term-missing"
            "--cov-report=html"
          ];
        };

        coverage = {
          run = {
            source = ["src/${packageName}"];
            omit = ["*/__main__.py"];
          };
          report = {
            exclude_lines = [
              "pragma: no cover"
              "if __name__ == .__main__.:"
              "if TYPE_CHECKING:"
              "raise NotImplementedError"
            ];
          };
        };

        hatch.build.targets.wheel = {
          packages = ["src/${packageName}"];
        };
      };
    };

    # Generate source files
    src_files = config.src_structure;
    
    # Generate additional files
    additional_files = config.additional_files or {};
    
    # Metadata
    metadata = {
      inherit name description;
      variant = variant;
      package_name = packageName;
    };
  };

  # Generate a template instantiation script
  mkTemplateScript = variant_name: pkgs.writeShellScriptBin "generate-${variant_name}" ''
    set -euo pipefail
    
    if [ $# -ne 2 ]; then
      echo "Usage: $0 <project-name> <description>"
      exit 1
    fi
    
    PROJECT_NAME="$1"
    DESCRIPTION="$2"
    PACKAGE_NAME=$(echo "$PROJECT_NAME" | tr '-' '_')
    
    echo "Generating ${variant_name} project: $PROJECT_NAME"
    echo "Description: $DESCRIPTION"
    echo "Package name: $PACKAGE_NAME"
    
    # Create directory structure
    mkdir -p "src/$PACKAGE_NAME"
    mkdir -p tests
    
    echo "Generated ${variant_name} template for $PROJECT_NAME"
    echo "Next steps:"
    echo "  1. cd into your project directory"
    echo "  2. Run 'uv sync' to install dependencies"
    echo "  3. Start developing!"
  '';

in {
  inherit variants generateProject;
  
  # Template generation scripts
  scripts = lib.mapAttrs (name: _: mkTemplateScript name) variants;
  
  # Helper function to list available variants
  listVariants = lib.attrNames variants;
}
