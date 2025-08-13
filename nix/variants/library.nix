# Library template variant
{
  name,
  description ? "A Python library",
  ...
}: {
  # Library-specific project configuration
  project = {
    inherit name description;
    
    # Library-specific classifiers
    classifiers = [
      "Development Status :: 3 - Alpha"
      "Intended Audience :: Developers"
      "License :: OSI Approved :: MIT License"
      "Programming Language :: Python :: 3"
      "Programming Language :: Python :: 3.13"
      "Programming Language :: Python :: 3 :: Only"
      "Topic :: Software Development :: Libraries :: Python Modules"
      "Typing :: Typed"
    ];

    # Libraries typically don't have CLI entry points
    scripts = {};

    # Minimal dependencies for a library
    dependencies = [];

    # Library development dependencies
    optional-dependencies.dev = [
      "pytest>=7.0"
      "pytest-cov>=4.0"
      "mypy>=1.0"
      "ruff>=0.1"
      "sphinx>=7.0"
      "sphinx-rtd-theme>=1.3"
      "myst-parser>=2.0"
    ];

    # Optional documentation dependencies
    optional-dependencies.docs = [
      "sphinx>=7.0"
      "sphinx-rtd-theme>=1.3"
      "myst-parser>=2.0"
      "sphinx-autodoc-typehints>=1.24"
    ];
  };

  # Library-specific source structure
  src_structure = {
    "__init__.py" = ''
      """${description}"""
      
      import tomllib
      from importlib.metadata import PackageNotFoundError, metadata, version
      from pathlib import Path
      
      # Try to get package name from pyproject.toml first
      try:
          pyproject_path = Path(__file__).parent.parent.parent / "pyproject.toml"
          if pyproject_path.exists():
              with open(pyproject_path, "rb") as f:
                  pyproject = tomllib.load(f)
                  __package_name__ = pyproject["project"]["name"]
          else:
              __package_name__ = __name__.split(".")[-1]
      except Exception:
          __package_name__ = __name__.split(".")[-1]
      
      # Fetch metadata from installed package
      try:
          __version__ = version(__package_name__)
          _metadata = metadata(__package_name__)
          __author__ = _metadata.get("Author", "Unknown")
          __email__ = _metadata.get("Author-email", "")
      except PackageNotFoundError:
          # Fallback for development/editable installs
          __version__ = "0.0.0"
          __author__ = "Unknown"
          __email__ = ""
      
      # Public API - export main classes/functions here
      __all__ = [
          "__version__",
          "__package_name__",
          "__author__",
          "__email__",
          # Add your public API here
      ]
    '';

    "core.py" = ''
      """Core functionality for ${name}."""
      
      from typing import Any, Optional
      
      
      class ${name.title().replace("-", "").replace("_", "")}:
          """Main class for ${name} library."""
          
          def __init__(self, config: Optional[dict[str, Any]] = None) -> None:
              """Initialize the ${name} instance.
              
              Args:
                  config: Optional configuration dictionary
              """
              self.config = config or {}
          
          def process(self, data: Any) -> Any:
              """Process the given data.
              
              Args:
                  data: Input data to process
                  
              Returns:
                  Processed data
              """
              # Implement your core logic here
              return data
    '';

    "utils.py" = ''
      """Utility functions for ${name}."""
      
      from typing import Any
      
      
      def helper_function(value: Any) -> str:
          """A helper function.
          
          Args:
              value: Input value
              
          Returns:
              String representation of the value
          """
          return str(value)
    '';

    "exceptions.py" = ''
      """Custom exceptions for ${name}."""
      
      
      class ${name.title().replace("-", "").replace("_", "")}Error(Exception):
          """Base exception for ${name}."""
          pass
      
      
      class ${name.title().replace("-", "").replace("_", "")}ConfigError(${name.title().replace("-", "").replace("_", "")}Error):
          """Configuration error for ${name}."""
          pass
    '';
  };

  # Additional library files
  additional_files = {
    "docs/index.md" = ''
      # ${name}
      
      ${description}
      
      ## Installation
      
      ```bash
      pip install ${name}
      ```
      
      ## Usage
      
      ```python
      from ${name} import ${name.title().replace("-", "").replace("_", "")}
      
      # Create an instance
      instance = ${name.title().replace("-", "").replace("_", "")}()
      
      # Process some data
      result = instance.process("your data")
      ```
      
      ## API Reference
      
      See the API documentation for detailed information.
    '';

    "docs/conf.py" = ''
      """Sphinx configuration for ${name} documentation."""
      
      project = "${name}"
      copyright = "2024, Your Name"
      author = "Your Name"
      
      extensions = [
          "sphinx.ext.autodoc",
          "sphinx.ext.viewcode",
          "sphinx.ext.napoleon",
          "myst_parser",
          "sphinx_autodoc_typehints",
      ]
      
      templates_path = ["_templates"]
      exclude_patterns = ["_build", "Thumbs.db", ".DS_Store"]
      
      html_theme = "sphinx_rtd_theme"
      html_static_path = ["_static"]
      
      # Type hints configuration
      autodoc_typehints = "description"
      typehints_fully_qualified = False
    '';
  };
}
