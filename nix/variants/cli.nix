# CLI application template variant
{
  name,
  description ? "A command-line application",
  ...
}: {
  # CLI-specific project configuration
  project = {
    inherit name description;
    
    # CLI apps typically need these classifiers
    classifiers = [
      "Development Status :: 3 - Alpha"
      "Intended Audience :: Developers"
      "License :: OSI Approved :: MIT License"
      "Programming Language :: Python :: 3"
      "Programming Language :: Python :: 3.13"
      "Programming Language :: Python :: 3 :: Only"
      "Environment :: Console"
      "Topic :: Utilities"
      "Typing :: Typed"
    ];

    # CLI entry point
    scripts = {
      ${name} = "${name}.main:main";
    };

    # Common CLI dependencies
    dependencies = [
      "click>=8.0"
      "rich>=13.0"
      "typer>=0.9"
    ];

    # CLI development dependencies
    optional-dependencies.dev = [
      "pytest>=7.0"
      "pytest-cov>=4.0"
      "pytest-click>=1.1"
      "mypy>=1.0"
      "ruff>=0.1"
      "types-click>=7.0"
    ];
  };

  # CLI-specific source structure
  src_structure = {
    "__init__.py" = ''
      """${description}"""
      
      import tomllib
      from importlib.metadata import PackageNotFoundError, metadata, version
      from pathlib import Path
      
      # Public API exports
      __all__ = ["main", "__version__", "__package_name__"]
      
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
    '';

    "main.py" = ''
      #!/usr/bin/env python3
      """Main entry point for the ${name} CLI application."""
      
      import sys
      from typing import Optional
      
      import click
      from rich.console import Console
      from rich.traceback import install
      
      from . import __package_name__, __version__
      
      # Install rich traceback handler
      install(show_locals=True)
      console = Console()
      
      
      @click.group()
      @click.version_option(version=__version__, prog_name=__package_name__)
      @click.option("--verbose", "-v", is_flag=True, help="Enable verbose output")
      @click.pass_context
      def cli(ctx: click.Context, verbose: bool) -> None:
          """${description}"""
          ctx.ensure_object(dict)
          ctx.obj["verbose"] = verbose
          
          if verbose:
              console.print(f"[dim]Starting {__package_name__} v{__version__}[/dim]")
      
      
      @cli.command()
      @click.argument("name", default="World")
      @click.pass_context
      def greet(ctx: click.Context, name: str) -> None:
          """Greet someone."""
          verbose = ctx.obj.get("verbose", False)
          
          if verbose:
              console.print(f"[dim]Greeting {name}...[/dim]")
          
          console.print(f"[bold green]Hello, {name}![/bold green]")
          console.print(f"This is [cyan]{__package_name__}[/cyan] v[yellow]{__version__}[/yellow]")
      
      
      def main(args: Optional[list[str]] = None) -> int:
          """Main entry point for the CLI application."""
          try:
              cli(args)
              return 0
          except KeyboardInterrupt:
              console.print("\n[red]Interrupted by user[/red]", file=sys.stderr)
              return 130
          except Exception as e:
              console.print(f"[red]Error: {e}[/red]", file=sys.stderr)
              return 1
      
      
      if __name__ == "__main__":
          sys.exit(main())
    '';

    "cli/__init__.py" = ''
      """CLI module for ${name}."""
    '';

    "cli/commands.py" = ''
      """Additional CLI commands for ${name}."""
      
      import click
      from rich.console import Console
      
      console = Console()
      
      # Add more commands here as your CLI grows
    '';
  };
}
