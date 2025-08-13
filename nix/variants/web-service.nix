# Web service template variant
{
  name,
  description ? "A web service application",
  ...
}: {
  # Web service-specific project configuration
  project = {
    inherit name description;
    
    # Web service classifiers
    classifiers = [
      "Development Status :: 3 - Alpha"
      "Intended Audience :: Developers"
      "License :: OSI Approved :: MIT License"
      "Programming Language :: Python :: 3"
      "Programming Language :: Python :: 3.13"
      "Programming Language :: Python :: 3 :: Only"
      "Framework :: FastAPI"
      "Topic :: Internet :: WWW/HTTP :: HTTP Servers"
      "Typing :: Typed"
    ];

    # Web service entry points
    scripts = {
      "${name}-server" = "${name}.main:main";
    };

    # Web service dependencies
    dependencies = [
      "fastapi>=0.104"
      "uvicorn[standard]>=0.24"
      "pydantic>=2.5"
      "pydantic-settings>=2.1"
      "httpx>=0.25"
    ];

    # Web service development dependencies
    optional-dependencies.dev = [
      "pytest>=7.0"
      "pytest-cov>=4.0"
      "pytest-asyncio>=0.21"
      "httpx>=0.25"
      "mypy>=1.0"
      "ruff>=0.1"
      "types-requests>=2.31"
    ];

    # Production dependencies
    optional-dependencies.prod = [
      "gunicorn>=21.2"
      "prometheus-client>=0.19"
    ];
  };

  # Web service-specific source structure
  src_structure = {
    "__init__.py" = ''
      """${description}"""
      
      import tomllib
      from importlib.metadata import PackageNotFoundError, metadata, version
      from pathlib import Path
      
      # Public API exports
      __all__ = ["__version__", "__package_name__"]
      
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
      """Main entry point for the ${name} web service."""
      
      import sys
      from typing import Optional
      
      import uvicorn
      from fastapi import FastAPI
      
      from . import __package_name__, __version__
      from .api import router
      from .config import get_settings
      
      
      def create_app() -> FastAPI:
          """Create and configure the FastAPI application."""
          settings = get_settings()
          
          app = FastAPI(
              title=__package_name__,
              version=__version__,
              description="${description}",
              debug=settings.debug,
          )
          
          # Include API router
          app.include_router(router, prefix="/api/v1")
          
          @app.get("/health")
          async def health_check():
              """Health check endpoint."""
              return {"status": "healthy", "version": __version__}
          
          return app
      
      
      def main(args: Optional[list[str]] = None) -> int:
          """Main entry point for the web service."""
          try:
              settings = get_settings()
              app = create_app()
              
              uvicorn.run(
                  app,
                  host=settings.host,
                  port=settings.port,
                  reload=settings.debug,
                  log_level="debug" if settings.debug else "info",
              )
              return 0
          except KeyboardInterrupt:
              print("\nShutting down server...", file=sys.stderr)
              return 0
          except Exception as e:
              print(f"Error starting server: {e}", file=sys.stderr)
              return 1
      
      
      if __name__ == "__main__":
          sys.exit(main())
    '';

    "config.py" = ''
      """Configuration settings for ${name}."""
      
      from functools import lru_cache
      from typing import Optional
      
      from pydantic import Field
      from pydantic_settings import BaseSettings
      
      
      class Settings(BaseSettings):
          """Application settings."""
          
          # Server configuration
          host: str = Field(default="127.0.0.1", description="Server host")
          port: int = Field(default=8000, description="Server port")
          debug: bool = Field(default=False, description="Debug mode")
          
          # Application configuration
          app_name: str = Field(default="${name}", description="Application name")
          version: str = Field(default="0.0.0", description="Application version")
          
          # Database configuration (if needed)
          database_url: Optional[str] = Field(default=None, description="Database URL")
          
          class Config:
              env_file = ".env"
              env_file_encoding = "utf-8"
      
      
      @lru_cache()
      def get_settings() -> Settings:
          """Get cached settings instance."""
          return Settings()
    '';

    "api/__init__.py" = ''
      """API module for ${name}."""
      
      from .router import router
      
      __all__ = ["router"]
    '';

    "api/router.py" = ''
      """API router for ${name}."""
      
      from fastapi import APIRouter, HTTPException
      from pydantic import BaseModel
      
      router = APIRouter()
      
      
      class GreetingRequest(BaseModel):
          """Request model for greeting endpoint."""
          name: str
      
      
      class GreetingResponse(BaseModel):
          """Response model for greeting endpoint."""
          message: str
          name: str
      
      
      @router.get("/")
      async def root():
          """Root endpoint."""
          return {"message": "Welcome to ${name} API"}
      
      
      @router.post("/greet", response_model=GreetingResponse)
      async def greet(request: GreetingRequest):
          """Greet endpoint."""
          if not request.name.strip():
              raise HTTPException(status_code=400, detail="Name cannot be empty")
          
          return GreetingResponse(
              message=f"Hello, {request.name}!",
              name=request.name
          )
    '';

    "models.py" = ''
      """Data models for ${name}."""
      
      from datetime import datetime
      from typing import Optional
      
      from pydantic import BaseModel, Field
      
      
      class BaseEntity(BaseModel):
          """Base entity with common fields."""
          
          id: Optional[int] = Field(default=None, description="Entity ID")
          created_at: Optional[datetime] = Field(default=None, description="Creation timestamp")
          updated_at: Optional[datetime] = Field(default=None, description="Update timestamp")
          
          class Config:
              from_attributes = True
    '';

    "services.py" = ''
      """Business logic services for ${name}."""
      
      from typing import Any, Optional
      
      
      class ${name.title().replace("-", "").replace("_", "")}Service:
          """Main service class for ${name}."""
          
          def __init__(self, config: Optional[dict[str, Any]] = None) -> None:
              """Initialize the service.
              
              Args:
                  config: Optional configuration dictionary
              """
              self.config = config or {}
          
          async def process_data(self, data: Any) -> Any:
              """Process data asynchronously.
              
              Args:
                  data: Input data to process
                  
              Returns:
                  Processed data
              """
              # Implement your business logic here
              return data
    '';
  };

  # Additional web service files
  additional_files = {
    ".env.example" = ''
      # ${name} Configuration
      
      # Server settings
      HOST=127.0.0.1
      PORT=8000
      DEBUG=true
      
      # Database settings (uncomment if using a database)
      # DATABASE_URL=sqlite:///./app.db
      
      # Other settings
      APP_NAME=${name}
      VERSION=0.0.0
    '';

    "docker-compose.yml" = ''
      version: '3.8'
      
      services:
        ${name}:
          build: .
          ports:
            - "8000:8000"
          environment:
            - HOST=0.0.0.0
            - PORT=8000
            - DEBUG=false
          env_file:
            - .env
          volumes:
            - .:/app
          command: ["python", "-m", "${name}"]
      
        # Uncomment if you need a database
        # db:
        #   image: postgres:15
        #   environment:
        #     POSTGRES_DB: ${name}
        #     POSTGRES_USER: user
        #     POSTGRES_PASSWORD: password
        #   volumes:
        #     - postgres_data:/var/lib/postgresql/data
        #   ports:
        #     - "5432:5432"
      
      # volumes:
      #   postgres_data:
    '';

    "Dockerfile" = ''
      FROM python:3.13-slim
      
      WORKDIR /app
      
      # Install system dependencies
      RUN apt-get update && apt-get install -y \
          build-essential \
          && rm -rf /var/lib/apt/lists/*
      
      # Copy requirements
      COPY pyproject.toml uv.lock ./
      
      # Install uv and dependencies
      RUN pip install uv
      RUN uv sync --frozen
      
      # Copy source code
      COPY src/ ./src/
      
      # Install the package
      RUN uv pip install -e .
      
      EXPOSE 8000
      
      CMD ["python", "-m", "${name}"]
    '';
  };
}
