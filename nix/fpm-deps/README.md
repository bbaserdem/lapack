# FPM (Fortran Package Manager) Nix Builder

This directory contains a generic Nix builder for Fortran packages that use FPM (Fortran Package Manager).

## Structure

- `default.nix` - The main FPM builder derivation
- `builder.nix` - Helper functions for building FPM packages
- `example-package.nix` - Example of how to package a Fortran project
- `fpm-deps-tool.nix` - The fpm-deps dependency visualization tool

## Usage

### Basic Usage

To package a Fortran project that uses FPM, create a derivation like this:

```nix
{ callPackage, fetchFromGitHub }:

let
  fpmBuilder = callPackage ./fpm-deps/default.nix { };
in
fpmBuilder {
  pname = "my-fortran-package";
  version = "1.0.0";
  
  src = fetchFromGitHub {
    owner = "username";
    repo = "repo-name";
    rev = "v1.0.0";
    sha256 = "...";
  };
  
  meta = {
    description = "My Fortran package";
    homepage = "https://github.com/username/repo-name";
    license = lib.licenses.mit;
  };
}
```

### Using the Builder Helper

The `builder.nix` provides convenient functions:

```nix
{ fpm-builder }:

# Build from GitHub
fpm-builder.buildFpmPackage {
  pname = "my-package";
  version = "1.0.0";
  src = fetchFromGitHub { ... };
}

# Build from local source
fpm-builder.buildLocalFpmPackage {
  pname = "local-package";
  version = "dev";
  src = ./path/to/source;
}

# Build with dependencies
fpm-builder.buildFpmPackageWithDeps {
  pname = "package-with-deps";
  version = "1.0.0";
  src = ...;
  dependencies = [ openblas lapack ];
}
```

### Custom Build Options

You can customize the build process:

```nix
fpmBuilder {
  # ... basic attributes ...
  
  # Custom FPM build flags
  fpmBuildFlags = [ "--release" "--flag" "-O3" ];
  
  # Custom FPM install flags
  fpmInstallFlags = [ "--no-rebuild" ];
  
  # Additional build inputs
  buildInputs = [ openblas ];
  
  # Pre-build setup
  preBuildPhases = [
    ''
      export CUSTOM_VAR=value
    ''
  ];
  
  # Post-install fixups
  postInstallPhases = [
    ''
      # Additional installation steps
    ''
  ];
}
```

## Environment Variables

The builder sets these environment variables:
- `FC` - Fortran compiler (gfortran)
- `FFLAGS` - Compiler flags (-O3 -fPIC)
- `FPM_FC` - FPM-specific compiler variable
- `FPM_FFLAGS` - FPM-specific flags

## Notes

- FPM normally downloads dependencies during build, but Nix sandboxing prevents this
- For packages with dependencies, you'll need to vendor them or provide them as buildInputs
- The builder automatically patches executables with correct rpaths for libraries

## Special Cases

### fpm-deps Tool

The `fpm-deps-tool.nix` package demonstrates a special case where the FPM builder cannot be used directly:

1. **Circular Dependency**: fpm-deps depends on FPM itself as a library (not just as a build tool)
2. **Network Access**: FPM would try to download this dependency during build, which Nix forbids
3. **Solution**: Use wrapper scripts that execute `fpm run` at runtime instead of building

This approach is necessary for any FPM package that:
- Depends on FPM itself as a library
- Has dependencies that cannot be easily vendored
- Requires network access during the build process

See `fpm-deps-tool.nix` for the implementation and `fpm-deps-builder-example.nix` for how it would look if we could use the standard builder.