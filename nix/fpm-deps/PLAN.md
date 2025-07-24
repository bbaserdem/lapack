# FPM Nix Packaging Plan

## Problem Statement

FPM (Fortran Package Manager) presents unique challenges for Nix packaging:

1. **Network Dependency Fetching**: FPM downloads dependencies during build, which Nix's sandbox forbids
2. **No Existing Packages**: No FPM packages exist in nixpkgs yet, so we're pioneering this approach
3. **Circular Dependencies**: Some packages (like fpm-deps) depend on FPM itself as a library

## Current State

- We have a generic FPM builder (`default.nix`) that works for simple packages
- fpm-deps currently uses a wrapper approach to avoid build-time issues
- The wrapper approach fails because FPM tries to create build directories in read-only Nix store

## Dependency Tree Analysis

### fpm-deps Dependencies
```
fpm-deps (0.2.1)
└── fpm (ac06dfc6a8c542261c4f5f133a119c2747d47da6)
    ├── toml-f (d7b892b1d074b7cfc5d75c3e0eb36ebc1f7958c1)
    ├── M_CLI2 (7264878cdb1baff7323cc48596d829ccfe7751b8)
    └── fortran-regex (1.1.2)
```

### Required Fetches
1. **fpm-deps**: Already have (main package)
2. **fpm**: `github:fortran-lang/fpm` @ `ac06dfc6a8c542261c4f5f133a119c2747d47da6`
3. **toml-f**: `github:toml-f/toml-f` @ `d7b892b1d074b7cfc5d75c3e0eb36ebc1f7958c1`
4. **M_CLI2**: `github:urbanjost/M_CLI2` @ `7264878cdb1baff7323cc48596d829ccfe7751b8`
5. **fortran-regex**: `github:perazz/fortran-regex` @ `1.1.2`

## Implementation Strategy

### Phase 1: Pre-fetch All Dependencies

```nix
# In the derivation
fpm-src = fetchFromGitHub {
  owner = "fortran-lang";
  repo = "fpm";
  rev = "ac06dfc6a8c542261c4f5f133a119c2747d47da6";
  sha256 = ""; # To be determined
};

toml-f-src = fetchFromGitHub {
  owner = "toml-f";
  repo = "toml-f";
  rev = "d7b892b1d074b7cfc5d75c3e0eb36ebc1f7958c1";
  sha256 = ""; # To be determined
};

# ... similar for other dependencies
```

### Phase 2: Patch fpm.toml Files

Two approaches to consider:

#### Option A: Path Dependencies
```toml
# Patch fpm-deps/fpm.toml
[dependencies]
fpm.path = "@FPM_SRC@"  # Will be substituted
```

#### Option B: Vendor Directory Structure
```
build/
└── dependencies/
    ├── fpm/
    ├── toml-f/
    ├── M_CLI2/
    └── fortran-regex/
```

### Phase 3: Build Process

```nix
preBuild = ''
  # Create dependency structure
  mkdir -p build/dependencies
  
  # Link or copy dependencies
  cp -r ${fpm-src} build/dependencies/fpm
  
  # Patch fpm's own fpm.toml for its dependencies
  substituteInPlace build/dependencies/fpm/fpm.toml \
    --replace 'toml-f.git = "..."' 'toml-f.path = "../toml-f"' \
    --replace 'M_CLI2.git = "..."' 'M_CLI2.path = "../M_CLI2"' \
    --replace 'fortran-regex.git = "..."' 'fortran-regex.path = "../fortran-regex"'
  
  # Copy FPM's dependencies
  cp -r ${toml-f-src} build/dependencies/toml-f
  cp -r ${m-cli2-src} build/dependencies/M_CLI2
  cp -r ${fortran-regex-src} build/dependencies/fortran-regex
  
  # Patch main fpm.toml
  substituteInPlace fpm.toml \
    --replace 'fpm.git = "..."' 'fpm.path = "build/dependencies/fpm"'
'';

buildPhase = ''
  fortran-fpm build --release
'';
```

## Technical Considerations

### 1. FPM Dependency Resolution
- Need to understand if FPM supports relative paths in `path` dependencies
- May need to use absolute paths instead
- Check if FPM looks for pre-built dependencies or needs source

### 2. Build Order
- Dependencies might need to be built in order
- Or FPM might handle this automatically

### 3. Module Files
- Fortran module files (.mod) need to be in the right place
- FPM might expect specific directory structures

## Alternative Approaches

### 1. Bootstrap FPM First
- Build a minimal FPM without dependencies
- Use that to build the full FPM
- Then build fpm-deps

### 2. Patch FPM Itself
- Modify FPM to support offline/vendored dependencies better
- Submit patches upstream

### 3. Use FPM's Cache
- Pre-populate FPM's cache directory
- Set FPM_CACHE environment variable

## Next Steps

1. **Test Dependency Fetching**: Verify all git revisions fetch correctly
2. **Explore FPM Internals**: Understand exact dependency resolution mechanism
3. **Prototype**: Try the vendoring approach with a simple test
4. **Iterate**: Refine based on what works

## Success Criteria

- [ ] fpm-deps builds successfully in Nix sandbox
- [ ] No network access required during build
- [ ] Resulting binary works correctly
- [ ] Approach is reusable for other FPM packages
- [ ] Clear documentation for future FPM packagers

## Open Questions

1. Does FPM support `path` in dependency specification?
2. What's the exact structure FPM expects in `build/dependencies/`?
3. Can we use symlinks or do we need copies?
4. How does FPM handle transitive dependency versions?
5. Is there a way to make FPM skip dependency fetching if they exist?

---

This plan will be updated as we discover more about FPM's behavior and requirements.