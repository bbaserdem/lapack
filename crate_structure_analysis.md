# Crate Structure Analysis: Monolithic vs. Modular

## Overview

This document analyzes the trade-offs between monolithic and modular crate structures for Rust LAPACK bindings and presents our decision.

## Option 1: Monolithic Structure

A single crate containing both FFI bindings and safe wrappers.

### Pros:
- **Simpler dependency management**: Users only need to depend on one crate
- **Easier versioning**: Single version number to track
- **Less boilerplate**: No need for separate Cargo.toml files
- **Faster compilation**: No inter-crate dependencies

### Cons:
- **Poor separation of concerns**: Mixes unsafe FFI with safe abstractions
- **Harder to maintain**: Changes to FFI affect entire crate
- **Limited flexibility**: Users cannot opt-out of safe wrappers
- **Larger binary size**: All code included even if only FFI needed
- **Testing complexity**: Unsafe and safe code tested together

## Option 2: Modular Structure

Multiple crates with clear separation:
- `lapack-sys`: Raw FFI bindings
- `lapack`: Safe Rust wrappers
- `lapack-src`: Backend provider selection

### Pros:
- **Clear separation of concerns**: FFI isolated from safe code
- **Better maintainability**: Changes localized to specific crates
- **Flexibility**: Users can use raw FFI if needed
- **Follows Rust conventions**: Standard `-sys` pattern
- **Easier testing**: Test unsafe and safe code separately
- **Backend flexibility**: Easy to swap LAPACK implementations

### Cons:
- **More complex setup**: Multiple crates to manage
- **Version coordination**: Need to keep crates in sync
- **Additional boilerplate**: Multiple Cargo.toml files
- **Potential dependency conflicts**: If versions misalign

## Research Findings Comparison

Based on our research of existing crates:

### Established Pattern
All major LAPACK/BLAS crates use modular structure:
- `blas-sys` + `blas`
- `lapack-sys` + `lapack` 
- `lapacke-sys` + `lapacke`
- `openblas-sys` + `openblas-src`

### Community Expectations
The Rust community expects:
- `-sys` crates for raw FFI
- Safe wrapper crates built on top
- Separate source provider crates

## LAPACK Complexity Considerations

LAPACK contains:
- 1000+ functions across different categories
- Multiple data types (S, D, C, Z)
- Various matrix types (general, symmetric, banded, etc.)
- Complex dependency relationships

This complexity strongly favors modular structure for:
- Incremental implementation
- Category-based organization
- Type-specific optimizations
- Easier code generation

## Maintenance and Versioning

### Modular Advantages:
- **Independent updates**: Can update FFI without touching wrappers
- **Semantic versioning**: Each crate follows its own semver
- **Gradual migration**: Users can migrate at their own pace
- **Parallel development**: Teams can work on different crates

### Version Coordination Strategy:
```toml
# In lapack/Cargo.toml
[dependencies]
lapack-sys = { version = "0.1", path = "../lapack-sys" }

# Use workspace for coordinated development
[workspace]
members = ["lapack-sys", "lapack", "lapack-src"]
```

## User Experience

### For Library Authors:
- Can depend on minimal `lapack-sys` if needed
- Clear upgrade path from unsafe to safe
- Choice of backend implementation

### For Application Developers:
- Simple high-level API via `lapack` crate
- Documentation separated by safety level
- Clear examples for common use cases

## Decision: Modular Structure

We will adopt a **modular structure** with the following crates:

### 1. `lapack-sys`
- Raw FFI bindings to LAPACK
- Generated from LAPACK sources
- No dependencies except `libc`
- Follows standard `-sys` conventions

### 2. `lapack`
- Safe Rust wrappers
- Idiomatic API design
- Comprehensive error handling
- Built on `lapack-sys`

### 3. `lapack-src`
- Backend provider selection
- Features for different implementations:
  - `netlib`: Reference implementation
  - `openblas`: OpenBLAS backend
  - `mkl`: Intel MKL backend

## Rationale

The modular structure is chosen because:

1. **Industry Standard**: All successful Rust numerical computing crates use this pattern
2. **Maintainability**: LAPACK's size demands clear separation of concerns
3. **Flexibility**: Users have full control over safety/performance trade-offs
4. **Future-Proof**: Easy to add new backends or wrapper styles
5. **Community Friendly**: Follows established Rust conventions

## Implementation Plan

```
lapack-rust/
├── Cargo.toml              # Workspace root
├── README.md               # Project overview
├── lapack-sys/
│   ├── Cargo.toml
│   ├── build.rs            # FFI generation
│   ├── src/
│   │   └── lib.rs          # Generated bindings
│   └── lapack-bind-gen/    # Binding generator scripts
├── lapack/
│   ├── Cargo.toml
│   ├── src/
│   │   ├── lib.rs          # Public API
│   │   ├── error.rs        # Error types
│   │   ├── matrix/         # Matrix types
│   │   └── operations/     # Safe wrappers
│   ├── tests/
│   └── examples/
└── lapack-src/
    ├── Cargo.toml
    └── src/
        └── lib.rs          # Backend selection
```

This structure provides the best balance of maintainability, flexibility, and user experience for Rust LAPACK bindings.