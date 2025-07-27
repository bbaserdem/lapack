# Rust LAPACK Bindings

This project provides Rust bindings to LAPACK (Linear Algebra PACKage), offering both low-level FFI access and high-level safe abstractions.

## Project Structure

We have adopted a **modular crate structure** following established Rust conventions:

### Crates

- **`lapack-sys`**: Low-level FFI bindings to LAPACK functions
  - Raw, unsafe bindings
  - Minimal dependencies
  - Direct mapping to LAPACK C/Fortran API

- **`lapack`**: High-level safe Rust wrappers
  - Idiomatic Rust API
  - Comprehensive error handling
  - Type-safe matrix operations

- **`lapack-src`**: Backend provider selection
  - Allows choosing between different LAPACK implementations
  - Supports netlib, OpenBLAS, Intel MKL, etc.

## Design Decision: Modular Structure

After careful analysis (see `crate_structure_analysis.md`), we chose a modular structure for the following reasons:

1. **Separation of Concerns**: Clear boundary between unsafe FFI and safe abstractions
2. **Flexibility**: Users can choose to use raw FFI or safe wrappers
3. **Maintainability**: Changes to FFI don't affect high-level API
4. **Industry Standard**: Follows the established `-sys` crate pattern
5. **Backend Flexibility**: Easy to swap LAPACK implementations

## Getting Started

### For Application Developers

Add this to your `Cargo.toml`:

```toml
[dependencies]
lapack = "0.1"
lapack-src = { version = "0.1", features = ["openblas"] }
```

### For Library Authors

If you only need raw FFI:

```toml
[dependencies]
lapack-sys = "0.1"
```

## Minimum Supported Rust Version (MSRV)

This project requires Rust 1.72 or later. We use:
- **Edition**: 2021
- **MSRV**: 1.72

### Rationale for MSRV Selection

We chose Rust 1.72 (released August 2023) for the following reasons:

1. **Modern Features**: Access to important stabilized features like const generics improvements, GATs refinements, and better async support
2. **Scientific Computing Compatibility**: Aligns well with the ecosystem - ndarray requires 1.64+, and BLAS backends often require 1.71+
3. **Stability**: Over a year old, ensuring wide availability across platforms
4. **Conservative Choice**: Avoids bleeding-edge features while providing modern Rust capabilities

We use Edition 2021 rather than 2024 to maintain broader compatibility while still having access to important features like disjoint captures in closures, IntoIterator for arrays, and improved pattern matching.

## Error Handling Strategy

This project follows Rust's idiomatic error handling practices using `Result<T, E>` for fallible operations.

### Error Type Design

We use a custom `LapackError` enum (defined in `lapack::Error`) that categorizes different failure modes:

- **`InvalidParameter`**: Illegal argument values (corresponds to INFO < 0 in LAPACK)
- **`SingularMatrix`**: Matrix is singular and cannot be factored
- **`ConvergenceFailure`**: Iterative algorithms failed to converge
- **`DimensionMismatch`**: Input dimensions are incompatible
- **`InvalidLayout`**: Matrix layout is invalid for the operation
- **`AllocationError`**: Memory allocation failures
- **`LapackError`**: General LAPACK errors with INFO code

### Error Handling Principles

1. **Use `Result` for all fallible operations**: All LAPACK wrapper functions return `Result<T, Error>`
2. **Never panic in library code**: Panics are reserved for true programmer errors (invariant violations)
3. **Preserve error information**: LAPACK INFO codes are translated to meaningful error types
4. **Support error propagation**: Errors implement standard traits for use with `?` operator

### FFI Boundary Safety

At the FFI boundary (in `lapack-sys`):
- Raw functions remain `unsafe` and don't perform error checking
- Higher-level wrappers in `lapack` crate check INFO parameters and convert to `Result`
- All pointer validity and dimension checks happen in the safe wrapper layer

### Example Usage

```rust
use lapack::{Result, Error};

fn solve_system(a: &mut [f64], b: &mut [f64]) -> Result<()> {
    // This returns Result<(), Error>
    lapack::dgesv(Layout::ColumnMajor, n, nrhs, a, lda, b, ldb)?;
    Ok(())
}

// Handle errors explicitly
match solve_system(&mut a, &mut b) {
    Ok(()) => println!("System solved successfully"),
    Err(Error::SingularMatrix { position }) => {
        eprintln!("Matrix is singular at position {}", position);
    }
    Err(e) => eprintln!("LAPACK error: {}", e),
}
```

## Linking Strategy

This project supports both static and dynamic linking with multiple LAPACK implementations. The linking behavior is controlled through Cargo features.

### Default Behavior

By default, no LAPACK implementation is selected. Users must explicitly choose a backend through `lapack-src` features.

### Available Backends

- **netlib**: Reference LAPACK implementation
- **openblas**: OpenBLAS (includes optimized BLAS + LAPACK)
- **intel-mkl**: Intel Math Kernel Library

### Linking Options

Each backend supports different linking modes:

```toml
# Dynamic linking (default when available)
lapack-src = { version = "0.1", features = ["openblas"] }

# Static linking
lapack-src = { version = "0.1", features = ["openblas-static"] }

# System-provided library
lapack-src = { version = "0.1", features = ["openblas-system"] }
```

### Feature Combinations

- `backend-static`: Forces static linking
- `backend-system`: Uses system-installed library (prefers dynamic)
- `cache`: Reuses build artifacts between builds (OpenBLAS only)

### Platform Notes

#### Linux
- Dynamic libraries require `LD_LIBRARY_PATH` if not in standard locations
- System packages typically provide dynamic libraries by default

#### macOS
- Can use Accelerate framework (future feature)
- Dynamic libraries require `DYLD_LIBRARY_PATH` if not in standard locations

#### Windows
- OpenBLAS requires vcpkg or pre-built binaries
- Use `x64-windows-static` triplet for static linking
- Intel MKL works well on Windows with proper installation

### Recommendations

1. **For Applications**: Choose a specific backend with your preferred linking
2. **For Libraries**: Don't specify a backend; let the application decide
3. **For Development**: Use dynamic linking for faster builds
4. **For Distribution**: Consider static linking for easier deployment

### Example Configurations

```toml
# Development setup with fast rebuilds
[dependencies]
lapack = "0.1"
lapack-src = { version = "0.1", features = ["openblas", "cache"] }

# Production build with static linking
[dependencies]
lapack = "0.1"
lapack-src = { version = "0.1", features = ["openblas-static"] }

# Using system-installed LAPACK
[dependencies]
lapack = "0.1"
lapack-src = { version = "0.1", features = ["netlib-system"] }
```

## Feature Flags

This project uses a hierarchical feature flag system to allow fine-grained control over which LAPACK routines are included in your build. This helps reduce binary size and compilation time for applications that only need specific functionality.

### Default Features

- `std`: Standard library support (enabled by default)

### Routine Categories

The feature flags follow LAPACK's natural organization:

#### Full Support
- `full`: Enables all LAPACK routines (driver + computational + auxiliary)

#### Major Categories
- `driver`: All driver routines (high-level problem solvers)
- `computational`: All computational routines (matrix operations)
- `auxiliary`: All auxiliary routines (BLAS + utilities)

#### Driver Routine Subcategories
- `linear-systems`: Linear equation solvers (GESV, POSV, etc.)
- `least-squares`: Least squares problems (GELS, GELSS, etc.)
- `eigenvalues`: Eigenvalue problems (GEEV, SYEV, etc.)
- `svd`: Singular value decomposition (GESVD, etc.)

#### Computational Routine Subcategories
- `factorization`: LU, Cholesky, QR factorizations
- `orthogonal`: Orthogonal/unitary transformations
- `reduction`: Matrix reductions (to triangular, bidiagonal, etc.)

#### BLAS Support Levels
- `blas1`: Vector operations (AXPY, DOT, etc.)
- `blas2`: Matrix-vector operations (GEMV, GER, etc.)
- `blas3`: Matrix-matrix operations (GEMM, etc.)
- `utility`: Utility routines (LACPY, LASET, etc.)

### Precision Options

Control which numeric types are supported:

- `single`: Single precision (f32) support
- `double`: Double precision (f64) support
- `complex`: Complex number support
- `all-precisions`: Enable all precision types

### Advanced Features

- `parallel`: Enable parallel execution support
- `serialization`: Enable serde serialization for matrix types

### Usage Examples

```toml
# Minimal: Only linear system solvers
[dependencies]
lapack = { version = "0.1", features = ["linear-systems"] }

# Scientific computing: Common numerical algorithms
[dependencies]
lapack = { version = "0.1", features = ["linear-systems", "eigenvalues", "svd"] }

# Full LAPACK support
[dependencies]
lapack = { version = "0.1", features = ["full"] }

# No-std embedded environment
[dependencies]
lapack = { version = "0.1", default-features = false, features = ["linear-systems"] }

# Specific precision only
[dependencies]
lapack = { version = "0.1", features = ["linear-systems", "double"] }
```

### Feature Flag Guidelines

1. **Start Small**: Only enable the features you need
2. **Use Categories**: Enable routine categories rather than individual routines
3. **Consider Binary Size**: Each feature adds to compilation time and binary size
4. **Precision Matters**: Only enable the numeric types you actually use

## Project Status

This project is currently in early development. See the task list for progress on individual components.

## Contributing

Contributions are welcome! Please see our contributing guidelines.

## License

This project is licensed under the same terms as LAPACK itself.