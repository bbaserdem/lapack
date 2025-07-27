# Phase 1 Analysis Complete: LAPACK Structure and LAPACKE Interface

## Executive Summary
This document provides a comprehensive reference for implementing Rust FFI bindings to LAPACK, based on the analysis of LAPACK structure, CMake build system, and LAPACKE C interface.

## 1. LAPACK Project Structure

### Critical Directories for Rust FFI
- **`/LAPACKE/`** - Primary target for FFI bindings
  - `include/` - Header files (lapacke.h, lapacke_config.h, lapacke_utils.h)
  - `src/` - C wrapper implementations
  - `utils/` - Utility functions
  
- **`/SRC/`** - LAPACK Fortran source (reference only)
- **`/CMAKE/`** - Build configuration modules
- **`/TESTING/`** - Test suites for validation reference

## 2. Build System Configuration

### Essential CMake Commands
```bash
# Minimal LAPACKE build
cmake -DLAPACKE=ON -DBUILD_TESTING=OFF -DCMAKE_BUILD_TYPE=Release ..
make lapacke

# Static library build (recommended for Rust)
cmake -DLAPACKE=ON -DBUILD_SHARED_LIBS=OFF ..
```

### Key Build Options
- `LAPACKE=ON` - Enable LAPACKE C interface
- `BUILD_INDEX64=ON` - 64-bit integer support
- `BUILD_SHARED_LIBS=OFF` - Static linking
- Precision flags: `BUILD_SINGLE`, `BUILD_DOUBLE`, `BUILD_COMPLEX`, `BUILD_COMPLEX16`

## 3. LAPACKE C Interface

### Type System
```c
// Integer types
typedef int32_t lapack_int;     // Default
typedef int64_t lapack_int;     // With LAPACK_ILP64

// Complex types (default C99)
typedef float _Complex lapack_complex_float;
typedef double _Complex lapack_complex_double;

// Constants
#define LAPACK_ROW_MAJOR 101
#define LAPACK_COL_MAJOR 102
```

### Function Naming Convention
Pattern: `LAPACKE_<precision><operation>`
- Precision: `s` (float), `d` (double), `c` (complex float), `z` (complex double)
- Operation: `gesv`, `getrf`, `potrf`, etc.

### Standard Function Signature
```c
lapack_int LAPACKE_routine(
    int matrix_layout,    // Row/column major
    /* routine-specific parameters */
) -> lapack_int;         // 0 = success, <0 = error
```

## 4. Core Routines for Initial Implementation

### Priority Matrix

| Category | Routine | Purpose | Priority |
|----------|---------|---------|----------|
| **Linear Systems** | | | |
| | `gesv` | General system solver AX=B | **P1** |
| | `getrf/getrs` | LU factorization & solve | **P1** |
| | `potrf/potrs` | Cholesky factorization & solve | **P1** |
| **Least Squares** | | | |
| | `gels` | QR/LQ least squares | **P2** |
| | `gelsd` | SVD least squares | **P2** |
| **Eigenvalues** | | | |
| | `syev` | Symmetric eigenvalues | **P3** |
| | `geev` | General eigenvalues | **P3** |
| **SVD** | | | |
| | `gesvd` | General SVD | **P4** |
| | `gesdd` | Divide-conquer SVD | **P4** |

### Initial Focus: Double Precision P1 Routines
1. `LAPACKE_dgesv` - Most common use case
2. `LAPACKE_dgetrf` + `LAPACKE_dgetrs` - Factorization approach
3. `LAPACKE_dpotrf` + `LAPACKE_dpotrs` - Positive definite systems

## 5. Rust FFI Implementation Strategy

### Project Structure
```
rust/
├── lapack-sys/          # Low-level FFI bindings
│   ├── src/
│   │   ├── lib.rs       # Main FFI definitions
│   │   ├── types.rs     # Type definitions
│   │   └── lapacke.rs   # Function bindings
│   └── build.rs         # Build script
├── lapack/              # Safe Rust wrappers
│   ├── src/
│   │   ├── lib.rs
│   │   ├── error.rs     # Error handling
│   │   ├── linear.rs    # Linear system solvers
│   │   └── types.rs     # Safe type wrappers
└── lapack-src/          # Backend selection
```

### Type Mappings
```rust
// lapack-sys/src/types.rs
pub type lapack_int = i32;  // or i64 for ILP64
pub type lapack_logical = lapack_int;

// Using num-complex for complex types
pub type lapack_complex_float = Complex<f32>;
pub type lapack_complex_double = Complex<f64>;

// Constants
pub const LAPACK_ROW_MAJOR: c_int = 101;
pub const LAPACK_COL_MAJOR: c_int = 102;
```

### Example FFI Binding
```rust
// lapack-sys/src/lapacke.rs
extern "C" {
    pub fn LAPACKE_dgesv(
        matrix_layout: c_int,
        n: lapack_int,
        nrhs: lapack_int,
        a: *mut f64,
        lda: lapack_int,
        ipiv: *mut lapack_int,
        b: *mut f64,
        ldb: lapack_int,
    ) -> lapack_int;
}
```

### Safe Wrapper Pattern
```rust
// lapack/src/linear.rs
pub fn dgesv(
    a: &mut Array2<f64>,
    b: &mut Array2<f64>,
) -> Result<Vec<i32>, LapackError> {
    // Validate dimensions
    // Get raw pointers
    // Call unsafe FFI
    // Convert error codes
    // Return pivot indices
}
```

## 6. Build Integration

### Build Script (build.rs)
```rust
fn main() {
    // Use pkg-config if available
    if pkg_config::probe_library("lapacke").is_ok() {
        return;
    }
    
    // Otherwise link manually
    println!("cargo:rustc-link-lib=lapacke");
    println!("cargo:rustc-link-lib=lapack");
    println!("cargo:rustc-link-lib=blas");
}
```

### Feature Flags
```toml
[features]
default = []
system = ["lapack-src/system"]
netlib = ["lapack-src/netlib"]
openblas = ["lapack-src/openblas"]
intel-mkl = ["lapack-src/intel-mkl"]
```

## 7. Testing Strategy

### Unit Tests
- Test each routine with known solutions
- Verify error handling for invalid inputs
- Test both row and column major layouts

### Integration Tests
```rust
#[test]
fn test_dgesv_2x2() {
    let mut a = array![[3.0, 1.0], [1.0, 2.0]];
    let mut b = array![[9.0], [8.0]];
    
    let pivots = dgesv(&mut a, &mut b).unwrap();
    
    assert_abs_diff_eq!(b[[0, 0]], 2.0, epsilon = 1e-10);
    assert_abs_diff_eq!(b[[1, 0]], 3.0, epsilon = 1e-10);
}
```

## 8. Next Steps

1. **Implement lapack-sys crate**
   - Define core types
   - Add P1 routine bindings
   - Create build script

2. **Create safe wrappers**
   - Error handling framework
   - Memory safety guarantees
   - Ergonomic API design

3. **Comprehensive testing**
   - Unit tests for each routine
   - Property-based testing
   - Performance benchmarks

4. **Documentation**
   - API documentation
   - Usage examples
   - Migration guide from C

## Appendix: Quick Reference

### Building LAPACKE
```bash
git clone https://github.com/Reference-LAPACK/lapack.git
cd lapack
mkdir build && cd build
cmake -DLAPACKE=ON -DBUILD_SHARED_LIBS=OFF ..
make -j lapacke
```

### Finding LAPACKE
```bash
pkg-config --cflags --libs lapacke
```

### Common Error Codes
- `0`: Success
- `< 0`: Invalid parameter at position `-error_code`
- `> 0`: Computational failure (routine-specific)

This completes the Phase 1 analysis. The project now has a solid foundation for implementing Rust FFI bindings to LAPACK.