# LAPACKE Headers Analysis

## Overview
This document analyzes the LAPACKE header files to understand the C interface, naming conventions, and data types used for LAPACK routines.

## Key Header Files

### 1. `lapacke.h`
- Main LAPACKE interface header
- Includes function prototypes for all LAPACKE routines
- Defines matrix layout constants and error codes

### 2. `lapacke_config.h`
- Configuration and type definitions
- Platform-specific type mappings
- Complex number type definitions

### 3. `lapacke_utils.h`
- Utility function declarations
- Helper routines for LAPACKE implementation

## Data Types

### Integer Types
```c
// Standard 32-bit integer (default)
#define lapack_int        int32_t

// 64-bit integer (when LAPACK_ILP64 is defined)
#define lapack_int        int64_t

// Logical type (same as lapack_int)
#define lapack_logical    lapack_int
```

### Complex Types
LAPACKE supports multiple complex number representations:

1. **Structure-based** (when `LAPACK_COMPLEX_STRUCTURE` is defined):
```c
typedef struct { float real, imag; } _lapack_complex_float;
typedef struct { double real, imag; } _lapack_complex_double;
```

2. **C99 Complex** (default):
```c
#define lapack_complex_float    float _Complex
#define lapack_complex_double   double _Complex
```

3. **C++ Complex** (when `LAPACK_COMPLEX_CPP` is defined):
```c
#define lapack_complex_float    std::complex<float>
#define lapack_complex_double   std::complex<double>
```

### Helper Functions
```c
lapack_complex_float lapack_make_complex_float(float re, float im);
lapack_complex_double lapack_make_complex_double(double re, double im);
```

## Constants and Enums

### Matrix Layout
```c
#define LAPACK_ROW_MAJOR               101
#define LAPACK_COL_MAJOR               102
```

### Error Codes
```c
#define LAPACK_WORK_MEMORY_ERROR       -1010
#define LAPACK_TRANSPOSE_MEMORY_ERROR  -1011
```

## Function Naming Conventions

### Pattern: `LAPACKE_<precision><name>`

#### Precision Prefixes:
- `s` - Single precision real (float)
- `d` - Double precision real (double)
- `c` - Single precision complex (lapack_complex_float)
- `z` - Double precision complex (lapack_complex_double)

#### Examples:
- `LAPACKE_sgesv` - Single precision general matrix solver
- `LAPACKE_dgesv` - Double precision general matrix solver
- `LAPACKE_cgesv` - Single precision complex general matrix solver
- `LAPACKE_zgesv` - Double precision complex general matrix solver

### Function Signature Pattern

All LAPACKE functions follow a consistent pattern:
1. Return type: `lapack_int` (error code, 0 = success)
2. First parameter: `int matrix_layout` (row/column major)
3. Remaining parameters: routine-specific

Example:
```c
lapack_int LAPACKE_dgesv(int matrix_layout, lapack_int n, lapack_int nrhs,
                         double* a, lapack_int lda, lapack_int* ipiv,
                         double* b, lapack_int ldb);
```

## Core Routine Categories

### 1. Linear Systems Solvers
- `gesv` - General matrix solver
- `getrf` - LU factorization
- `getrs` - Solve using LU factorization
- `posv` - Positive definite solver
- `potrf` - Cholesky factorization
- `potrs` - Solve using Cholesky

### 2. Least Squares
- `gels` - Least squares solver
- `gelsd` - Least squares with SVD
- `gelss` - Least squares with SVD (deprecated)
- `gelsy` - Least squares with complete orthogonal factorization

### 3. Eigenvalue Problems
- `geev` - General eigenvalues and eigenvectors
- `syev` - Symmetric eigenvalues
- `heev` - Hermitian eigenvalues

### 4. Singular Value Decomposition
- `gesvd` - General SVD
- `gesdd` - SVD using divide-and-conquer
- `gesvdx` - SVD with subset selection
- `gesvdq` - SVD with preconditioning

## Memory Management

### Allocation/Deallocation
```c
#define LAPACKE_malloc(size)    malloc(size)
#define LAPACKE_free(p)         free(p)
```

### Work Arrays
Many LAPACKE routines have two versions:
1. High-level: Automatically allocates work arrays
2. Work version: User provides work arrays (suffix `_work`)

## API Versioning

### 64-bit Integer API
When `LAPACKE_API64` is defined:
- Functions get `_64` suffix
- Uses 64-bit integers for all size parameters
- Enabled via `BUILD_INDEX64_EXT_API` CMake option

Example:
```c
#ifdef LAPACKE_API64
    #define API_SUFFIX(a) a##_64
#else
    #define API_SUFFIX(a) a
#endif
```

## Recommendations for Rust FFI

### 1. Type Mappings
```rust
// Basic types
type lapack_int = i32;  // or i64 for ILP64
type lapack_logical = lapack_int;

// Complex types (using num-complex crate)
type lapack_complex_float = Complex<f32>;
type lapack_complex_double = Complex<f64>;
```

### 2. Function Binding Pattern
```rust
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

### 3. Constants
```rust
pub const LAPACK_ROW_MAJOR: c_int = 101;
pub const LAPACK_COL_MAJOR: c_int = 102;
```

## Next Steps
1. Identify core routines for initial implementation
2. Design safe Rust wrappers around unsafe FFI calls
3. Handle error codes appropriately in Rust