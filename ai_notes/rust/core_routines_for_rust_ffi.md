# Core LAPACK Routines for Initial Rust FFI Bindings

## Overview
This document identifies and prioritizes the core LAPACK routines for initial Rust FFI implementation. These routines cover the most common linear algebra operations.

## Selection Criteria
1. **Frequency of use** - Commonly used in scientific computing
2. **Fundamental operations** - Building blocks for more complex algorithms
3. **Simple data types** - Start with real types before complex
4. **Well-documented** - Clear specifications and examples available

## Priority 1: Essential Linear System Solvers

### General Dense Linear Systems
- **`gesv`** - Solve AX = B for general matrices
  - `LAPACKE_sgesv` (single precision)
  - `LAPACKE_dgesv` (double precision)
  - Use case: Most common linear system solver

- **`getrf`** - LU factorization of general matrices
  - `LAPACKE_sgetrf` (single precision)
  - `LAPACKE_dgetrf` (double precision)
  - Use case: Preprocessing for solving multiple systems

- **`getrs`** - Solve using LU factorization
  - `LAPACKE_sgetrs` (single precision)
  - `LAPACKE_dgetrs` (double precision)
  - Use case: Efficient solving after factorization

### Positive Definite Linear Systems
- **`potrf`** - Cholesky factorization
  - `LAPACKE_spotrf` (single precision)
  - `LAPACKE_dpotrf` (double precision)
  - Use case: Symmetric positive definite matrices

- **`potrs`** - Solve using Cholesky factorization
  - `LAPACKE_spotrs` (single precision)
  - `LAPACKE_dpotrs` (double precision)
  - Use case: Efficient solving for SPD systems

## Priority 2: Least Squares Problems

- **`gels`** - Least squares solver using QR/LQ
  - `LAPACKE_sgels` (single precision)
  - `LAPACKE_dgels` (double precision)
  - Use case: Overdetermined/underdetermined systems

- **`gelsd`** - Least squares using SVD
  - `LAPACKE_sgelsd` (single precision)
  - `LAPACKE_dgelsd` (double precision)
  - Use case: Rank-deficient least squares

## Priority 3: Eigenvalue Problems

- **`syev`** - Symmetric eigenvalue decomposition
  - `LAPACKE_ssyev` (single precision)
  - `LAPACKE_dsyev` (double precision)
  - Use case: Real symmetric matrices

- **`geev`** - General eigenvalue decomposition
  - `LAPACKE_sgeev` (single precision)
  - `LAPACKE_dgeev` (double precision)
  - Use case: Non-symmetric matrices

## Priority 4: Singular Value Decomposition

- **`gesvd`** - General SVD
  - `LAPACKE_sgesvd` (single precision)
  - `LAPACKE_dgesvd` (double precision)
  - Use case: Matrix decomposition, rank determination

- **`gesdd`** - SVD using divide-and-conquer
  - `LAPACKE_sgesdd` (single precision)
  - `LAPACKE_dgesdd` (double precision)
  - Use case: Faster SVD for large matrices

## Implementation Roadmap

### Phase 1: Basic Infrastructure (Priority 1 routines)
1. **Double precision only** initially:
   - `LAPACKE_dgesv`
   - `LAPACKE_dgetrf` + `LAPACKE_dgetrs`
   - `LAPACKE_dpotrf` + `LAPACKE_dpotrs`

2. **Focus areas**:
   - Type definitions and constants
   - Error handling framework
   - Basic test infrastructure

### Phase 2: Extended Functionality
1. Add single precision variants
2. Add Priority 2 routines (least squares)
3. Implement work array management

### Phase 3: Advanced Features
1. Add Priority 3 & 4 routines
2. Complex number support
3. Performance optimizations

### Phase 4: Complete Coverage
1. Add remaining precisions (complex)
2. Advanced routine variants
3. Full test coverage

## Function Signatures for Initial Implementation

### Example: `dgesv` (Double precision general solver)
```c
lapack_int LAPACKE_dgesv(
    int matrix_layout,      // LAPACK_ROW_MAJOR or LAPACK_COL_MAJOR
    lapack_int n,          // Order of matrix A
    lapack_int nrhs,       // Number of right-hand sides
    double* a,             // Matrix A (n×n)
    lapack_int lda,        // Leading dimension of A
    lapack_int* ipiv,      // Pivot indices (output)
    double* b,             // Right-hand side matrix B (n×nrhs)
    lapack_int ldb         // Leading dimension of B
) -> lapack_int;           // Returns 0 on success, <0 for errors
```

## Rust FFI Considerations

### Safe Wrapper Design
```rust
pub fn dgesv(a: &mut Array2<f64>, b: &mut Array2<f64>) -> Result<Vec<i32>, LapackError> {
    // Validate inputs
    // Call unsafe LAPACKE_dgesv
    // Handle errors
    // Return pivot indices
}
```

### Memory Layout
- Support both row-major (Rust default) and column-major (Fortran default)
- Use `matrix_layout` parameter appropriately
- Consider performance implications

### Error Handling
- Map LAPACK error codes to Rust Result types
- Provide meaningful error messages
- Handle workspace allocation failures

## Testing Strategy

### Unit Tests
- Test each routine with known solutions
- Test error conditions (singular matrices, etc.)
- Test both row and column major layouts

### Integration Tests
- Solve real problems from numerical analysis
- Compare with reference implementations
- Performance benchmarks

### Property Tests
- Random matrix generation
- Verify mathematical properties
- Stress test with edge cases

## Next Steps
1. Implement FFI bindings for Priority 1 double precision routines
2. Create safe Rust wrappers with proper error handling
3. Develop comprehensive test suite
4. Document usage examples