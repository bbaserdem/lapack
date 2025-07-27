# Multi-Precision Sparse Matrix Support for LAPACK

## Overview

This implementation provides sparse matrix support for all LAPACK precision types:
- **S**: Single precision real (32-bit floating point)
- **D**: Double precision real (64-bit floating point)
- **C**: Single precision complex (2 × 32-bit floating point)
- **Z**: Double precision complex (2 × 64-bit floating point)

## File Structure

Each precision follows LAPACK's naming convention with the precision prefix:

```
/SRC/SPARSE/
├── MODULES/
│   ├── sparse_types.f90           # Original double precision types
│   ├── sparse_types_extended.f90  # All precision types
│   └── sparse_constants.f90       # Shared constants
├── COO/
│   ├── [SDCZ]COOINIT.f90         # Initialize COO matrices
│   ├── [SDCZ]COOCONV.f90         # COO conversions
│   ├── [SDCZ]COOMV.f90           # COO matrix-vector multiply
│   └── ...
├── CSR/
│   ├── [SDCZ]CSRINIT.f90         # Initialize CSR matrices
│   ├── [SDCZ]CSRMV.f90           # CSR matrix-vector multiply
│   ├── [SDCZ]CSRCSC.f90          # CSR-CSC multiplication
│   └── ...
├── CSC/
│   ├── [SDCZ]CSCINIT.f90         # Initialize CSC matrices
│   ├── [SDCZ]CSCMV.f90           # CSC matrix-vector multiply
│   └── ...
├── UTILS/
│   ├── [SDCZ]SPADD2.f90          # Sparse matrix addition
│   ├── [SDCZ]SPCOMP.f90          # Remove duplicates/zeros
│   ├── [SDCZ]SPSORT.f90          # Sort indices
│   └── ...
└── IO/
    ├── [SDCZ]SPREAD.f90          # Read sparse matrices
    └── [SDCZ]SPWRITE.f90         # Write sparse matrices
```

## Usage Examples

### Single Precision (S)
```fortran
USE sparse_types_extended
USE ISO_FORTRAN_ENV, ONLY: real32

TYPE(sparse_csr_s) :: A
REAL(real32) :: x(100), y(100)

! Initialize and use single precision sparse matrix
CALL SCSRINIT(row_ptr, col_ind, values, nrows, nnz, A)
CALL SCSRMV('N', nrows, ncols, 1.0_real32, A, x, 1, 0.0_real32, y, 1)
```

### Complex Precision (C)
```fortran
USE sparse_types_extended
USE ISO_FORTRAN_ENV, ONLY: real32

TYPE(sparse_coo_c) :: B
COMPLEX(real32) :: alpha = (1.0_real32, 0.0_real32)

! Complex matrices support conjugate transpose
CALL CCOOMV('C', m, n, alpha, B, x, 1, beta, y, 1)  ! 'C' = conjugate transpose
```

### Double Complex (Z)
```fortran
USE sparse_types_extended
USE ISO_FORTRAN_ENV, ONLY: real64

TYPE(sparse_csc_z) :: C
COMPLEX(real64) :: values(nnz)

! Initialize double complex sparse matrix
CALL ZCSCINIT(col_ptr, row_ind, values, ncols, nnz, C)
```

## Numerical Stability Considerations

### Single Precision (S)
- Limited to ~7 decimal digits of precision
- May experience round-off errors in ill-conditioned problems
- Suitable for graphics, machine learning, and applications with moderate accuracy requirements
- Use double precision for scientific computing requiring high accuracy

### Complex Types (C, Z)
- Complex arithmetic is approximately 4× more expensive than real
- Conjugate transpose ('C') is supported in addition to regular transpose ('T')
- Complex absolute value uses CABS (single) or ZABS (double complex)
- Be aware of branch cuts in complex functions

### General Guidelines
1. **Condition Number**: Monitor matrix condition numbers, especially in single precision
2. **Iterative Refinement**: Consider using mixed precision with iterative refinement
3. **Scaling**: Pre-scale matrices to avoid overflow/underflow
4. **Zero Detection**: Use appropriate tolerances for each precision:
   - Single: ~1e-6
   - Double: ~1e-15
   - Complex: Based on magnitude of both real and imaginary parts

## Compiling

Update your CMakeLists.txt to include the extended types module and all precision variants:

```cmake
set(SPARSE_MODULES
    MODULES/sparse_types.f90
    MODULES/sparse_types_extended.f90
    MODULES/sparse_constants.f90
)

# Include all precision variants
file(GLOB SPARSE_COO_ALL COO/[SDCZ]*.f90)
file(GLOB SPARSE_CSR_ALL CSR/[SDCZ]*.f90)
file(GLOB SPARSE_CSC_ALL CSC/[SDCZ]*.f90)
file(GLOB SPARSE_UTILS_ALL UTILS/[SDCZ]*.f90 UTILS/LSAME.f90)
file(GLOB SPARSE_IO_ALL IO/[SDCZ]*.f90)
```

## Testing

Run the multi-precision test suite:
```bash
./test_precision_variants
```

This verifies that all precision variants compile and function correctly.

## Performance Notes

- Single precision (S) operations are typically 2× faster than double (D)
- Complex operations (C/Z) are approximately 4× slower than their real counterparts
- Memory usage scales with precision:
  - S: 4 bytes per value
  - D: 8 bytes per value
  - C: 8 bytes per value
  - Z: 16 bytes per value

Choose precision based on your accuracy requirements and performance constraints.