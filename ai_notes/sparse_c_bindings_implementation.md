# LAPACKE Sparse Matrix C Bindings Implementation

## Summary
Successfully implemented C bindings for sparse matrices in LAPACKE following standard LAPACK conventions.

## Files Created:
- **Header**: `LAPACKE/include/lapacke_sparse.h` (function prototypes and constants)
- **Source**: `LAPACKE/src/lapacke_dcooinit.c` (COO matrix initialization)
- **Source**: `LAPACKE/src/lapacke_dcoomv.c` (COO matrix-vector multiplication)
- **Source**: `LAPACKE/src/lapacke_dcoofree.c` (COO memory cleanup)
- **Source**: `LAPACKE/src/lapacke_dcoo2den.c` (COO to dense conversion)
- **Example**: `LAPACKE/example/example_dcoo_sparse.c` (usage demonstration)

## Functions Implemented:
- COO matrix initialization from arrays
- COO matrix-vector multiplication (including transpose)
- COO memory management (allocation/deallocation)
- COO to dense matrix conversion
- Input validation and error handling

## Build Integration:
- Updated `LAPACKE/include/lapacke.h` to include sparse header
- Modified `LAPACKE/src/CMakeLists.txt` to include sparse sources

## Key Features:
- Follows LAPACKE conventions (error codes, parameter validation)
- Supports both column-major and row-major layouts where applicable
- Memory management with proper cleanup
- 1-based to 0-based index conversion for C compatibility
- Comprehensive error checking

## Function Details:

### `LAPACKE_dcooinit`
```c
lapack_int LAPACKE_dcooinit(lapack_int nrows, lapack_int ncols, lapack_int nnz,
                           const lapack_int* row_ind, const lapack_int* col_ind,
                           const double* values, double** coo_values, 
                           lapack_int** coo_row_ind, lapack_int** coo_col_ind);
```
Initializes a COO sparse matrix from input arrays with memory allocation and validation.

### `LAPACKE_dcoomv`
```c
lapack_int LAPACKE_dcoomv(char trans, lapack_int m, lapack_int n, double alpha,
                         lapack_int nnz, const double* values,
                         const lapack_int* row_ind, const lapack_int* col_ind,
                         const double* x, lapack_int incx, double beta,
                         double* y, lapack_int incy);
```
Performs sparse matrix-vector multiplication: `y := alpha*A*x + beta*y` or `y := alpha*A^T*x + beta*y`.

### `LAPACKE_dcoo2den`
```c
lapack_int LAPACKE_dcoo2den(int matrix_layout, lapack_int m, lapack_int n,
                           lapack_int nnz, const double* values,
                           const lapack_int* row_ind, const lapack_int* col_ind,
                           double* a, lapack_int lda);
```
Converts COO sparse matrix to dense format with support for both row and column major layouts.

### `LAPACKE_dcoofree`
```c
lapack_int LAPACKE_dcoofree(double* values, lapack_int* row_ind, lapack_int* col_ind);
```
Frees memory allocated for COO sparse matrix arrays.

## Usage Example:
The example demonstrates:
1. COO matrix initialization
2. Conversion to dense format for verification
3. Matrix-vector multiplication (normal and transpose)
4. Proper memory cleanup

## Future Extensions:
- Additional precision variants (S, C, Z)
- CSR and CSC format bindings
- Additional sparse operations (conversion between formats)
- Advanced sparse solvers interface

Second best suggestion would have been to implement all precision variants (S/C/Z) simultaneously, but focusing on double precision first provides a solid foundation.