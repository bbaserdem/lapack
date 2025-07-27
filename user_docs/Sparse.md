# Sparse Matrix

Sparse matrices will need the following implementations.

## Implementation Strategy

### Data Type Approach
- Start with double precision (D) implementation first
- Follow LAPACK's prefix-based naming convention (S/D/C/Z)
- Each precision gets separate implementations (no polymorphism)
- Use modern Fortran modules for type definitions and constants

### File Structure and Organization

```
/SRC/SPARSE/
├── MODULES/
│   ├── sparse_types.f90      # Type definitions for COO, CSR, CSC
│   ├── sparse_constants.f90  # Sparse-specific constants
│   └── sparse_utils.f90      # Common utilities
├── COO/
│   ├── DCOOINIT.f90          # Double precision COO initialization
│   ├── DCOOCONV.f90          # COO conversions
│   └── ...
├── CSR/
│   ├── DCSRINIT.f90          # Double precision CSR initialization
│   ├── DCSRMV.f90            # CSR matrix-vector multiply
│   └── ...
├── CSC/
│   ├── DCSCINIT.f90          # Double precision CSC initialization
│   ├── DCSCMM.f90            # CSC matrix-matrix multiply
│   └── ...
└── IO/
    ├── DSPREAD.f90           # Read sparse matrices
    └── DSPWRITE.f90          # Write sparse matrices
```

### Naming Convention
Following LAPACK's pattern:
- Precision prefix: S (single), D (double), C (complex), Z (double complex)
- Format identifier: COO, CSR, CSC
- Operation suffix: INIT, CONV, MV, MM, etc.

Examples:
- `DCOOINIT` - Double precision COO initialization
- `DCSR2CSC` - Double CSR to CSC conversion
- `DCSRMV` - Double CSR matrix-vector multiply

### Implementation Order
1. Start with core types module defining sparse matrix structures
2. Implement COO format first (simplest and most general)
3. Add conversions between formats
4. Implement basic operations (matrix-vector multiply first)
5. Extend to other precisions once double precision is stable

## Data Types

- `coo`: Coordinate list sparse matrix.
- `csr`: Compressed sparse row matrix.
- `csc`: Compressed sparse column matrix.

## Routines

We will divide the routines into two;

- main: The needs to be implemented types
- auxillary: They are doable through composition of main ones, and faster to do so.

### Main Routines

#### Initialization

- `SUBROUTINE DCOOALLOC(NROWS, NCOLS, NNZ, COO)` - Allocate COO matrix
- `SUBROUTINE DCSRALLOC(NROWS, NCOLS, NNZ, CSR)` - Allocate CSR matrix
- `SUBROUTINE DCSCALLOC(NROWS, NCOLS, NNZ, CSC)` - Allocate CSC matrix

- `SUBROUTINE DCOOINIT(ROWIND, COLIND, VAL, NNZ, COO)` - Initialize COO from arrays
- `SUBROUTINE DCSRINIT(ROWPTR, COLIND, VAL, NROWS, NNZ, CSR)` - Initialize CSR from arrays
- `SUBROUTINE DCSCINIT(COLPTR, ROWIND, VAL, NCOLS, NNZ, CSC)` - Initialize CSC from arrays

#### Type Conversions

- `SUBROUTINE DCOO2CSR(COO, CSR)` - Convert COO to CSR
- `SUBROUTINE DCSR2COO(CSR, COO)` - Convert CSR to COO
- `SUBROUTINE DCOO2CSC(COO, CSC)` - Convert COO to CSC
- `SUBROUTINE DCSC2COO(CSC, COO)` - Convert CSC to COO

- `SUBROUTINE DDEN2COO(DENSE, LDA, M, N, COO)` - Convert dense to COO
- `SUBROUTINE DCOO2DEN(COO, DENSE, LDA)` - Convert COO to dense

#### Multiplication

- `SUBROUTINE DGECSC(TRANS, M, N, K, ALPHA, A, LDA, CSC, BETA, C, LDC)` - Dense-CSC multiply
- `SUBROUTINE DCSRGE(TRANS, M, N, K, ALPHA, CSR, B, LDB, BETA, C, LDC)` - CSR-Dense multiply
- `SUBROUTINE DCSRCSC(TRANS, M, N, K, ALPHA, CSR, CSC, BETA, C)` - CSR-CSC multiply
- `SUBROUTINE DCSRMV(TRANS, M, N, ALPHA, CSR, X, INCX, BETA, Y, INCY)` - CSR matrix-vector multiply
- `SUBROUTINE DCSCMV(TRANS, M, N, ALPHA, CSC, X, INCX, BETA, Y, INCY)` - CSC matrix-vector multiply

#### Transposition

- `SUBROUTINE DCSRTRANS(CSR, CSC)` - Transpose CSR to CSC
- `SUBROUTINE DCSCTRANS(CSC, CSR)` - Transpose CSC to CSR
- `SUBROUTINE DCOOTRANS(COO)` - In-place transpose of COO

#### Element Access and Modification

- `DOUBLE PRECISION FUNCTION DSPGET(SPARSE, I, J)` - Get element value
- `SUBROUTINE DSPSET(SPARSE, I, J, VALUE)` - Set element value
- `SUBROUTINE DSPADD(SPARSE, I, J, VALUE)` - Add to element value

#### Basic Matrix Operations

- `SUBROUTINE DSPADD2(A, B, C)` - Addition of same-format sparse matrices
- `SUBROUTINE DSPSCAL(ALPHA, SPARSE)` - In-place scaling
- `DOUBLE PRECISION FUNCTION DSPNRM(SPARSE, NORM)` - Matrix norms (1, inf, Frobenius)
- `SUBROUTINE DSPDIAG(SPARSE, DIAG)` - Extract diagonal as vector

#### Matrix Properties and Analysis

- `LOGICAL FUNCTION DSPSYM(SPARSE)` - Check if symmetric
- `LOGICAL FUNCTION DSPPDEF(SPARSE)` - Check if positive definite
- `INTEGER FUNCTION DSPBW(SPARSE)` - Compute bandwidth
- `INTEGER FUNCTION DSPNNZ(SPARSE)` - Number of non-zeros
- `SUBROUTINE DSPPATTERN(SPARSE, PATTERN)` - Extract sparsity pattern

#### Memory Management

- `SUBROUTINE DSPSIZE(SPARSE, NEW_NNZ)` - Dynamic resizing
- `SUBROUTINE DSPCOMP(SPARSE)` - Remove duplicates/zeros
- `SUBROUTINE DSPSORT(SPARSE)` - Ensure sorted indices

#### I/O Operations

- `SUBROUTINE DSPREAD(FILENAME, FORMAT, SPARSE, INFO)` - Read sparse matrix
- `SUBROUTINE DSPWRITE(SPARSE, FILENAME, FORMAT, INFO)` - Write sparse matrix

### Auxiliary Routines

These routines can be implemented through composition of main routines:

#### Conversions

- `SUBROUTINE DCSR2CSC(CSR, CSC)` - Convert CSR to CSC (via COO)
- `SUBROUTINE DCSC2CSR(CSC, CSR)` - Convert CSC to CSR (via COO)

- `SUBROUTINE DDEN2CSR(DENSE, LDA, M, N, CSR)` - Convert dense to CSR (via COO)
- `SUBROUTINE DDEN2CSC(DENSE, LDA, M, N, CSC)` - Convert dense to CSC (via COO)
- `SUBROUTINE DCSR2DEN(CSR, DENSE, LDA)` - Convert CSR to dense (via COO)
- `SUBROUTINE DCSC2DEN(CSC, DENSE, LDA)` - Convert CSC to dense (via COO)

#### Multiplication

- `SUBROUTINE DGECSR(TRANS, M, N, K, ALPHA, A, LDA, CSR, BETA, C, LDC)` - Dense-CSR multiply
- `SUBROUTINE DGECOO(TRANS, M, N, K, ALPHA, A, LDA, COO, BETA, C, LDC)` - Dense-COO multiply
- `SUBROUTINE DGEMV` - Dense matrix-vector (already in BLAS)
- `SUBROUTINE DGEMM` - Dense matrix-matrix (already in BLAS)

- `SUBROUTINE DGER` - Vector outer product (already in BLAS)
- `SUBROUTINE DCOOMV(TRANS, M, N, ALPHA, COO, X, INCX, BETA, Y, INCY)` - COO matrix-vector multiply
- `DDOT` - Vector dot product (already in BLAS)

- `SUBROUTINE DCOOGE(TRANS, M, N, K, ALPHA, COO, B, LDB, BETA, C, LDC)` - COO-Dense multiply
- `SUBROUTINE DCOOCSR(TRANS, M, N, K, ALPHA, COO, CSR, BETA, C)` - COO-CSR multiply
- `SUBROUTINE DCOOCSC(TRANS, M, N, K, ALPHA, COO, CSC, BETA, C)` - COO-CSC multiply

- `SUBROUTINE DCSRCSR(TRANS, M, N, K, ALPHA, A, B, BETA, C)` - CSR-CSR multiply
- `SUBROUTINE DCSRCOO(TRANS, M, N, K, ALPHA, CSR, COO, BETA, C)` - CSR-COO multiply
- `SUBROUTINE DCSCCOO(TRANS, M, N, K, ALPHA, CSC, COO, BETA, C)` - CSC-COO multiply
- `SUBROUTINE DCSCCSR(TRANS, M, N, K, ALPHA, CSC, CSR, BETA, C)` - CSC-CSR multiply
