# Sparse Matrix

Sparse matrices will need the following implementations.

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

- `function alloc_coo(nrows, ncols, nnz) result(coo)`
- `function alloc_csr(nrows, ncols, nnz) result(coo)`
- `function alloc_csc(nrows, ncols, nnz) result(coo)`

- `function init_coo(rowind, colind, val) result(coo)`
- `function init_csr(rowind, colind, val) result(coo)`
- `function init_csc(rowind, colind, val) result(coo)`

#### Type Conversions

- `function convert_coo_to_csr(coo) result(csr)`
- `function convert_csr_to_coo(csr) result(coo)`
- `function convert_coo_to_csc(coo) result(csc)`
- `function convert_csc_to_coo(csc) result(coo)`

- `function convert_dense_to_coo(dense) result(coo)`
- `function convert_coo_to_dense(coo) result(dense)`

#### Multiplication

- `function multiply_dense_csc(dense, csc) result(out)`
- `function multiply_csr_dense(csr, dense) result(out)`
- `function multiply_csr_csc(csr, csc) result(out)`
- `function multiply_vec_csc(vec, csc) result(out)`
- `function multiply_csr_vec(csr, vec) result(out)`

#### Transposition

- `function transpose_csr_to_csc (csr) resuLt(csc)`
- `function transpose_csc_to_csr (csc) result(csr)`
- `subroutine transpose_coo (coo)`

#### Element Access and Modification

- `function get_element(sparse, i, j) result(value)`
- `subroutine set_element(sparse, i, j, value)`
- `subroutine add_element(sparse, i, j, value)`

#### Basic Matrix Operations

- `function add_sparse(A, B) result(C)` - Addition of same-format matrices
- `subroutine scale_sparse(alpha, A)` - In-place scaling
- `function norm_sparse(A, norm_type) result(norm)` - Matrix norms (1, inf, Frobenius)
- `function get_diagonal(sparse) result(diag)` - Extract diagonal as vector

#### Matrix Properties and Analysis

- `function is_symmetric(sparse) result(logical)`
- `function is_positive_definite(sparse) result(logical)`
- `function bandwidth(sparse) result(bw)`
- `function nnz(sparse) result(count)` - Number of non-zeros
- `function sparsity_pattern(sparse) result(pattern)` - For visualization/analysis

#### Memory Management

- `subroutine resize_sparse(sparse, new_nnz)` - Dynamic resizing
- `subroutine compress_sparse(sparse)` - Remove duplicates/zeros
- `subroutine sort_indices(sparse)` - Ensure sorted indices

#### I/O Operations

- `function read_sparse(filename, format) result(sparse)` - Matrix Market, Harwell-Boeing formats
- `subroutine write_sparse(sparse, filename, format)`

### Auxillary Routines

#### Conversions

- `function convert_csr_to_csc(csr) result(csc)`
- `function convert_csc_to_csr(csc) result(csr)`

- `function convert_dense_to_csr(dense) result(csr)`
- `function convert_dense_to_csc(dense) result(csc)`
- `function convert_csr_to_dense(csr) result(dense)`
- `function convert_csc_to_dense(csc) result(dense)`

#### Multiplication

- `multiply_dense_csr(dense, csr)`
- `multiply_dense_coo(dense, coo)`
- `multiply_dense_vec(dense, vec)`
- `multiply_dense_dense(dense1, dense2)`

- `multiply_vec_dense(vec, dense)`
- `multiply_vec_csr(vec, csr)`
- `multiply_vec_coo(vec, coo)`
- `multiply_vec_vec(vec1, vec2)`

- `multiply_coo_dense(coo, dense)`
- `multiply_coo_csr(coo, csr)`
- `multiply_coo_csc(coo, csc)`
- `multiply_coo_vec(coo, vec)`

- `multiply_csr_csr(csr1, csr2)`
- `multiply_csr_coo(csr, coo)`
- `multiply_csc_coo(csc, coo)`
- `multiply_csc_csr(csc, csr)`
