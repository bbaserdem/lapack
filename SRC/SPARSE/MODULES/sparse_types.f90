!> \brief Sparse matrix type definitions
!>
!> This module defines the fundamental sparse matrix types for LAPACK:
!> - COO (Coordinate List) format
!> - CSR (Compressed Sparse Row) format  
!> - CSC (Compressed Sparse Column) format
!>
!> Each type supports double precision (D prefix) following LAPACK conventions.
!> Extension to other precisions (S/C/Z) will follow the same pattern.

MODULE sparse_types
    USE ISO_FORTRAN_ENV, ONLY: int32, real64
    IMPLICIT NONE
    PRIVATE

    ! Export types
    PUBLIC :: sparse_coo_d, sparse_csr_d, sparse_csc_d
    
    !> \brief COO (Coordinate List) sparse matrix type for double precision
    !>
    !> The COO format stores a list of (row, column, value) tuples.
    !> This is the most general sparse format and easiest to construct.
    TYPE :: sparse_coo_d
        INTEGER(int32) :: nrows = 0     !< Number of rows
        INTEGER(int32) :: ncols = 0     !< Number of columns  
        INTEGER(int32) :: nnz = 0       !< Number of non-zero elements
        INTEGER(int32) :: nnz_alloc = 0 !< Allocated size for arrays
        
        INTEGER(int32), ALLOCATABLE :: row_ind(:)  !< Row indices (1-based)
        INTEGER(int32), ALLOCATABLE :: col_ind(:)  !< Column indices (1-based)
        REAL(real64), ALLOCATABLE :: values(:)     !< Non-zero values
        
        LOGICAL :: sorted = .FALSE.     !< Whether indices are sorted
        LOGICAL :: checked = .FALSE.    !< Whether duplicates/zeros removed
    END TYPE sparse_coo_d

    !> \brief CSR (Compressed Sparse Row) sparse matrix type for double precision
    !>
    !> The CSR format compresses row information using row pointers.
    !> Efficient for row-wise operations and matrix-vector multiplication.
    TYPE :: sparse_csr_d
        INTEGER(int32) :: nrows = 0     !< Number of rows
        INTEGER(int32) :: ncols = 0     !< Number of columns
        INTEGER(int32) :: nnz = 0       !< Number of non-zero elements
        INTEGER(int32) :: nnz_alloc = 0 !< Allocated size for col_ind/values
        
        INTEGER(int32), ALLOCATABLE :: row_ptr(:)  !< Row pointers (size nrows+1)
        INTEGER(int32), ALLOCATABLE :: col_ind(:)  !< Column indices (1-based)
        REAL(real64), ALLOCATABLE :: values(:)     !< Non-zero values
        
        LOGICAL :: sorted = .FALSE.     !< Whether columns within rows are sorted
    END TYPE sparse_csr_d

    !> \brief CSC (Compressed Sparse Column) sparse matrix type for double precision
    !>
    !> The CSC format compresses column information using column pointers.
    !> Efficient for column-wise operations and compatibility with column-major storage.
    TYPE :: sparse_csc_d
        INTEGER(int32) :: nrows = 0     !< Number of rows
        INTEGER(int32) :: ncols = 0     !< Number of columns
        INTEGER(int32) :: nnz = 0       !< Number of non-zero elements
        INTEGER(int32) :: nnz_alloc = 0 !< Allocated size for row_ind/values
        
        INTEGER(int32), ALLOCATABLE :: col_ptr(:)  !< Column pointers (size ncols+1)
        INTEGER(int32), ALLOCATABLE :: row_ind(:)  !< Row indices (1-based)
        REAL(real64), ALLOCATABLE :: values(:)     !< Non-zero values
        
        LOGICAL :: sorted = .FALSE.     !< Whether rows within columns are sorted
    END TYPE sparse_csc_d

END MODULE sparse_types