!> \brief Extended sparse matrix type definitions for all precisions
!>
!> This module defines the fundamental sparse matrix types for LAPACK:
!> - COO (Coordinate List) format
!> - CSR (Compressed Sparse Row) format  
!> - CSC (Compressed Sparse Column) format
!>
!> Each type supports all LAPACK precisions:
!> - S: Single precision real
!> - D: Double precision real
!> - C: Single precision complex
!> - Z: Double precision complex

MODULE sparse_types_extended
    USE ISO_FORTRAN_ENV, ONLY: int32, real32, real64
    IMPLICIT NONE
    PRIVATE

    ! Export all types
    PUBLIC :: sparse_coo_s, sparse_csr_s, sparse_csc_s  ! Single precision
    PUBLIC :: sparse_coo_d, sparse_csr_d, sparse_csc_d  ! Double precision
    PUBLIC :: sparse_coo_c, sparse_csr_c, sparse_csc_c  ! Complex
    PUBLIC :: sparse_coo_z, sparse_csr_z, sparse_csc_z  ! Double complex
    
    !===========================================================================
    ! SINGLE PRECISION REAL TYPES
    !===========================================================================
    
    !> \brief COO sparse matrix type for single precision
    TYPE :: sparse_coo_s
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: row_ind(:)
        INTEGER(int32), ALLOCATABLE :: col_ind(:)
        REAL(real32), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
        LOGICAL :: checked = .FALSE.
    END TYPE sparse_coo_s

    !> \brief CSR sparse matrix type for single precision
    TYPE :: sparse_csr_s
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: row_ptr(:)
        INTEGER(int32), ALLOCATABLE :: col_ind(:)
        REAL(real32), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
    END TYPE sparse_csr_s

    !> \brief CSC sparse matrix type for single precision
    TYPE :: sparse_csc_s
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: col_ptr(:)
        INTEGER(int32), ALLOCATABLE :: row_ind(:)
        REAL(real32), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
    END TYPE sparse_csc_s
    
    !===========================================================================
    ! DOUBLE PRECISION REAL TYPES
    !===========================================================================
    
    !> \brief COO sparse matrix type for double precision
    TYPE :: sparse_coo_d
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: row_ind(:)
        INTEGER(int32), ALLOCATABLE :: col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
        LOGICAL :: checked = .FALSE.
    END TYPE sparse_coo_d

    !> \brief CSR sparse matrix type for double precision
    TYPE :: sparse_csr_d
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: row_ptr(:)
        INTEGER(int32), ALLOCATABLE :: col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
    END TYPE sparse_csr_d

    !> \brief CSC sparse matrix type for double precision
    TYPE :: sparse_csc_d
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: col_ptr(:)
        INTEGER(int32), ALLOCATABLE :: row_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
    END TYPE sparse_csc_d
    
    !===========================================================================
    ! SINGLE PRECISION COMPLEX TYPES
    !===========================================================================
    
    !> \brief COO sparse matrix type for single precision complex
    TYPE :: sparse_coo_c
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: row_ind(:)
        INTEGER(int32), ALLOCATABLE :: col_ind(:)
        COMPLEX(real32), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
        LOGICAL :: checked = .FALSE.
    END TYPE sparse_coo_c

    !> \brief CSR sparse matrix type for single precision complex
    TYPE :: sparse_csr_c
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: row_ptr(:)
        INTEGER(int32), ALLOCATABLE :: col_ind(:)
        COMPLEX(real32), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
    END TYPE sparse_csr_c

    !> \brief CSC sparse matrix type for single precision complex
    TYPE :: sparse_csc_c
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: col_ptr(:)
        INTEGER(int32), ALLOCATABLE :: row_ind(:)
        COMPLEX(real32), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
    END TYPE sparse_csc_c
    
    !===========================================================================
    ! DOUBLE PRECISION COMPLEX TYPES
    !===========================================================================
    
    !> \brief COO sparse matrix type for double precision complex
    TYPE :: sparse_coo_z
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: row_ind(:)
        INTEGER(int32), ALLOCATABLE :: col_ind(:)
        COMPLEX(real64), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
        LOGICAL :: checked = .FALSE.
    END TYPE sparse_coo_z

    !> \brief CSR sparse matrix type for double precision complex
    TYPE :: sparse_csr_z
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: row_ptr(:)
        INTEGER(int32), ALLOCATABLE :: col_ind(:)
        COMPLEX(real64), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
    END TYPE sparse_csr_z

    !> \brief CSC sparse matrix type for double precision complex
    TYPE :: sparse_csc_z
        INTEGER(int32) :: nrows = 0
        INTEGER(int32) :: ncols = 0
        INTEGER(int32) :: nnz = 0
        INTEGER(int32) :: nnz_alloc = 0
        
        INTEGER(int32), ALLOCATABLE :: col_ptr(:)
        INTEGER(int32), ALLOCATABLE :: row_ind(:)
        COMPLEX(real64), ALLOCATABLE :: values(:)
        
        LOGICAL :: sorted = .FALSE.
    END TYPE sparse_csc_z

END MODULE sparse_types_extended