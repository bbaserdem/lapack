!> \brief Sparse matrix constants and parameters
!>
!> This module defines constants used throughout the sparse matrix implementation

MODULE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64
    IMPLICIT NONE
    PUBLIC

    ! Error codes for sparse routines
    INTEGER(int32), PARAMETER :: SPARSE_SUCCESS = 0
    INTEGER(int32), PARAMETER :: SPARSE_ERR_ALLOC = -1
    INTEGER(int32), PARAMETER :: SPARSE_ERR_DIM = -2
    INTEGER(int32), PARAMETER :: SPARSE_ERR_NNZ = -3
    INTEGER(int32), PARAMETER :: SPARSE_ERR_FORMAT = -4
    INTEGER(int32), PARAMETER :: SPARSE_ERR_INVALID = -5
    INTEGER(int32), PARAMETER :: SPARSE_ERR_NOMEM = -6
    INTEGER(int32), PARAMETER :: SPARSE_ERR_IO = -7

    ! Matrix property flags
    INTEGER(int32), PARAMETER :: SPARSE_PROP_GENERAL = 0
    INTEGER(int32), PARAMETER :: SPARSE_PROP_SYMMETRIC = 1
    INTEGER(int32), PARAMETER :: SPARSE_PROP_HERMITIAN = 2
    INTEGER(int32), PARAMETER :: SPARSE_PROP_TRIANGULAR = 4
    INTEGER(int32), PARAMETER :: SPARSE_PROP_DIAGONAL = 8

    ! Matrix norm types
    CHARACTER(1), PARAMETER :: SPARSE_NORM_ONE = '1'      ! 1-norm (max column sum)
    CHARACTER(1), PARAMETER :: SPARSE_NORM_INF = 'I'      ! Infinity norm (max row sum)
    CHARACTER(1), PARAMETER :: SPARSE_NORM_FRO = 'F'      ! Frobenius norm

    ! Default allocation parameters
    INTEGER(int32), PARAMETER :: SPARSE_INIT_SIZE = 16    ! Initial allocation size
    REAL(real64), PARAMETER :: SPARSE_GROWTH_FACTOR = 1.5_real64  ! Growth factor for reallocation

    ! Tolerance for zero detection
    REAL(real64), PARAMETER :: SPARSE_ZERO_TOL = EPSILON(1.0_real64) * 100.0_real64

    ! File format identifiers
    INTEGER(int32), PARAMETER :: SPARSE_FMT_MM = 1        ! Matrix Market format
    INTEGER(int32), PARAMETER :: SPARSE_FMT_COO = 2       ! Simple COO text format
    INTEGER(int32), PARAMETER :: SPARSE_FMT_HB = 3        ! Harwell-Boeing format

END MODULE sparse_constants