!> \brief \b DDEN2COO converts dense matrix to COO format
!>
!> \par Purpose:
!> =============
!>
!> DDEN2COO converts a dense matrix to COO (Coordinate List) sparse format,
!> skipping zero elements according to the tolerance SPARSE_ZERO_TOL.
!>
!> \param[in] DENSE
!>          DENSE is COMPLEX*16 array, dimension (LDA,N)
!>          The input dense matrix.
!>
!> \param[in] LDA
!>          LDA is INTEGER
!>          The leading dimension of DENSE. LDA >= max(1,M).
!>
!> \param[in] M
!>          M is INTEGER
!>          The number of rows of the matrix. M >= 0.
!>
!> \param[in] N
!>          N is INTEGER
!>          The number of columns of the matrix. N >= 0.
!>
!> \param[out] COO
!>          COO is TYPE(sparse_coo_z)
!>          On exit, the COO sparse matrix containing non-zero elements from DENSE.
!>          Must be pre-allocated with sufficient space.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_ALLOC: insufficient allocated space in COO

SUBROUTINE ZDEN2COO(DENSE, LDA, M, N, COO, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64
    IMPLICIT NONE
    
    ! Arguments
    COMPLEX(real64), INTENT(IN) :: DENSE(LDA,*)
    INTEGER, INTENT(IN) :: LDA, M, N
    TYPE(sparse_coo_z), INTENT(INOUT) :: COO
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i, j, nnz_count
    COMPLEX(real64) :: val
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check arguments
    IF (M < 0) THEN
        INFO = -3
        RETURN
    END IF
    
    IF (N < 0) THEN
        INFO = -4
        RETURN
    END IF
    
    IF (LDA < MAX(1, M)) THEN
        INFO = -2
        RETURN
    END IF
    
    ! Check if COO is allocated
    IF (.NOT. ALLOCATED(COO%row_ind)) THEN
        INFO = -5
        RETURN
    END IF
    
    ! Set dimensions
    COO%nrows = M
    COO%ncols = N
    
    ! First pass: count non-zeros
    nnz_count = 0
    DO j = 1, N
        DO i = 1, M
            val = DENSE(i,j)
            IF (ZABS(val) > SPARSE_ZERO_TOL) THEN
                nnz_count = nnz_count + 1
            END IF
        END DO
    END DO
    
    ! Check if we have enough space
    IF (nnz_count > COO%nnz_alloc) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Set the number of non-zeros
    COO%nnz = nnz_count
    
    ! Special case: all zeros
    IF (nnz_count == 0) THEN
        RETURN
    END IF
    
    ! Second pass: store non-zeros in COO format
    nnz_count = 0
    DO j = 1, N
        DO i = 1, M
            val = DENSE(i,j)
            IF (ZABS(val) > SPARSE_ZERO_TOL) THEN
                nnz_count = nnz_count + 1
                COO%row_ind(nnz_count) = i
                COO%col_ind(nnz_count) = j
                COO%values(nnz_count) = val
            END IF
        END DO
    END DO
    
    ! Mark as sorted (we traverse in column-major order)
    COO%sorted = .FALSE.  ! Not sorted by rows
    COO%checked = .TRUE.  ! No duplicates or zeros
    
END SUBROUTINE ZDEN2COO