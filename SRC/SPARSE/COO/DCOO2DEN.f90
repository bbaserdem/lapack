!> \brief \b DCOO2DEN converts COO to dense matrix format
!>
!> \par Purpose:
!> =============
!>
!> DCOO2DEN converts a sparse matrix from COO (Coordinate List) format
!> to dense matrix format, filling in zeros for missing elements.
!>
!> \param[in] COO
!>          COO is TYPE(sparse_coo_d)
!>          The input COO sparse matrix.
!>
!> \param[out] DENSE
!>          DENSE is DOUBLE PRECISION array, dimension (LDA,*)
!>          On exit, the dense matrix representation of COO.
!>          The array must be at least (LDA,COO%ncols).
!>
!> \param[in] LDA
!>          LDA is INTEGER
!>          The leading dimension of DENSE. LDA >= max(1,COO%nrows).
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value

SUBROUTINE DCOO2DEN(COO, DENSE, LDA, INFO)
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_coo_d), INTENT(IN) :: COO
    REAL(real64), INTENT(OUT) :: DENSE(LDA,*)
    INTEGER, INTENT(IN) :: LDA
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i, j, k, row, col
    REAL(real64) :: val
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check if COO is valid
    IF (.NOT. ALLOCATED(COO%row_ind)) THEN
        INFO = -1
        RETURN
    END IF
    
    ! Check LDA
    IF (LDA < MAX(1, COO%nrows)) THEN
        INFO = -3
        RETURN
    END IF
    
    ! Initialize dense matrix to zero
    DO j = 1, COO%ncols
        DO i = 1, COO%nrows
            DENSE(i,j) = 0.0_real64
        END DO
    END DO
    
    ! Fill in non-zero elements from COO
    DO k = 1, COO%nnz
        row = COO%row_ind(k)
        col = COO%col_ind(k)
        val = COO%values(k)
        
        ! Check bounds
        IF (row < 1 .OR. row > COO%nrows .OR. &
            col < 1 .OR. col > COO%ncols) THEN
            INFO = SPARSE_ERR_INVALID
            RETURN
        END IF
        
        ! Note: This will overwrite if there are duplicates
        ! For duplicates, the last value wins
        DENSE(row,col) = val
    END DO
    
END SUBROUTINE DCOO2DEN