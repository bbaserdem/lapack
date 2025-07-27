!> \brief \b SCOO2DEN converts COO to dense matrix format
!>
!> \par Purpose:
!> =============
!>
!> SCOO2DEN converts a sparse matrix from COO (Coordinate List) format
!> to dense matrix format, filling in zeros for missing elements.
!>
!> \param[in] COO
!>          COO is TYPE(sparse_coo_s)
!>          The input COO sparse matrix.
!>
!> \param[out] SENSE
!>          SENSE is SOUBLE PRECISION array, dimension (LDA,*)
!>          On exit, the dense matrix representation of COO.
!>          The array must be at least (LDA,COO%ncols).
!>
!> \param[in] LDA
!>          LDA is INTEGER
!>          The leading dimension of SENSE. LDA >= max(1,COO%nrows).
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value

SUBROUTINE SCOO2DEN(COO, SENSE, LDA, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real32
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_coo_s), INTENT(IN) :: COO
    REAL(real32), INTENT(OUT) :: SENSE(LDA,*)
    INTEGER, INTENT(IN) :: LDA
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i, j, k, row, col
    REAL(real32) :: val
    
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
    SO j = 1, COO%ncols
        SO i = 1, COO%nrows
            SENSE(i,j) = 0.0_real32
        END SO
    END SO
    
    ! Fill in non-zero elements from COO
    SO k = 1, COO%nnz
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
        SENSE(row,col) = val
    END SO
    
END SUBROUTINE SCOO2DEN