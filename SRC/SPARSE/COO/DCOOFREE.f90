!> \brief \b DCOOFREE deallocates COO sparse matrix memory
!>
!> \par Purpose:
!> =============
!>
!> DCOOFREE deallocates all dynamically allocated memory associated
!> with a COO sparse matrix and resets the structure to an
!> uninitialized state.
!>
!> \param[inout] COO
!>          COO is TYPE(sparse_coo_d)
!>          On entry, the COO sparse matrix to deallocate.
!>          On exit, all allocated arrays are deallocated and
!>          dimensions are reset to zero.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value

SUBROUTINE DCOOFREE(COO, INFO)
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_coo_d), INTENT(INOUT) :: COO
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: ierr
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Deallocate row indices
    IF (ALLOCATED(COO%row_ind)) THEN
        DEALLOCATE(COO%row_ind, STAT=ierr)
        IF (ierr /= 0) THEN
            INFO = SPARSE_ERR_ALLOC
            RETURN
        END IF
    END IF
    
    ! Deallocate column indices
    IF (ALLOCATED(COO%col_ind)) THEN
        DEALLOCATE(COO%col_ind, STAT=ierr)
        IF (ierr /= 0) THEN
            INFO = SPARSE_ERR_ALLOC
            RETURN
        END IF
    END IF
    
    ! Deallocate values
    IF (ALLOCATED(COO%values)) THEN
        DEALLOCATE(COO%values, STAT=ierr)
        IF (ierr /= 0) THEN
            INFO = SPARSE_ERR_ALLOC
            RETURN
        END IF
    END IF
    
    ! Reset dimensions and counts
    COO%nrows = 0
    COO%ncols = 0
    COO%nnz = 0
    COO%nnz_alloc = 0
    COO%sorted = .FALSE.
    
END SUBROUTINE DCOOFREE