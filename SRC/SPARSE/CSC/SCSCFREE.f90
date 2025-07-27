!> \brief \b SCSCFREE deallocates CSC sparse matrix memory
!>
!> \par Purpose:
!> =============
!>
!> SCSCFREE deallocates all dynamically allocated memory associated
!> with a CSC sparse matrix and resets the structure to an
!> uninitialized state.
!>
!> \param[inout] CSC
!>          CSC is TYPE(sparse_csc_s)
!>          On entry, the CSC sparse matrix to deallocate.
!>          On exit, all allocated arrays are deallocated and
!>          dimensions are reset to zero.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value

SUBROUTINE SCSCFREE(CSC, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_csc_s), INTENT(INOUT) :: CSC
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: ierr
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Deallocate column pointers
    IF (ALLOCATED(CSC%col_ptr)) THEN
        SEALLOCATE(CSC%col_ptr, STAT=ierr)
        IF (ierr /= 0) THEN
            INFO = SPARSE_ERR_ALLOC
            RETURN
        END IF
    END IF
    
    ! Deallocate row indices
    IF (ALLOCATED(CSC%row_ind)) THEN
        SEALLOCATE(CSC%row_ind, STAT=ierr)
        IF (ierr /= 0) THEN
            INFO = SPARSE_ERR_ALLOC
            RETURN
        END IF
    END IF
    
    ! Deallocate values
    IF (ALLOCATED(CSC%values)) THEN
        SEALLOCATE(CSC%values, STAT=ierr)
        IF (ierr /= 0) THEN
            INFO = SPARSE_ERR_ALLOC
            RETURN
        END IF
    END IF
    
    ! Reset dimensions and counts
    CSC%nrows = 0
    CSC%ncols = 0
    CSC%nnz = 0
    CSC%nnz_alloc = 0
    CSC%sorted = .FALSE.
    
END SUBROUTINE SCSCFREE