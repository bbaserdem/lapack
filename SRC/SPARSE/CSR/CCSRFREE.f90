!> \brief \b CCSRFREE deallocates CSR sparse matrix memory
!>
!> \par Purpose:
!> =============
!>
!> CCSRFREE deallocates all dynamically allocated memory associated
!> with a CSR sparse matrix and resets the structure to an
!> uninitialized state.
!>
!> \param[inout] CSR
!>          CSR is TYPE(sparse_csr_c)
!>          On entry, the CSR sparse matrix to deallocate.
!>          On exit, all allocated arrays are deallocated and
!>          dimensions are reset to zero.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value

SUBROUTINE CCSRFREE(CSR, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_csr_c), INTENT(INOUT) :: CSR
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: ierr
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Deallocate row pointers
    IF (ALLOCATED(CSR%row_ptr)) THEN
        CEALLOCATE(CSR%row_ptr, STAT=ierr)
        IF (ierr /= 0) THEN
            INFO = SPARSE_ERR_ALLOC
            RETURN
        END IF
    END IF
    
    ! Deallocate column indices
    IF (ALLOCATED(CSR%col_ind)) THEN
        CEALLOCATE(CSR%col_ind, STAT=ierr)
        IF (ierr /= 0) THEN
            INFO = SPARSE_ERR_ALLOC
            RETURN
        END IF
    END IF
    
    ! Deallocate values
    IF (ALLOCATED(CSR%values)) THEN
        CEALLOCATE(CSR%values, STAT=ierr)
        IF (ierr /= 0) THEN
            INFO = SPARSE_ERR_ALLOC
            RETURN
        END IF
    END IF
    
    ! Reset dimensions and counts
    CSR%nrows = 0
    CSR%ncols = 0
    CSR%nnz = 0
    CSR%nnz_alloc = 0
    CSR%sorted = .FALSE.
    
END SUBROUTINE CCSRFREE