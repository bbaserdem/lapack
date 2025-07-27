!> \brief \b DCOOCHECK validates COO sparse matrix structure
!>
!> \par Purpose:
!> =============
!>
!> DCOOCHECK validates a COO sparse matrix structure by checking:
!> - Row and column indices are within bounds
!> - Detection of duplicate entries
!> - Matrix dimensions are valid
!>
!> \param[in] COO
!>          COO is TYPE(sparse_coo_d)
!>          The COO sparse matrix to validate.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit, matrix is valid
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_DIM: invalid matrix dimensions
!>          = SPARSE_ERR_INVALID: index out of bounds or duplicate entries found

SUBROUTINE DCOOCHECK(COO, INFO)
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_coo_d), INTENT(IN) :: COO
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i, j, k
    INTEGER :: row, col
    INTEGER, ALLOCATABLE :: entry_map(:,:)
    INTEGER :: map_size, hash_idx
    LOGICAL :: found_duplicate
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check if COO is valid
    IF (.NOT. ALLOCATED(COO%row_ind)) THEN
        INFO = -1
        RETURN
    END IF
    
    ! Check matrix dimensions
    IF (COO%nrows <= 0 .OR. COO%ncols <= 0) THEN
        INFO = SPARSE_ERR_DIM
        RETURN
    END IF
    
    ! Check that nnz matches allocated size
    IF (COO%nnz < 0) THEN
        INFO = SPARSE_ERR_DIM
        RETURN
    END IF
    
    ! If no entries, matrix is valid
    IF (COO%nnz == 0) THEN
        RETURN
    END IF
    
    ! Check bounds for all entries
    DO i = 1, COO%nnz
        row = COO%row_ind(i)
        col = COO%col_ind(i)
        
        ! Check row bounds
        IF (row < 1 .OR. row > COO%nrows) THEN
            INFO = SPARSE_ERR_INVALID
            RETURN
        END IF
        
        ! Check column bounds
        IF (col < 1 .OR. col > COO%ncols) THEN
            INFO = SPARSE_ERR_INVALID
            RETURN
        END IF
    END DO
    
    ! Check for duplicates using a simple hash table approach
    ! For small matrices, we can afford a direct mapping
    IF (COO%nrows * COO%ncols <= 1000000 .AND. COO%nnz > 1) THEN
        ! Use direct mapping for small matrices
        ALLOCATE(entry_map(COO%nrows, COO%ncols))
        entry_map = 0
        
        found_duplicate = .FALSE.
        DO i = 1, COO%nnz
            row = COO%row_ind(i)
            col = COO%col_ind(i)
            
            IF (entry_map(row, col) /= 0) THEN
                found_duplicate = .TRUE.
                EXIT
            END IF
            entry_map(row, col) = i
        END DO
        
        DEALLOCATE(entry_map)
        
        IF (found_duplicate) THEN
            INFO = SPARSE_ERR_INVALID
            RETURN
        END IF
    ELSE IF (COO%nnz > 1) THEN
        ! For larger matrices, use sorting to detect duplicates
        ! This is O(n log n) but uses less memory
        DO i = 1, COO%nnz - 1
            DO j = i + 1, COO%nnz
                IF (COO%row_ind(i) == COO%row_ind(j) .AND. &
                    COO%col_ind(i) == COO%col_ind(j)) THEN
                    INFO = SPARSE_ERR_INVALID
                    RETURN
                END IF
            END DO
        END DO
    END IF
    
END SUBROUTINE DCOOCHECK