!> \brief \b DSPSET sets an element value in a sparse matrix
!>
!> \par Purpose:
!> =============
!>
!> DSPSET sets the value of element (I,J) in a sparse matrix.
!> If the element already exists, its value is updated.
!> If the element doesn't exist and VALUE is non-zero, it is added.
!> If the element exists and VALUE is zero, it is removed.
!>
!> The subroutine handles memory reallocation if necessary.
!>
!> \param[in,out] COO
!>          COO is TYPE(sparse_coo_z)
!>          The sparse matrix in COO format to modify.
!>          Note: Currently only COO format is supported for DSPSET.
!>
!> \param[in] I
!>          I is INTEGER
!>          The row index of the element (1-based).
!>
!> \param[in] J
!>          J is INTEGER
!>          The column index of the element (1-based).
!>
!> \param[in] VALUE
!>          VALUE is COMPLEX*16
!>          The value to set for element (I,J).
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_ALLOC: memory allocation failed

SUBROUTINE ZSPSET_COO(COO, I, J, VALUE, INFO)
    USE sparse_types_extended
    USE sparse_constants
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_coo_z), INTENT(INOUT) :: COO
    INTEGER, INTENT(IN) :: I, J
    COMPLEX(real64), INTENT(IN) :: VALUE
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: k, new_size, ierr
    INTEGER, ALLOCATABLE :: temp_row(:), temp_col(:)
    COMPLEX(real64), ALLOCATABLE :: temp_val(:)
    LOGICAL :: found
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check arguments
    IF (I < 1 .OR. I > COO%nrows) THEN
        INFO = -2
        RETURN
    END IF
    
    IF (J < 1 .OR. J > COO%ncols) THEN
        INFO = -3
        RETURN
    END IF
    
    ! Search for the element
    found = .FALSE.
    DO k = 1, COO%nnz
        IF (COO%row_ind(k) == I .AND. COO%col_ind(k) == J) THEN
            found = .TRUE.
            EXIT
        END IF
    END DO
    
    IF (found) THEN
        IF (ZABS(VALUE) < SPARSE_ZERO_TOL) THEN
            ! Remove the element (shift remaining elements)
            DO k = k, COO%nnz - 1
                COO%row_ind(k) = COO%row_ind(k+1)
                COO%col_ind(k) = COO%col_ind(k+1)
                COO%values(k) = COO%values(k+1)
            END DO
            COO%nnz = COO%nnz - 1
            ! Mark as unsorted and unchecked
            COO%sorted = .FALSE.
            COO%checked = .FALSE.
        ELSE
            ! Update the value
            COO%values(k) = VALUE
        END IF
    ELSE
        ! Element not found
        IF (ZABS(VALUE) >= SPARSE_ZERO_TOL) THEN
            ! Add new element
            IF (COO%nnz >= COO%nnz_alloc) THEN
                ! Need to reallocate
                new_size = MAX(INT(COO%nnz_alloc * SPARSE_GROWTH_FACTOR), COO%nnz + 1)
                
                ! Allocate temporary arrays
                ALLOCATE(temp_row(new_size), temp_col(new_size), temp_val(new_size), STAT=ierr)
                IF (ierr /= 0) THEN
                    INFO = SPARSE_ERR_ALLOC
                    RETURN
                END IF
                
                ! Copy existing data
                temp_row(1:COO%nnz) = COO%row_ind(1:COO%nnz)
                temp_col(1:COO%nnz) = COO%col_ind(1:COO%nnz)
                temp_val(1:COO%nnz) = COO%values(1:COO%nnz)
                
                ! Deallocate old arrays
                DEALLOCATE(COO%row_ind, COO%col_ind, COO%values)
                
                ! Move new arrays
                CALL MOVE_ALLOC(temp_row, COO%row_ind)
                CALL MOVE_ALLOC(temp_col, COO%col_ind)
                CALL MOVE_ALLOC(temp_val, COO%values)
                
                COO%nnz_alloc = new_size
            END IF
            
            ! Add the new element
            COO%nnz = COO%nnz + 1
            COO%row_ind(COO%nnz) = I
            COO%col_ind(COO%nnz) = J
            COO%values(COO%nnz) = VALUE
            
            ! Mark as unsorted and unchecked
            COO%sorted = .FALSE.
            COO%checked = .FALSE.
        END IF
    END IF
    
    RETURN
END SUBROUTINE ZSPSET_COO