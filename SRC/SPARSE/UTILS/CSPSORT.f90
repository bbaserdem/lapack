!> \brief Sort indices in sparse matrices for better cache performance
!>
!> Ensures that indices within each row (CSR) or column (CSC) are sorted,
!> or that (row,col) pairs in COO format are in lexicographic order.
!> This improves cache locality and enables binary search operations.
!>
!> \param[in] FORMAT Character indicating sparse format:
!>            'O' or 'o': COO format
!>            'R' or 'r': CSR format
!>            'C' or 'c': CSC format
!> \param[in,out] SPARSE Sparse matrix to sort (type depends on FORMAT)
!> \param[out] INFO Return status:
!>             0: Success
!>            -1: Invalid format
!>            -2: Invalid matrix structure

SUBROUTINE CSPSORT(FORMAT, SPARSE, INFO)
    USE sparse_types_extended
    USE ISO_FORTRAN_ENV, ONLY: int32, real32
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER, INTENT(IN) :: FORMAT
    CLASS(*), INTENT(INOUT) :: SPARSE
    INTEGER(int32), INTENT(OUT) :: INFO
    
    ! External function
    LOGICAL, EXTERNAL :: LSAME
    
    ! Initialize
    INFO = 0
    
    ! Dispatch based on format
    IF (LSAME(FORMAT, 'O')) THEN
        ! COO format
        SELECT TYPE(SPARSE)
        TYPE IS (sparse_coo_c)
            CALL CSPSORT_COO(SPARSE, INFO)
        CLASS DEFAULT
            INFO = -1
        END SELECT
        
    ELSE IF (LSAME(FORMAT, 'R')) THEN
        ! CSR format
        SELECT TYPE(SPARSE)
        TYPE IS (sparse_csr_c)
            CALL CSPSORT_CSR(SPARSE, INFO)
        CLASS DEFAULT
            INFO = -1
        END SELECT
        
    ELSE IF (LSAME(FORMAT, 'C')) THEN
        ! CSC format
        SELECT TYPE(SPARSE)
        TYPE IS (sparse_csc_c)
            CALL CSPSORT_CSC(SPARSE, INFO)
        CLASS DEFAULT
            INFO = -1
        END SELECT
    ELSE
        INFO = -1
    END IF

CONTAINS

    !> Sort COO format by row then column (lexicographic order)
    SUBROUTINE CSPSORT_COO(COO, INFO)
        TYPE(sparse_coo_c), INTENT(INOUT) :: COO
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: i
        INTEGER(int32), ALLOCATABLE :: perm(:)
        INTEGER(int32), ALLOCATABLE :: temp_row(:), temp_col(:)
        COMPLEX(real32), ALLOCATABLE :: temp_val(:)
        
        ! Quick return if already sorted or empty
        IF (COO%sorted .OR. COO%nnz == 0) THEN
            COO%sorted = .TRUE.
            RETURN
        END IF
        
        ! Allocate permutation array
        ALLOCATE(perm(COO%nnz), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -2
            RETURN
        END IF
        
        ! Initialize permutation
        DO i = 1, COO%nnz
            perm(i) = i
        END DO
        
        ! Sort permutation array based on (row,col) pairs
        CALL quicksort_coo_indices(COO%row_ind, COO%col_ind, perm, 1, COO%nnz)
        
        ! Apply permutation
        ALLOCATE(temp_row(COO%nnz), temp_col(COO%nnz), temp_val(COO%nnz), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -2
            DEALLOCATE(perm)
            RETURN
        END IF
        
        DO i = 1, COO%nnz
            temp_row(i) = COO%row_ind(perm(i))
            temp_col(i) = COO%col_ind(perm(i))
            temp_val(i) = COO%values(perm(i))
        END DO
        
        COO%row_ind = temp_row
        COO%col_ind = temp_col
        COO%values = temp_val
        COO%sorted = .TRUE.
        
        DEALLOCATE(perm, temp_row, temp_col, temp_val)
        
    END SUBROUTINE CSPSORT_COO
    
    !> Sort CSR format - sort column indices within each row
    SUBROUTINE CSPSORT_CSR(CSR, INFO)
        TYPE(sparse_csr_c), INTENT(INOUT) :: CSR
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: i, row_start, row_end, row_nnz
        
        ! Quick return if already sorted
        IF (CSR%sorted) RETURN
        
        ! Sort column indices within each row
        DO i = 1, CSR%nrows
            row_start = CSR%row_ptr(i)
            row_end = CSR%row_ptr(i+1) - 1
            row_nnz = row_end - row_start + 1
            
            IF (row_nnz > 1) THEN
                ! Sort column indices and corresponding values
                CALL quicksort_with_values(CSR%col_ind(row_start:row_end), &
                                          CSR%values(row_start:row_end), &
                                          1, row_nnz)
            END IF
        END DO
        
        CSR%sorted = .TRUE.
        
    END SUBROUTINE CSPSORT_CSR
    
    !> Sort CSC format - sort row indices within each column
    SUBROUTINE CSPSORT_CSC(CSC, INFO)
        TYPE(sparse_csc_c), INTENT(INOUT) :: CSC
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: j, col_start, col_end, col_nnz
        
        ! Quick return if already sorted
        IF (CSC%sorted) RETURN
        
        ! Sort row indices within each column
        DO j = 1, CSC%ncols
            col_start = CSC%col_ptr(j)
            col_end = CSC%col_ptr(j+1) - 1
            col_nnz = col_end - col_start + 1
            
            IF (col_nnz > 1) THEN
                ! Sort row indices and corresponding values
                CALL quicksort_with_values(CSC%row_ind(col_start:col_end), &
                                          CSC%values(col_start:col_end), &
                                          1, col_nnz)
            END IF
        END DO
        
        CSC%sorted = .TRUE.
        
    END SUBROUTINE CSPSORT_CSC
    
    !> Quicksort for COO format using lexicographic order
    RECURSIVE SUBROUTINE quicksort_coo_indices(row_ind, col_ind, perm, first, last)
        INTEGER(int32), INTENT(IN) :: row_ind(:), col_ind(:)
        INTEGER(int32), INTENT(INOUT) :: perm(:)
        INTEGER(int32), INTENT(IN) :: first, last
        
        INTEGER(int32) :: i, j, pivot_row, pivot_col, temp
        
        IF (first < last) THEN
            ! Choose pivot
            pivot_row = row_ind(perm((first + last) / 2))
            pivot_col = col_ind(perm((first + last) / 2))
            
            i = first
            j = last
            
            DO
                ! Find elements to swap
                DO WHILE (row_ind(perm(i)) < pivot_row .OR. &
                         (row_ind(perm(i)) == pivot_row .AND. col_ind(perm(i)) < pivot_col))
                    i = i + 1
                END DO
                
                DO WHILE (row_ind(perm(j)) > pivot_row .OR. &
                         (row_ind(perm(j)) == pivot_row .AND. col_ind(perm(j)) > pivot_col))
                    j = j - 1
                END DO
                
                IF (i >= j) EXIT
                
                ! Swap
                temp = perm(i)
                perm(i) = perm(j)
                perm(j) = temp
                
                i = i + 1
                j = j - 1
            END DO
            
            ! Recurse
            CALL quicksort_coo_indices(row_ind, col_ind, perm, first, j)
            CALL quicksort_coo_indices(row_ind, col_ind, perm, i, last)
        END IF
    END SUBROUTINE quicksort_coo_indices
    
    !> Quicksort indices with corresponding values
    RECURSIVE SUBROUTINE quicksort_with_values(indices, values, first, last)
        INTEGER(int32), INTENT(INOUT) :: indices(:)
        COMPLEX(real32), INTENT(INOUT) :: values(:)
        INTEGER(int32), INTENT(IN) :: first, last
        
        INTEGER(int32) :: i, j, pivot, temp_ind
        COMPLEX(real32) :: temp_val
        
        IF (first < last) THEN
            pivot = indices((first + last) / 2)
            i = first
            j = last
            
            DO
                DO WHILE (indices(i) < pivot)
                    i = i + 1
                END DO
                DO WHILE (indices(j) > pivot)
                    j = j - 1
                END DO
                IF (i >= j) EXIT
                
                ! Swap indices
                temp_ind = indices(i)
                indices(i) = indices(j)
                indices(j) = temp_ind
                
                ! Swap values
                temp_val = values(i)
                values(i) = values(j)
                values(j) = temp_val
                
                i = i + 1
                j = j - 1
            END DO
            
            CALL quicksort_with_values(indices, values, first, j)
            CALL quicksort_with_values(indices, values, i, last)
        END IF
    END SUBROUTINE quicksort_with_values

END SUBROUTINE CSPSORT