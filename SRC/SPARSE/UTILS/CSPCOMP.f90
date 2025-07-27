!> \brief Remove duplicate entries and explicit zeros from sparse matrices
!>
!> Compresses sparse matrices by combining duplicate entries (summing values)
!> and removing entries with zero values. This is essential after operations
!> that may create duplicates (like DSPADD2) or zeros (like cancellation).
!>
!> \param[in] FORMAT Character indicating sparse format:
!>            'O' or 'o': COO format
!>            'R' or 'r': CSR format
!>            'C' or 'c': CSC format
!> \param[in,out] SPARSE Sparse matrix to compress (type depends on FORMAT)
!> \param[in] TOL Tolerance for zero detection (optional, default 0.0)
!> \param[out] INFO Return status:
!>             0: Success
!>            -1: Invalid format
!>            -2: Memory allocation error

SUBROUTINE CSPCOMP(FORMAT, SPARSE, TOL, INFO)
    USE sparse_types_extended
    USE ISO_FORTRAN_ENV, ONLY: int32, real32
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER, INTENT(IN) :: FORMAT
    CLASS(*), INTENT(INOUT) :: SPARSE
    COMPLEX(real32), INTENT(IN), OPTIONAL :: TOL
    INTEGER(int32), INTENT(OUT) :: INFO
    
    ! Local variables
    COMPLEX(real32) :: tolerance
    
    ! External function
    LOGICAL, EXTERNAL :: LSAME
    
    ! External procedure interface
    INTERFACE
        SUBROUTINE CSPSORT(FORMAT, SPARSE, INFO)
            CHARACTER, INTENT(IN) :: FORMAT
            CLASS(*), INTENT(INOUT) :: SPARSE
            INTEGER, INTENT(OUT) :: INFO
        END SUBROUTINE CSPSORT
    END INTERFACE
    
    ! Initialize
    INFO = 0
    tolerance = (0.0_real32, 0.0_real32)
    IF (PRESENT(TOL)) tolerance = CABS(TOL)
    
    ! Dispatch based on format
    IF (LSAME(FORMAT, 'O')) THEN
        ! COO format
        SELECT TYPE(SPARSE)
        TYPE IS (sparse_coo_c)
            CALL CSPCOMP_COO(SPARSE, tolerance, INFO)
        CLASS DEFAULT
            INFO = -1
        END SELECT
        
    ELSE IF (LSAME(FORMAT, 'R')) THEN
        ! CSR format
        SELECT TYPE(SPARSE)
        TYPE IS (sparse_csr_c)
            CALL CSPCOMP_CSR(SPARSE, tolerance, INFO)
        CLASS DEFAULT
            INFO = -1
        END SELECT
        
    ELSE IF (LSAME(FORMAT, 'C')) THEN
        ! CSC format
        SELECT TYPE(SPARSE)
        TYPE IS (sparse_csc_c)
            CALL CSPCOMP_CSC(SPARSE, tolerance, INFO)
        CLASS DEFAULT
            INFO = -1
        END SELECT
    ELSE
        INFO = -1
    END IF

CONTAINS

    !> Compress COO format by removing duplicates and zeros
    SUBROUTINE CSPCOMP_COO(COO, TOL, INFO)
        TYPE(sparse_coo_c), INTENT(INOUT) :: COO
        COMPLEX(real32), INTENT(IN) :: TOL
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: i, j, nnz_new
        INTEGER(int32) :: current_row, current_col
        COMPLEX(real32) :: sum_val
        LOGICAL :: first_sort_needed
        
        ! Quick return if empty or already checked
        IF (COO%nnz == 0) THEN
            COO%checked = .TRUE.
            RETURN
        END IF
        
        ! First ensure the matrix is sorted
        first_sort_needed = .NOT. COO%sorted
        IF (first_sort_needed) THEN
            CALL CSPSORT('O', COO, INFO)
            IF (INFO /= 0) RETURN
        END IF
        
        ! Now compress by combining duplicates and removing zeros
        nnz_new = 0
        i = 1
        
        DO WHILE (i <= COO%nnz)
            current_row = COO%row_ind(i)
            current_col = COO%col_ind(i)
            sum_val = COO%values(i)
            
            ! Find all duplicates and sum their values
            j = i + 1
            DO WHILE (j <= COO%nnz .AND. &
                      COO%row_ind(j) == current_row .AND. &
                      COO%col_ind(j) == current_col)
                sum_val = sum_val + COO%values(j)
                j = j + 1
            END DO
            
            ! Store if non-zero
            IF (CABS(sum_val) > TOL) THEN
                nnz_new = nnz_new + 1
                COO%row_ind(nnz_new) = current_row
                COO%col_ind(nnz_new) = current_col
                COO%values(nnz_new) = sum_val
            END IF
            
            i = j
        END DO
        
        ! Update nnz count
        COO%nnz = nnz_new
        COO%checked = .TRUE.
        
    END SUBROUTINE CSPCOMP_COO
    
    !> Compress CSR format by removing duplicates and zeros
    SUBROUTINE CSPCOMP_CSR(CSR, TOL, INFO)
        TYPE(sparse_csr_c), INTENT(INOUT) :: CSR
        COMPLEX(real32), INTENT(IN) :: TOL
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: i, j, k, nnz_new, row_start, row_end
        INTEGER(int32) :: current_col, new_row_start
        COMPLEX(real32) :: sum_val
        INTEGER(int32), ALLOCATABLE :: new_col_ind(:)
        COMPLEX(real32), ALLOCATABLE :: new_values(:)
        INTEGER(int32), ALLOCATABLE :: new_row_ptr(:)
        
        ! Quick return if empty
        IF (CSR%nnz == 0) RETURN
        
        ! First ensure each row is sorted
        IF (.NOT. CSR%sorted) THEN
            CALL CSPSORT('R', CSR, INFO)
            IF (INFO /= 0) RETURN
        END IF
        
        ! Allocate temporary arrays
        ALLOCATE(new_col_ind(CSR%nnz), new_values(CSR%nnz), &
                 new_row_ptr(CSR%nrows + 1), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -2
            RETURN
        END IF
        
        ! Process each row
        nnz_new = 0
        new_row_ptr(1) = 1
        
        DO i = 1, CSR%nrows
            row_start = CSR%row_ptr(i)
            row_end = CSR%row_ptr(i+1) - 1
            new_row_start = nnz_new + 1
            
            j = row_start
            DO WHILE (j <= row_end)
                current_col = CSR%col_ind(j)
                sum_val = CSR%values(j)
                
                ! Find duplicates in this row
                k = j + 1
                DO WHILE (k <= row_end .AND. CSR%col_ind(k) == current_col)
                    sum_val = sum_val + CSR%values(k)
                    k = k + 1
                END DO
                
                ! Store if non-zero
                IF (CABS(sum_val) > TOL) THEN
                    nnz_new = nnz_new + 1
                    new_col_ind(nnz_new) = current_col
                    new_values(nnz_new) = sum_val
                END IF
                
                j = k
            END DO
            
            new_row_ptr(i+1) = nnz_new + 1
        END DO
        
        ! Copy back if there were changes
        IF (nnz_new /= CSR%nnz) THEN
            CSR%nnz = nnz_new
            CSR%col_ind(1:nnz_new) = new_col_ind(1:nnz_new)
            CSR%values(1:nnz_new) = new_values(1:nnz_new)
            CSR%row_ptr = new_row_ptr
        END IF
        
        DEALLOCATE(new_col_ind, new_values, new_row_ptr)
        
    END SUBROUTINE CSPCOMP_CSR
    
    !> Compress CSC format by removing duplicates and zeros
    SUBROUTINE CSPCOMP_CSC(CSC, TOL, INFO)
        TYPE(sparse_csc_c), INTENT(INOUT) :: CSC
        COMPLEX(real32), INTENT(IN) :: TOL
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: i, j, k, nnz_new, col_start, col_end
        INTEGER(int32) :: current_row, new_col_start
        COMPLEX(real32) :: sum_val
        INTEGER(int32), ALLOCATABLE :: new_row_ind(:)
        COMPLEX(real32), ALLOCATABLE :: new_values(:)
        INTEGER(int32), ALLOCATABLE :: new_col_ptr(:)
        
        ! Quick return if empty
        IF (CSC%nnz == 0) RETURN
        
        ! First ensure each column is sorted
        IF (.NOT. CSC%sorted) THEN
            CALL CSPSORT('C', CSC, INFO)
            IF (INFO /= 0) RETURN
        END IF
        
        ! Allocate temporary arrays
        ALLOCATE(new_row_ind(CSC%nnz), new_values(CSC%nnz), &
                 new_col_ptr(CSC%ncols + 1), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -2
            RETURN
        END IF
        
        ! Process each column
        nnz_new = 0
        new_col_ptr(1) = 1
        
        DO j = 1, CSC%ncols
            col_start = CSC%col_ptr(j)
            col_end = CSC%col_ptr(j+1) - 1
            new_col_start = nnz_new + 1
            
            i = col_start
            DO WHILE (i <= col_end)
                current_row = CSC%row_ind(i)
                sum_val = CSC%values(i)
                
                ! Find duplicates in this column
                k = i + 1
                DO WHILE (k <= col_end .AND. CSC%row_ind(k) == current_row)
                    sum_val = sum_val + CSC%values(k)
                    k = k + 1
                END DO
                
                ! Store if non-zero
                IF (CABS(sum_val) > TOL) THEN
                    nnz_new = nnz_new + 1
                    new_row_ind(nnz_new) = current_row
                    new_values(nnz_new) = sum_val
                END IF
                
                i = k
            END DO
            
            new_col_ptr(j+1) = nnz_new + 1
        END DO
        
        ! Copy back if there were changes
        IF (nnz_new /= CSC%nnz) THEN
            CSC%nnz = nnz_new
            CSC%row_ind(1:nnz_new) = new_row_ind(1:nnz_new)
            CSC%values(1:nnz_new) = new_values(1:nnz_new)
            CSC%col_ptr = new_col_ptr
        END IF
        
        DEALLOCATE(new_row_ind, new_values, new_col_ptr)
        
    END SUBROUTINE CSPCOMP_CSC

END SUBROUTINE CSPCOMP