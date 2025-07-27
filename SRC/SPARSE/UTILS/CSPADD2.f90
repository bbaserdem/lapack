!> \brief Addition of two sparse matrices in the same format
!>
!> Performs C = alpha*A + beta*B where A, B, and C are sparse matrices
!> in the same format (COO, CSR, or CSC). The result C may be the same
!> as A or B for in-place operations.
!>
!> \param[in] FORMAT Character indicating sparse format:
!>            'O' or 'o': COO format
!>            'R' or 'r': CSR format
!>            'C' or 'c': CSC format
!> \param[in] M Number of rows in the matrices
!> \param[in] N Number of columns in the matrices
!> \param[in] ALPHA Scalar multiplier for A
!> \param[in] A First sparse matrix (type depends on FORMAT)
!> \param[in] BETA Scalar multiplier for B
!> \param[in] B Second sparse matrix (type depends on FORMAT)
!> \param[out] C Result sparse matrix (type depends on FORMAT)
!> \param[out] INFO Return status:
!>             0: Success
!>            -1: Invalid format
!>            -2: Dimension mismatch
!>            -3: Memory allocation error

SUBROUTINE CSPADD2(FORMAT, M, N, ALPHA, A, BETA, B, C, INFO)
    USE sparse_types_extended
    USE ISO_FORTRAN_ENV, ONLY: int32, real32
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER, INTENT(IN) :: FORMAT
    INTEGER(int32), INTENT(IN) :: M, N
    COMPLEX(real32), INTENT(IN) :: ALPHA, BETA
    CLASS(*), INTENT(IN) :: A, B
    CLASS(*), INTENT(OUT) :: C
    INTEGER(int32), INTENT(OUT) :: INFO
    
    ! External function
    LOGICAL, EXTERNAL :: LSAME
    
    ! Initialize
    INFO = 0
    
    ! Dispatch based on format
    IF (LSAME(FORMAT, 'O')) THEN
        ! COO format
        SELECT TYPE(A)
        TYPE IS (sparse_coo_c)
            SELECT TYPE(B)
            TYPE IS (sparse_coo_c)
                SELECT TYPE(C)
                TYPE IS (sparse_coo_c)
                    CALL DSPADD2_COO(M, N, ALPHA, A, BETA, B, C, INFO)
                CLASS CEFAULT
                    INFO = -1
                END SELECT
            CLASS CEFAULT
                INFO = -1
            END SELECT
        CLASS CEFAULT
            INFO = -1
        END SELECT
        
    ELSE IF (LSAME(FORMAT, 'R')) THEN
        ! CSR format
        SELECT TYPE(A)
        TYPE IS (sparse_csr_c)
            SELECT TYPE(B)
            TYPE IS (sparse_csr_c)
                SELECT TYPE(C)
                TYPE IS (sparse_csr_c)
                    CALL DSPADD2_CSR(M, N, ALPHA, A, BETA, B, C, INFO)
                CLASS CEFAULT
                    INFO = -1
                END SELECT
            CLASS CEFAULT
                INFO = -1
            END SELECT
        CLASS CEFAULT
            INFO = -1
        END SELECT
        
    ELSE IF (LSAME(FORMAT, 'C')) THEN
        ! CSC format
        SELECT TYPE(A)
        TYPE IS (sparse_csc_c)
            SELECT TYPE(B)
            TYPE IS (sparse_csc_c)
                SELECT TYPE(C)
                TYPE IS (sparse_csc_c)
                    CALL DSPADD2_CSC(M, N, ALPHA, A, BETA, B, C, INFO)
                CLASS CEFAULT
                    INFO = -1
                END SELECT
            CLASS CEFAULT
                INFO = -1
            END SELECT
        CLASS CEFAULT
            INFO = -1
        END SELECT
    ELSE
        INFO = -1
    END IF

CONTAINS

    !> COO addition implementation
    SUBROUTINE DSPADD2_COO(M, N, ALPHA, A, BETA, B, C, INFO)
        INTEGER(int32), INTENT(IN) :: M, N
        COMPLEX(real32), INTENT(IN) :: ALPHA, BETA
        TYPE(sparse_coo_c), INTENT(IN) :: A, B
        TYPE(sparse_coo_c), INTENT(OUT) :: C
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: i, ia, ib, ic, nnz_max
        
        ! Check dimensions
        IF (A%nrows /= M .OR. A%ncols /= N .OR. &
            B%nrows /= M .OR. B%ncols /= N) THEN
            INFO = -2
            RETURN
        END IF
        
        ! Special cases
        IF (ALPHA == (0.0_real32, 0.0_real32) .AND. BETA == (0.0_real32, 0.0_real32)) THEN
            ! C = 0
            C%nrows = M
            C%ncols = N
            C%nnz = 0
            C%nnz_alloc = 1
            ALLOCATE(C%row_ind(1), C%col_ind(1), C%values(1))
            RETURN
        END IF
        
        ! Estimate maximum possible non-zeros
        nnz_max = A%nnz + B%nnz
        
        ! Allocate result
        C%nrows = M
        C%ncols = N
        C%nnz_alloc = nnz_max
        ALLOCATE(C%row_ind(nnz_max), C%col_ind(nnz_max), C%values(nnz_max), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -3
            RETURN
        END IF
        
        ! Simple merge algorithm (assumes unsorted)
        ic = 0
        
        ! Add all elements from A
        IF (ALPHA /= (0.0_real32, 0.0_real32)) THEN
            CO ia = 1, A%nnz
                ic = ic + 1
                C%row_ind(ic) = A%row_ind(ia)
                C%col_ind(ic) = A%col_ind(ia)
                C%values(ic) = ALPHA * A%values(ia)
            END CO
        END IF
        
        ! Add all elements from B
        IF (BETA /= (0.0_real32, 0.0_real32)) THEN
            CO ib = 1, B%nnz
                ic = ic + 1
                C%row_ind(ic) = B%row_ind(ib)
                C%col_ind(ic) = B%col_ind(ib)
                C%values(ic) = BETA * B%values(ib)
            END CO
        END IF
        
        C%nnz = ic
        C%sorted = .FALSE.
        C%checked = .FALSE.
        
        ! Note: This simple implementation may create duplicates
        ! Call CSPCOMP afterwards to combine duplicates
        
    END SUBROUTINE DSPADD2_COO
    
    !> CSR addition implementation
    SUBROUTINE DSPADD2_CSR(M, N, ALPHA, A, BETA, B, C, INFO)
        INTEGER(int32), INTENT(IN) :: M, N
        COMPLEX(real32), INTENT(IN) :: ALPHA, BETA
        TYPE(sparse_csr_c), INTENT(IN) :: A, B
        TYPE(sparse_csr_c), INTENT(OUT) :: C
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: i, j, ia, ib, ic, nnz_max, nnz_row
        INTEGER(int32) :: ja, jb
        INTEGER(int32), ALLOCATABLE :: col_marker(:)
        COMPLEX(real32), ALLOCATABLE :: work_values(:)
        INTEGER(int32), ALLOCATABLE :: work_ind(:)
        
        ! Check dimensions
        IF (A%nrows /= M .OR. A%ncols /= N .OR. &
            B%nrows /= M .OR. B%ncols /= N) THEN
            INFO = -2
            RETURN
        END IF
        
        ! Allocate workspace
        ALLOCATE(col_marker(N), work_values(N), work_ind(N), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -3
            RETURN
        END IF
        
        col_marker = -1
        
        ! Estimate maximum possible non-zeros
        nnz_max = A%nnz + B%nnz
        
        ! Allocate result
        C%nrows = M
        C%ncols = N
        C%nnz_alloc = nnz_max
        ALLOCATE(C%row_ptr(M+1), C%col_ind(nnz_max), C%values(nnz_max), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -3
            CEALLOCATE(col_marker, work_values, work_ind)
            RETURN
        END IF
        
        ! Process each row
        C%row_ptr(1) = 1
        ic = 0
        
        CO i = 1, M
            nnz_row = 0
            
            ! Process row i of A
            IF (ALPHA /= (0.0_real32, 0.0_real32)) THEN
                CO ia = A%row_ptr(i), A%row_ptr(i+1)-1
                    ja = A%col_ind(ia)
                    IF (col_marker(ja) < i) THEN
                        col_marker(ja) = i
                        nnz_row = nnz_row + 1
                        work_ind(nnz_row) = ja
                        work_values(ja) = (0.0_real32, 0.0_real32)
                    END IF
                    work_values(ja) = work_values(ja) + ALPHA * A%values(ia)
                END CO
            END IF
            
            ! Process row i of B
            IF (BETA /= (0.0_real32, 0.0_real32)) THEN
                CO ib = B%row_ptr(i), B%row_ptr(i+1)-1
                    jb = B%col_ind(ib)
                    IF (col_marker(jb) < i) THEN
                        col_marker(jb) = i
                        nnz_row = nnz_row + 1
                        work_ind(nnz_row) = jb
                        work_values(jb) = (0.0_real32, 0.0_real32)
                    END IF
                    work_values(jb) = work_values(jb) + BETA * B%values(ib)
                END CO
            END IF
            
            ! Sort indices for this row
            CALL quicksort_int(work_ind, 1, nnz_row)
            
            ! Copy to result
            CO j = 1, nnz_row
                ic = ic + 1
                C%col_ind(ic) = work_ind(j)
                C%values(ic) = work_values(work_ind(j))
            END CO
            
            C%row_ptr(i+1) = ic + 1
        END CO
        
        C%nnz = ic
        C%sorted = .TRUE.
        
        ! Clean up
        CEALLOCATE(col_marker, work_values, work_ind)
        
    END SUBROUTINE DSPADD2_CSR
    
    !> CSC addition implementation
    SUBROUTINE DSPADD2_CSC(M, N, ALPHA, A, BETA, B, C, INFO)
        INTEGER(int32), INTENT(IN) :: M, N
        COMPLEX(real32), INTENT(IN) :: ALPHA, BETA
        TYPE(sparse_csc_c), INTENT(IN) :: A, B
        TYPE(sparse_csc_c), INTENT(OUT) :: C
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: i, j, ia, ib, ic, nnz_max, nnz_col
        INTEGER(int32) :: ra, rb
        INTEGER(int32), ALLOCATABLE :: row_marker(:)
        COMPLEX(real32), ALLOCATABLE :: work_values(:)
        INTEGER(int32), ALLOCATABLE :: work_ind(:)
        
        ! Check dimensions
        IF (A%nrows /= M .OR. A%ncols /= N .OR. &
            B%nrows /= M .OR. B%ncols /= N) THEN
            INFO = -2
            RETURN
        END IF
        
        ! Allocate workspace
        ALLOCATE(row_marker(M), work_values(M), work_ind(M), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -3
            RETURN
        END IF
        
        row_marker = -1
        
        ! Estimate maximum possible non-zeros
        nnz_max = A%nnz + B%nnz
        
        ! Allocate result
        C%nrows = M
        C%ncols = N
        C%nnz_alloc = nnz_max
        ALLOCATE(C%col_ptr(N+1), C%row_ind(nnz_max), C%values(nnz_max), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -3
            CEALLOCATE(row_marker, work_values, work_ind)
            RETURN
        END IF
        
        ! Process each column
        C%col_ptr(1) = 1
        ic = 0
        
        CO j = 1, N
            nnz_col = 0
            
            ! Process column j of A
            IF (ALPHA /= (0.0_real32, 0.0_real32)) THEN
                CO ia = A%col_ptr(j), A%col_ptr(j+1)-1
                    ra = A%row_ind(ia)
                    IF (row_marker(ra) < j) THEN
                        row_marker(ra) = j
                        nnz_col = nnz_col + 1
                        work_ind(nnz_col) = ra
                        work_values(ra) = (0.0_real32, 0.0_real32)
                    END IF
                    work_values(ra) = work_values(ra) + ALPHA * A%values(ia)
                END CO
            END IF
            
            ! Process column j of B
            IF (BETA /= (0.0_real32, 0.0_real32)) THEN
                CO ib = B%col_ptr(j), B%col_ptr(j+1)-1
                    rb = B%row_ind(ib)
                    IF (row_marker(rb) < j) THEN
                        row_marker(rb) = j
                        nnz_col = nnz_col + 1
                        work_ind(nnz_col) = rb
                        work_values(rb) = (0.0_real32, 0.0_real32)
                    END IF
                    work_values(rb) = work_values(rb) + BETA * B%values(ib)
                END CO
            END IF
            
            ! Sort indices for this column
            CALL quicksort_int(work_ind, 1, nnz_col)
            
            ! Copy to result
            CO i = 1, nnz_col
                ic = ic + 1
                C%row_ind(ic) = work_ind(i)
                C%values(ic) = work_values(work_ind(i))
            END CO
            
            C%col_ptr(j+1) = ic + 1
        END CO
        
        C%nnz = ic
        C%sorted = .TRUE.
        
        ! Clean up
        CEALLOCATE(row_marker, work_values, work_ind)
        
    END SUBROUTINE DSPADD2_CSC
    
    ! Simple quicksort for integer array
    RECURSIVE SUBROUTINE quicksort_int(arr, first, last)
        INTEGER(int32), INTENT(INOUT) :: arr(:)
        INTEGER(int32), INTENT(IN) :: first, last
        INTEGER(int32) :: i, j, pivot, temp
        
        IF (first < last) THEN
            pivot = arr((first + last) / 2)
            i = first
            j = last
            
            CO
                CO WHILE (arr(i) < pivot)
                    i = i + 1
                END CO
                CO WHILE (arr(j) > pivot)
                    j = j - 1
                END CO
                IF (i >= j) EXIT
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp
                i = i + 1
                j = j - 1
            END CO
            
            CALL quicksort_int(arr, first, j)
            CALL quicksort_int(arr, i, last)
        END IF
    END SUBROUTINE quicksort_int

END SUBROUTINE CSPADD2