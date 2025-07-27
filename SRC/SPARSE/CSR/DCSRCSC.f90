!> \brief Multiplies two sparse matrices: CSR × CSC
!>
!> Performs C = alpha*op(A)*op(B) + beta*C where A is CSR, B is CSC
!> This is one of the most efficient sparse-sparse multiplication algorithms
!> as CSR is row-oriented and CSC is column-oriented, making the inner product
!> computation natural.
!>
!> \param[in] TRANSA Character flag for op(A):
!>            'N' or 'n': op(A) = A
!>            'T' or 't': op(A) = A^T (transpose)
!> \param[in] TRANSB Character flag for op(B):
!>            'N' or 'n': op(B) = B  
!>            'T' or 't': op(B) = B^T (transpose)
!> \param[in] M Number of rows of op(A) and C
!> \param[in] N Number of columns of op(B) and C
!> \param[in] K Number of columns of op(A) and rows of op(B)
!> \param[in] ALPHA Scalar multiplier
!> \param[in] A CSR matrix A
!> \param[in] B CSC matrix B
!> \param[in] BETA Scalar multiplier for C
!> \param[in,out] C Result matrix in CSR format

SUBROUTINE DCSRCSC(TRANSA, TRANSB, M, N, K, ALPHA, A, B, BETA, C)
    USE sparse_types
    USE ISO_FORTRAN_ENV, ONLY: int32, real64
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER, INTENT(IN) :: TRANSA, TRANSB
    INTEGER(int32), INTENT(IN) :: M, N, K
    REAL(real64), INTENT(IN) :: ALPHA, BETA
    TYPE(sparse_csr_d), INTENT(IN) :: A
    TYPE(sparse_csc_d), INTENT(IN) :: B
    TYPE(sparse_csr_d), INTENT(INOUT) :: C
    
    ! Local variables
    INTEGER(int32) :: i, j, ia, ib, ic, nnz_est, nnz_row
    INTEGER(int32) :: kk  ! Use kk to avoid conflict with argument K
    INTEGER(int32) :: row_start_a, row_end_a, col_start_b, col_end_b
    REAL(real64) :: sum
    LOGICAL :: nota, notb
    INTEGER(int32), ALLOCATABLE :: col_marker(:), col_list(:)
    REAL(real64), ALLOCATABLE :: work_values(:)
    
    ! External function
    LOGICAL, EXTERNAL :: LSAME
    
    ! Check transpose flags
    nota = LSAME(TRANSA, 'N')
    notb = LSAME(TRANSB, 'N')
    
    ! Dimension checks
    IF (.NOT. nota .AND. .NOT. LSAME(TRANSA, 'T')) THEN
        PRINT *, 'DCSRCSC: Invalid TRANSA = ', TRANSA
        RETURN
    END IF
    
    IF (.NOT. notb .AND. .NOT. LSAME(TRANSB, 'T')) THEN
        PRINT *, 'DCSRCSC: Invalid TRANSB = ', TRANSB
        RETURN
    END IF
    
    ! Check matrix dimensions
    IF (nota) THEN
        IF (A%nrows /= M .OR. A%ncols /= K) THEN
            PRINT *, 'DCSRCSC: Dimension mismatch in A'
            RETURN
        END IF
    ELSE
        IF (A%ncols /= M .OR. A%nrows /= K) THEN
            PRINT *, 'DCSRCSC: Dimension mismatch in transposed A'
            RETURN
        END IF
    END IF
    
    IF (notb) THEN
        IF (B%nrows /= K .OR. B%ncols /= N) THEN
            PRINT *, 'DCSRCSC: Dimension mismatch in B'
            RETURN
        END IF
    ELSE
        IF (B%ncols /= K .OR. B%nrows /= N) THEN
            PRINT *, 'DCSRCSC: Dimension mismatch in transposed B'
            RETURN
        END IF
    END IF
    
    ! Handle alpha = 0 case
    IF (ALPHA == 0.0_real64) THEN
        IF (BETA == 0.0_real64) THEN
            ! C = 0
            C%nnz = 0
            C%row_ptr = 1
        ELSE IF (BETA /= 1.0_real64) THEN
            ! C = beta * C
            C%values = BETA * C%values
        END IF
        RETURN
    END IF
    
    ! Allocate workspace
    ALLOCATE(col_marker(N), col_list(N), work_values(N))
    col_marker = -1
    
    ! Estimate nnz for result (use heuristic: nnz(C) ≈ min(M*N, 2*max(nnz(A), nnz(B))))
    nnz_est = MIN(M * N, 2 * MAX(A%nnz, B%nnz))
    
    ! Initialize result matrix C
    IF (BETA == 0.0_real64) THEN
        ! Allocate fresh C
        C%nrows = M
        C%ncols = N
        C%nnz_alloc = nnz_est
        IF (ALLOCATED(C%row_ptr)) DEALLOCATE(C%row_ptr)
        IF (ALLOCATED(C%col_ind)) DEALLOCATE(C%col_ind)
        IF (ALLOCATED(C%values)) DEALLOCATE(C%values)
        ALLOCATE(C%row_ptr(M+1), C%col_ind(nnz_est), C%values(nnz_est))
    ELSE
        ! We need to add to existing C - for now, error
        PRINT *, 'DCSRCSC: beta /= 0 not yet implemented'
        DEALLOCATE(col_marker, col_list, work_values)
        RETURN
    END IF
    
    ! Main multiplication algorithm
    ! For each row of A (or column of A^T)
    C%row_ptr(1) = 1
    ic = 0
    
    DO i = 1, M
        nnz_row = 0
        work_values = 0.0_real64
        
        IF (nota) THEN
            ! Process row i of A
            row_start_a = A%row_ptr(i)
            row_end_a = A%row_ptr(i+1) - 1
            
            ! For each non-zero in row i of A
            DO ia = row_start_a, row_end_a
                kk = A%col_ind(ia)  ! Column index in A = row index in B
                
                IF (notb) THEN
                    ! Process row kk of B (which is stored as column data)
                    ! Need to iterate through all columns of B to find row kk entries
                    DO j = 1, N
                        col_start_b = B%col_ptr(j)
                        col_end_b = B%col_ptr(j+1) - 1
                        
                        ! Binary search for row kk in column j
                        DO ib = col_start_b, col_end_b
                            IF (B%row_ind(ib) == kk) THEN
                                ! Found element B(k,j)
                                IF (col_marker(j) < i) THEN
                                    col_marker(j) = i
                                    nnz_row = nnz_row + 1
                                    col_list(nnz_row) = j
                                END IF
                                work_values(j) = work_values(j) + ALPHA * A%values(ia) * B%values(ib)
                                EXIT
                            END IF
                        END DO
                    END DO
                ELSE
                    ! B is transposed: process column kk of B as a row
                    col_start_b = B%col_ptr(kk)
                    col_end_b = B%col_ptr(kk+1) - 1
                    
                    DO ib = col_start_b, col_end_b
                        j = B%row_ind(ib)  ! This becomes column index in B^T
                        IF (col_marker(j) < i) THEN
                            col_marker(j) = i
                            nnz_row = nnz_row + 1
                            col_list(nnz_row) = j
                        END IF
                        work_values(j) = work_values(j) + ALPHA * A%values(ia) * B%values(ib)
                    END DO
                END IF
            END DO
        ELSE
            ! A is transposed - not implemented yet
            PRINT *, 'DCSRCSC: Transposed A not yet implemented'
            DEALLOCATE(col_marker, col_list, work_values)
            RETURN
        END IF
        
        ! Store the non-zeros for row i
        ! First, sort column indices
        CALL quicksort_int(col_list, 1, nnz_row)
        
        ! Check if we need to reallocate
        IF (ic + nnz_row > C%nnz_alloc) THEN
            CALL resize_csr(C, 2 * C%nnz_alloc)
        END IF
        
        ! Copy sorted values to C
        DO j = 1, nnz_row
            ic = ic + 1
            C%col_ind(ic) = col_list(j)
            C%values(ic) = work_values(col_list(j))
        END DO
        
        C%row_ptr(i+1) = ic + 1
    END DO
    
    C%nnz = ic
    
    ! Clean up
    DEALLOCATE(col_marker, col_list, work_values)
    
CONTAINS

    ! Simple quicksort for integer array
    RECURSIVE SUBROUTINE quicksort_int(arr, first, last)
        INTEGER(int32), INTENT(INOUT) :: arr(:)
        INTEGER(int32), INTENT(IN) :: first, last
        INTEGER(int32) :: i, j, pivot, temp
        
        IF (first < last) THEN
            pivot = arr((first + last) / 2)
            i = first
            j = last
            
            DO
                DO WHILE (arr(i) < pivot)
                    i = i + 1
                END DO
                DO WHILE (arr(j) > pivot)
                    j = j - 1
                END DO
                IF (i >= j) EXIT
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp
                i = i + 1
                j = j - 1
            END DO
            
            CALL quicksort_int(arr, first, j)
            CALL quicksort_int(arr, i, last)
        END IF
    END SUBROUTINE quicksort_int
    
    ! Resize CSR matrix arrays
    SUBROUTINE resize_csr(mat, new_size)
        TYPE(sparse_csr_d), INTENT(INOUT) :: mat
        INTEGER(int32), INTENT(IN) :: new_size
        INTEGER(int32), ALLOCATABLE :: temp_ind(:)
        REAL(real64), ALLOCATABLE :: temp_val(:)
        
        ALLOCATE(temp_ind(new_size), temp_val(new_size))
        temp_ind(1:mat%nnz) = mat%col_ind(1:mat%nnz)
        temp_val(1:mat%nnz) = mat%values(1:mat%nnz)
        
        DEALLOCATE(mat%col_ind, mat%values)
        mat%col_ind = temp_ind
        mat%values = temp_val
        mat%nnz_alloc = new_size
    END SUBROUTINE resize_csr

END SUBROUTINE DCSRCSC