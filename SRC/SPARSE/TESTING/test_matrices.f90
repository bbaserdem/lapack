!> \brief Test program with various test matrices for sparse routines
!>
!> This program creates and tests with:
!> - Identity matrix
!> - Tridiagonal matrix
!> - Random sparse matrix
!> - Upper triangular matrix
!> - Lower triangular matrix

PROGRAM test_matrices
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64, output_unit, error_unit
    IMPLICIT NONE
    
    ! Test parameters
    INTEGER, PARAMETER :: N = 100       ! Matrix size for square matrices
    INTEGER, PARAMETER :: M = 80        ! Rows for rectangular matrices
    INTEGER, PARAMETER :: MAX_NNZ = N*N ! Maximum non-zeros
    
    ! Sparse matrices
    TYPE(sparse_coo_d) :: coo_identity, coo_tridiag, coo_random
    TYPE(sparse_coo_d) :: coo_upper, coo_lower
    TYPE(sparse_csr_d) :: csr
    TYPE(sparse_csc_d) :: csc
    
    ! Test variables
    INTEGER :: info, i, j, k, nnz
    INTEGER :: test_count, pass_count
    REAL(real64) :: tol = 1.0E-14_real64
    LOGICAL :: test_ok
    
    ! Random number generation
    REAL(real64) :: rand_val
    INTEGER :: seed_size
    INTEGER, ALLOCATABLE :: seed(:)
    
    ! External routines
    EXTERNAL :: DCOOALLOC, DCOOINIT, DCOOFREE
    EXTERNAL :: DCSRALLOC, DCSRFREE, DCSCALLOC, DCSCFREE
    EXTERNAL :: DCOO2CSR, DCOO2CSC, DCSR2COO, DCSC2COO
    
    WRITE(output_unit, '(A)') '========================================='
    WRITE(output_unit, '(A)') 'Testing Various Sparse Matrix Patterns'
    WRITE(output_unit, '(A)') '========================================='
    
    test_count = 0
    pass_count = 0
    
    ! Initialize random seed
    CALL RANDOM_SEED(SIZE=seed_size)
    ALLOCATE(seed(seed_size))
    seed = 12345  ! Fixed seed for reproducibility
    CALL RANDOM_SEED(PUT=seed)
    
    ! Test 1: Identity Matrix
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 1: Identity Matrix'
    WRITE(output_unit, '(A,I0,A,I0)') 'Size: ', N, 'x', N
    
    CALL create_identity_matrix(N, coo_identity, info)
    IF (info == 0) THEN
        WRITE(output_unit, '(A,I0)') 'Non-zeros: ', coo_identity%nnz
        test_ok = test_identity_properties(coo_identity)
        IF (test_ok) THEN
            pass_count = pass_count + 1
            WRITE(output_unit, '(A)') 'PASSED: Identity matrix properties verified'
        ELSE
            WRITE(error_unit, '(A)') 'FAILED: Identity matrix properties incorrect'
        END IF
    ELSE
        WRITE(error_unit, '(A,I0)') 'FAILED: Could not create identity matrix, INFO = ', info
    END IF
    
    ! Test 2: Tridiagonal Matrix
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 2: Tridiagonal Matrix'
    WRITE(output_unit, '(A,I0,A,I0)') 'Size: ', N, 'x', N
    
    CALL create_tridiagonal_matrix(N, 2.0_real64, -1.0_real64, -1.0_real64, coo_tridiag, info)
    IF (info == 0) THEN
        WRITE(output_unit, '(A,I0)') 'Non-zeros: ', coo_tridiag%nnz
        test_ok = test_tridiagonal_properties(coo_tridiag, N)
        IF (test_ok) THEN
            pass_count = pass_count + 1
            WRITE(output_unit, '(A)') 'PASSED: Tridiagonal matrix properties verified'
        ELSE
            WRITE(error_unit, '(A)') 'FAILED: Tridiagonal matrix properties incorrect'
        END IF
    ELSE
        WRITE(error_unit, '(A,I0)') 'FAILED: Could not create tridiagonal matrix, INFO = ', info
    END IF
    
    ! Test 3: Random Sparse Matrix
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 3: Random Sparse Matrix'
    WRITE(output_unit, '(A,I0,A,I0)') 'Size: ', M, 'x', N
    
    CALL create_random_sparse_matrix(M, N, 0.1_real64, coo_random, info)  ! 10% density
    IF (info == 0) THEN
        WRITE(output_unit, '(A,I0)') 'Non-zeros: ', coo_random%nnz
        WRITE(output_unit, '(A,F6.2,A)') 'Density: ', &
            REAL(coo_random%nnz, real64) / REAL(M*N, real64) * 100.0_real64, '%'
        pass_count = pass_count + 1
        WRITE(output_unit, '(A)') 'PASSED: Random sparse matrix created'
    ELSE
        WRITE(error_unit, '(A,I0)') 'FAILED: Could not create random matrix, INFO = ', info
    END IF
    
    ! Test 4: Conversion Round-trip Test (COO -> CSR -> COO)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 4: COO -> CSR -> COO Round-trip'
    
    CALL test_coo_csr_roundtrip(coo_tridiag, test_ok, info)
    IF (test_ok) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A)') 'PASSED: COO-CSR round-trip preserves data'
    ELSE
        WRITE(error_unit, '(A,I0)') 'FAILED: COO-CSR round-trip error, INFO = ', info
    END IF
    
    ! Test 5: Conversion Round-trip Test (COO -> CSC -> COO)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 5: COO -> CSC -> COO Round-trip'
    
    CALL test_coo_csc_roundtrip(coo_tridiag, test_ok, info)
    IF (test_ok) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A)') 'PASSED: COO-CSC round-trip preserves data'
    ELSE
        WRITE(error_unit, '(A,I0)') 'FAILED: COO-CSC round-trip error, INFO = ', info
    END IF
    
    ! Test 6: Upper Triangular Matrix
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 6: Upper Triangular Matrix'
    WRITE(output_unit, '(A,I0,A,I0)') 'Size: ', N, 'x', N
    
    CALL create_upper_triangular_matrix(N, coo_upper, info)
    IF (info == 0) THEN
        WRITE(output_unit, '(A,I0)') 'Non-zeros: ', coo_upper%nnz
        pass_count = pass_count + 1
        WRITE(output_unit, '(A)') 'PASSED: Upper triangular matrix created'
    ELSE
        WRITE(error_unit, '(A,I0)') 'FAILED: Could not create upper triangular matrix, INFO = ', info
    END IF
    
    ! Test 7: Lower Triangular Matrix
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 7: Lower Triangular Matrix'
    WRITE(output_unit, '(A,I0,A,I0)') 'Size: ', N, 'x', N
    
    CALL create_lower_triangular_matrix(N, coo_lower, info)
    IF (info == 0) THEN
        WRITE(output_unit, '(A,I0)') 'Non-zeros: ', coo_lower%nnz
        pass_count = pass_count + 1
        WRITE(output_unit, '(A)') 'PASSED: Lower triangular matrix created'
    ELSE
        WRITE(error_unit, '(A,I0)') 'FAILED: Could not create lower triangular matrix, INFO = ', info
    END IF
    
    ! Summary
    WRITE(output_unit, '(/A)') '========================================='
    WRITE(output_unit, '(A,I0,A,I0)') 'Tests passed: ', pass_count, ' out of ', test_count
    WRITE(output_unit, '(A)') '========================================='
    
    ! Clean up
    CALL DCOOFREE(coo_identity, info)
    CALL DCOOFREE(coo_tridiag, info)
    CALL DCOOFREE(coo_random, info)
    CALL DCOOFREE(coo_upper, info)
    CALL DCOOFREE(coo_lower, info)
    DEALLOCATE(seed)
    
CONTAINS

    SUBROUTINE create_identity_matrix(n, coo, info)
        INTEGER, INTENT(IN) :: n
        TYPE(sparse_coo_d), INTENT(OUT) :: coo
        INTEGER, INTENT(OUT) :: info
        
        INTEGER :: i
        INTEGER, ALLOCATABLE :: row_ind(:), col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        
        EXTERNAL :: DCOOALLOC, DCOOINIT
        
        ! Allocate arrays
        ALLOCATE(row_ind(n), col_ind(n), values(n))
        
        ! Fill identity matrix
        DO i = 1, n
            row_ind(i) = i
            col_ind(i) = i
            values(i) = 1.0_real64
        END DO
        
        ! Allocate and initialize COO matrix
        CALL DCOOALLOC(n, n, n, coo, info)
        IF (info /= 0) RETURN
        
        CALL DCOOINIT(row_ind, col_ind, values, n, coo, info)
        
        DEALLOCATE(row_ind, col_ind, values)
    END SUBROUTINE create_identity_matrix
    
    SUBROUTINE create_tridiagonal_matrix(n, diag, sub_diag, super_diag, coo, info)
        INTEGER, INTENT(IN) :: n
        REAL(real64), INTENT(IN) :: diag, sub_diag, super_diag
        TYPE(sparse_coo_d), INTENT(OUT) :: coo
        INTEGER, INTENT(OUT) :: info
        
        INTEGER :: i, nnz, idx
        INTEGER, ALLOCATABLE :: row_ind(:), col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        
        EXTERNAL :: DCOOALLOC, DCOOINIT
        
        ! Calculate number of non-zeros
        nnz = n + 2*(n-1)  ! diagonal + sub-diagonal + super-diagonal
        
        ! Allocate arrays
        ALLOCATE(row_ind(nnz), col_ind(nnz), values(nnz))
        
        idx = 0
        
        ! Fill diagonal
        DO i = 1, n
            idx = idx + 1
            row_ind(idx) = i
            col_ind(idx) = i
            values(idx) = diag
        END DO
        
        ! Fill sub-diagonal
        DO i = 2, n
            idx = idx + 1
            row_ind(idx) = i
            col_ind(idx) = i-1
            values(idx) = sub_diag
        END DO
        
        ! Fill super-diagonal
        DO i = 1, n-1
            idx = idx + 1
            row_ind(idx) = i
            col_ind(idx) = i+1
            values(idx) = super_diag
        END DO
        
        ! Allocate and initialize COO matrix
        CALL DCOOALLOC(n, n, nnz, coo, info)
        IF (info /= 0) RETURN
        
        CALL DCOOINIT(row_ind, col_ind, values, nnz, coo, info)
        
        DEALLOCATE(row_ind, col_ind, values)
    END SUBROUTINE create_tridiagonal_matrix
    
    SUBROUTINE create_random_sparse_matrix(m, n, density, coo, info)
        INTEGER, INTENT(IN) :: m, n
        REAL(real64), INTENT(IN) :: density
        TYPE(sparse_coo_d), INTENT(OUT) :: coo
        INTEGER, INTENT(OUT) :: info
        
        INTEGER :: i, j, k, nnz_max, nnz_actual
        REAL(real64) :: rand_val
        INTEGER, ALLOCATABLE :: row_ind(:), col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        LOGICAL, ALLOCATABLE :: mask(:,:)
        
        EXTERNAL :: DCOOALLOC, DCOOINIT
        
        ! Estimate maximum non-zeros
        nnz_max = CEILING(REAL(m*n, real64) * density)
        
        ! Allocate temporary arrays
        ALLOCATE(row_ind(nnz_max), col_ind(nnz_max), values(nnz_max))
        ALLOCATE(mask(m,n))
        
        mask = .FALSE.
        nnz_actual = 0
        
        ! Generate random sparse pattern
        DO k = 1, nnz_max
            ! Find a random unoccupied position
            DO
                CALL RANDOM_NUMBER(rand_val)
                i = 1 + INT(rand_val * m)
                IF (i > m) i = m
                
                CALL RANDOM_NUMBER(rand_val)
                j = 1 + INT(rand_val * n)
                IF (j > n) j = n
                
                IF (.NOT. mask(i,j)) EXIT
            END DO
            
            mask(i,j) = .TRUE.
            nnz_actual = nnz_actual + 1
            row_ind(nnz_actual) = i
            col_ind(nnz_actual) = j
            CALL RANDOM_NUMBER(values(nnz_actual))
            values(nnz_actual) = 2.0_real64 * values(nnz_actual) - 1.0_real64  ! Range [-1, 1]
        END DO
        
        ! Allocate and initialize COO matrix
        CALL DCOOALLOC(m, n, nnz_actual, coo, info)
        IF (info /= 0) THEN
            DEALLOCATE(row_ind, col_ind, values, mask)
            RETURN
        END IF
        
        CALL DCOOINIT(row_ind(1:nnz_actual), col_ind(1:nnz_actual), &
                      values(1:nnz_actual), nnz_actual, coo, info)
        
        DEALLOCATE(row_ind, col_ind, values, mask)
    END SUBROUTINE create_random_sparse_matrix
    
    SUBROUTINE create_upper_triangular_matrix(n, coo, info)
        INTEGER, INTENT(IN) :: n
        TYPE(sparse_coo_d), INTENT(OUT) :: coo
        INTEGER, INTENT(OUT) :: info
        
        INTEGER :: i, j, nnz, idx
        INTEGER, ALLOCATABLE :: row_ind(:), col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        
        EXTERNAL :: DCOOALLOC, DCOOINIT
        
        ! Calculate number of non-zeros
        nnz = n*(n+1)/2
        
        ! Allocate arrays
        ALLOCATE(row_ind(nnz), col_ind(nnz), values(nnz))
        
        idx = 0
        DO i = 1, n
            DO j = i, n
                idx = idx + 1
                row_ind(idx) = i
                col_ind(idx) = j
                values(idx) = REAL(i+j, real64)
            END DO
        END DO
        
        ! Allocate and initialize COO matrix
        CALL DCOOALLOC(n, n, nnz, coo, info)
        IF (info /= 0) RETURN
        
        CALL DCOOINIT(row_ind, col_ind, values, nnz, coo, info)
        
        DEALLOCATE(row_ind, col_ind, values)
    END SUBROUTINE create_upper_triangular_matrix
    
    SUBROUTINE create_lower_triangular_matrix(n, coo, info)
        INTEGER, INTENT(IN) :: n
        TYPE(sparse_coo_d), INTENT(OUT) :: coo
        INTEGER, INTENT(OUT) :: info
        
        INTEGER :: i, j, nnz, idx
        INTEGER, ALLOCATABLE :: row_ind(:), col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        
        EXTERNAL :: DCOOALLOC, DCOOINIT
        
        ! Calculate number of non-zeros
        nnz = n*(n+1)/2
        
        ! Allocate arrays
        ALLOCATE(row_ind(nnz), col_ind(nnz), values(nnz))
        
        idx = 0
        DO i = 1, n
            DO j = 1, i
                idx = idx + 1
                row_ind(idx) = i
                col_ind(idx) = j
                values(idx) = REAL(i+j, real64)
            END DO
        END DO
        
        ! Allocate and initialize COO matrix
        CALL DCOOALLOC(n, n, nnz, coo, info)
        IF (info /= 0) RETURN
        
        CALL DCOOINIT(row_ind, col_ind, values, nnz, coo, info)
        
        DEALLOCATE(row_ind, col_ind, values)
    END SUBROUTINE create_lower_triangular_matrix
    
    LOGICAL FUNCTION test_identity_properties(coo) RESULT(is_identity)
        TYPE(sparse_coo_d), INTENT(IN) :: coo
        INTEGER :: k
        
        is_identity = .TRUE.
        
        ! Check square matrix
        IF (coo%nrows /= coo%ncols) THEN
            is_identity = .FALSE.
            RETURN
        END IF
        
        ! Check number of non-zeros
        IF (coo%nnz /= coo%nrows) THEN
            is_identity = .FALSE.
            RETURN
        END IF
        
        ! Check diagonal elements
        DO k = 1, coo%nnz
            IF (coo%row_ind(k) /= coo%col_ind(k)) THEN
                is_identity = .FALSE.
                RETURN
            END IF
            IF (ABS(coo%values(k) - 1.0_real64) > tol) THEN
                is_identity = .FALSE.
                RETURN
            END IF
        END DO
    END FUNCTION test_identity_properties
    
    LOGICAL FUNCTION test_tridiagonal_properties(coo, n) RESULT(is_tridiag)
        TYPE(sparse_coo_d), INTENT(IN) :: coo
        INTEGER, INTENT(IN) :: n
        INTEGER :: k, row, col
        
        is_tridiag = .TRUE.
        
        ! Check square matrix
        IF (coo%nrows /= n .OR. coo%ncols /= n) THEN
            is_tridiag = .FALSE.
            RETURN
        END IF
        
        ! Check all elements are on tri-diagonal
        DO k = 1, coo%nnz
            row = coo%row_ind(k)
            col = coo%col_ind(k)
            IF (ABS(row - col) > 1) THEN
                is_tridiag = .FALSE.
                RETURN
            END IF
        END DO
    END FUNCTION test_tridiagonal_properties
    
    SUBROUTINE test_coo_csr_roundtrip(coo_in, test_ok, info)
        TYPE(sparse_coo_d), INTENT(IN) :: coo_in
        LOGICAL, INTENT(OUT) :: test_ok
        INTEGER, INTENT(OUT) :: info
        
        TYPE(sparse_csr_d) :: csr
        TYPE(sparse_coo_d) :: coo_out
        INTEGER :: k
        
        EXTERNAL :: DCSRALLOC, DCOOALLOC, DCOO2CSR, DCSR2COO
        EXTERNAL :: DCSRFREE, DCOOFREE
        
        test_ok = .FALSE.
        
        ! Allocate CSR
        CALL DCSRALLOC(coo_in%nrows, coo_in%ncols, coo_in%nnz, csr, info)
        IF (info /= 0) RETURN
        
        ! Convert COO to CSR
        CALL DCOO2CSR(coo_in, csr, info)
        IF (info /= 0) THEN
            CALL DCSRFREE(csr, info)
            RETURN
        END IF
        
        ! Allocate output COO
        CALL DCOOALLOC(csr%nrows, csr%ncols, csr%nnz, coo_out, info)
        IF (info /= 0) THEN
            CALL DCSRFREE(csr, info)
            RETURN
        END IF
        
        ! Convert CSR back to COO
        CALL DCSR2COO(csr, coo_out, info)
        IF (info /= 0) THEN
            CALL DCSRFREE(csr, info)
            CALL DCOOFREE(coo_out, info)
            RETURN
        END IF
        
        ! Compare matrices
        test_ok = compare_coo_matrices(coo_in, coo_out)
        
        ! Clean up
        CALL DCSRFREE(csr, info)
        CALL DCOOFREE(coo_out, info)
    END SUBROUTINE test_coo_csr_roundtrip
    
    SUBROUTINE test_coo_csc_roundtrip(coo_in, test_ok, info)
        TYPE(sparse_coo_d), INTENT(IN) :: coo_in
        LOGICAL, INTENT(OUT) :: test_ok
        INTEGER, INTENT(OUT) :: info
        
        TYPE(sparse_csc_d) :: csc
        TYPE(sparse_coo_d) :: coo_out
        
        EXTERNAL :: DCSCALLOC, DCOOALLOC, DCOO2CSC, DCSC2COO
        EXTERNAL :: DCSCFREE, DCOOFREE
        
        test_ok = .FALSE.
        
        ! Allocate CSC
        CALL DCSCALLOC(coo_in%nrows, coo_in%ncols, coo_in%nnz, csc, info)
        IF (info /= 0) RETURN
        
        ! Convert COO to CSC
        CALL DCOO2CSC(coo_in, csc, info)
        IF (info /= 0) THEN
            CALL DCSCFREE(csc, info)
            RETURN
        END IF
        
        ! Allocate output COO
        CALL DCOOALLOC(csc%nrows, csc%ncols, csc%nnz, coo_out, info)
        IF (info /= 0) THEN
            CALL DCSCFREE(csc, info)
            RETURN
        END IF
        
        ! Convert CSC back to COO
        CALL DCSC2COO(csc, coo_out, info)
        IF (info /= 0) THEN
            CALL DCSCFREE(csc, info)
            CALL DCOOFREE(coo_out, info)
            RETURN
        END IF
        
        ! Compare matrices
        test_ok = compare_coo_matrices(coo_in, coo_out)
        
        ! Clean up
        CALL DCSCFREE(csc, info)
        CALL DCOOFREE(coo_out, info)
    END SUBROUTINE test_coo_csc_roundtrip
    
    LOGICAL FUNCTION compare_coo_matrices(coo1, coo2) RESULT(are_equal)
        TYPE(sparse_coo_d), INTENT(IN) :: coo1, coo2
        INTEGER :: i, j, k1, k2
        REAL(real64) :: val1, val2
        LOGICAL :: found
        
        are_equal = .TRUE.
        
        ! Check dimensions
        IF (coo1%nrows /= coo2%nrows .OR. coo1%ncols /= coo2%ncols) THEN
            are_equal = .FALSE.
            RETURN
        END IF
        
        ! Check number of non-zeros
        IF (coo1%nnz /= coo2%nnz) THEN
            are_equal = .FALSE.
            RETURN
        END IF
        
        ! Check each element in coo1 exists in coo2 with same value
        DO k1 = 1, coo1%nnz
            i = coo1%row_ind(k1)
            j = coo1%col_ind(k1)
            val1 = coo1%values(k1)
            
            found = .FALSE.
            DO k2 = 1, coo2%nnz
                IF (coo2%row_ind(k2) == i .AND. coo2%col_ind(k2) == j) THEN
                    val2 = coo2%values(k2)
                    IF (ABS(val1 - val2) < tol) THEN
                        found = .TRUE.
                        EXIT
                    END IF
                END IF
            END DO
            
            IF (.NOT. found) THEN
                are_equal = .FALSE.
                RETURN
            END IF
        END DO
    END FUNCTION compare_coo_matrices

END PROGRAM test_matrices