!> \brief Test SpMV accuracy against dense DGEMV
!>
!> This program compares sparse matrix-vector multiplication results
!> with dense BLAS DGEMV to ensure correctness

PROGRAM test_spmv_accuracy
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64, output_unit, error_unit
    IMPLICIT NONE
    
    ! Test parameters
    INTEGER, PARAMETER :: M = 50, N = 40
    INTEGER, PARAMETER :: MAX_NNZ = M * N
    REAL(real64), PARAMETER :: tol = 1.0E-12_real64
    
    ! Matrices and vectors
    TYPE(sparse_coo_d) :: coo
    TYPE(sparse_csr_d) :: csr  
    TYPE(sparse_csc_d) :: csc
    REAL(real64), ALLOCATABLE :: dense(:,:), x(:), y_sparse(:), y_dense(:), y_ref(:)
    
    ! Test variables
    INTEGER :: info, i, j, k
    INTEGER :: test_count, pass_count
    REAL(real64) :: alpha, beta, error, max_error
    CHARACTER :: trans
    LOGICAL :: test_ok
    
    ! External routines
    EXTERNAL :: DGEMV  ! BLAS routine
    EXTERNAL :: DCOOMV, DCSRMV, DCSCMV
    EXTERNAL :: DCOOALLOC, DCOOFREE, DCSRALLOC, DCSRFREE, DCSCALLOC, DCSCFREE
    EXTERNAL :: DDEN2COO, DCOO2DEN, DCOO2CSR, DCOO2CSC
    
    WRITE(output_unit, '(A)') '============================================='
    WRITE(output_unit, '(A)') 'Testing SpMV Accuracy Against Dense DGEMV'
    WRITE(output_unit, '(A)') '============================================='
    
    test_count = 0
    pass_count = 0
    
    ! Allocate arrays
    ALLOCATE(dense(M,N), x(MAX(M,N)), y_sparse(MAX(M,N)), y_dense(MAX(M,N)), y_ref(MAX(M,N)))
    
    ! Create a test matrix with known pattern
    CALL create_test_matrix(M, N, dense)
    
    ! Convert to COO format
    CALL DCOOALLOC(M, N, MAX_NNZ, coo, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to allocate COO matrix, INFO = ', info
        STOP 1
    END IF
    
    CALL DDEN2COO(dense, M, M, N, coo, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to convert dense to COO, INFO = ', info
        STOP 1
    END IF
    
    WRITE(output_unit, '(A,I0,A,I0)') 'Matrix size: ', M, ' x ', N
    WRITE(output_unit, '(A,I0)') 'Non-zeros: ', coo%nnz
    WRITE(output_unit, '(A,F6.2,A)') 'Density: ', &
        REAL(coo%nnz, real64) / REAL(M*N, real64) * 100.0_real64, '%'
    
    ! Test 1: COO SpMV vs DGEMV (no transpose)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 1: COO SpMV vs DGEMV (y = alpha*A*x + beta*y)'
    
    trans = 'N'
    alpha = 2.5_real64
    beta = -1.5_real64
    
    ! Initialize vectors
    CALL init_test_vector(N, x)
    CALL init_test_vector(M, y_sparse)
    y_dense = y_sparse
    y_ref = y_sparse
    
    ! Dense matrix-vector multiply (reference)
    CALL DGEMV(trans, M, N, alpha, dense, M, x, 1, beta, y_dense, 1)
    
    ! Sparse matrix-vector multiply
    CALL DCOOMV(trans, M, N, alpha, coo, x, 1, beta, y_sparse, 1)
    
    ! Compare results
    max_error = 0.0_real64
    DO i = 1, M
        error = ABS(y_sparse(i) - y_dense(i))
        max_error = MAX(max_error, error)
    END DO
    
    test_ok = (max_error < tol)
    IF (test_ok) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,E12.5)') 'PASSED - Max error: ', max_error
    ELSE
        WRITE(error_unit, '(A,E12.5)') 'FAILED - Max error: ', max_error
        CALL print_vector_diff(M, y_sparse, y_dense)
    END IF
    
    ! Test 2: COO SpMV vs DGEMV (transpose)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 2: COO SpMV vs DGEMV (y = alpha*A^T*x + beta*y)'
    
    trans = 'T'
    alpha = 1.0_real64
    beta = 0.0_real64
    
    ! Initialize vectors
    CALL init_test_vector(M, x)
    y_sparse = 0.0_real64
    y_dense = 0.0_real64
    
    ! Dense matrix-vector multiply (reference)
    CALL DGEMV(trans, M, N, alpha, dense, M, x, 1, beta, y_dense, 1)
    
    ! Sparse matrix-vector multiply
    CALL DCOOMV(trans, M, N, alpha, coo, x, 1, beta, y_sparse, 1)
    
    ! Compare results
    max_error = 0.0_real64
    DO i = 1, N
        error = ABS(y_sparse(i) - y_dense(i))
        max_error = MAX(max_error, error)
    END DO
    
    test_ok = (max_error < tol)
    IF (test_ok) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,E12.5)') 'PASSED - Max error: ', max_error
    ELSE
        WRITE(error_unit, '(A,E12.5)') 'FAILED - Max error: ', max_error
        CALL print_vector_diff(N, y_sparse, y_dense)
    END IF
    
    ! Convert to CSR and test
    CALL DCSRALLOC(M, N, coo%nnz, csr, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to allocate CSR matrix, INFO = ', info
        STOP 1
    END IF
    
    CALL DCOO2CSR(coo, csr, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to convert COO to CSR, INFO = ', info
        STOP 1
    END IF
    
    ! Test 3: CSR SpMV vs DGEMV (no transpose)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 3: CSR SpMV vs DGEMV (y = alpha*A*x + beta*y)'
    
    trans = 'N'
    alpha = -1.0_real64
    beta = 2.0_real64
    
    ! Initialize vectors
    CALL init_test_vector(N, x)
    CALL init_test_vector(M, y_sparse)
    y_dense = y_sparse
    
    ! Dense matrix-vector multiply (reference)
    CALL DGEMV(trans, M, N, alpha, dense, M, x, 1, beta, y_dense, 1)
    
    ! Sparse matrix-vector multiply
    CALL DCSRMV(trans, M, N, alpha, csr, x, 1, beta, y_sparse, 1)
    
    ! Compare results
    max_error = 0.0_real64
    DO i = 1, M
        error = ABS(y_sparse(i) - y_dense(i))
        max_error = MAX(max_error, error)
    END DO
    
    test_ok = (max_error < tol)
    IF (test_ok) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,E12.5)') 'PASSED - Max error: ', max_error
    ELSE
        WRITE(error_unit, '(A,E12.5)') 'FAILED - Max error: ', max_error
    END IF
    
    ! Convert to CSC and test
    CALL DCSCALLOC(M, N, coo%nnz, csc, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to allocate CSC matrix, INFO = ', info
        STOP 1
    END IF
    
    CALL DCOO2CSC(coo, csc, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to convert COO to CSC, INFO = ', info
        STOP 1
    END IF
    
    ! Test 4: CSC SpMV vs DGEMV (no transpose)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 4: CSC SpMV vs DGEMV (y = alpha*A*x + beta*y)'
    
    trans = 'N'
    alpha = 3.0_real64
    beta = -0.5_real64
    
    ! Initialize vectors
    CALL init_test_vector(N, x)
    CALL init_test_vector(M, y_sparse)
    y_dense = y_sparse
    
    ! Dense matrix-vector multiply (reference)
    CALL DGEMV(trans, M, N, alpha, dense, M, x, 1, beta, y_dense, 1)
    
    ! Sparse matrix-vector multiply
    CALL DCSCMV(trans, M, N, alpha, csc, x, 1, beta, y_sparse, 1)
    
    ! Compare results
    max_error = 0.0_real64
    DO i = 1, M
        error = ABS(y_sparse(i) - y_dense(i))
        max_error = MAX(max_error, error)
    END DO
    
    test_ok = (max_error < tol)
    IF (test_ok) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,E12.5)') 'PASSED - Max error: ', max_error
    ELSE
        WRITE(error_unit, '(A,E12.5)') 'FAILED - Max error: ', max_error
    END IF
    
    ! Test 5: Special cases (alpha=0, beta=0)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 5: Special cases (alpha=0, beta=1)'
    
    trans = 'N'
    alpha = 0.0_real64
    beta = 1.0_real64
    
    ! Initialize vectors
    CALL init_test_vector(N, x)
    CALL init_test_vector(M, y_sparse)
    y_dense = y_sparse
    y_ref = y_sparse  ! Should remain unchanged
    
    ! Dense matrix-vector multiply (reference)
    CALL DGEMV(trans, M, N, alpha, dense, M, x, 1, beta, y_dense, 1)
    
    ! Sparse matrix-vector multiply
    CALL DCOOMV(trans, M, N, alpha, coo, x, 1, beta, y_sparse, 1)
    
    ! Both should equal original y
    max_error = 0.0_real64
    DO i = 1, M
        error = ABS(y_sparse(i) - y_ref(i))
        max_error = MAX(max_error, error)
    END DO
    
    test_ok = (max_error < tol)
    IF (test_ok) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,E12.5)') 'PASSED - Max error: ', max_error
    ELSE
        WRITE(error_unit, '(A,E12.5)') 'FAILED - Max error: ', max_error
    END IF
    
    ! Test 6: Zero matrix test
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 6: Zero matrix (beta scaling only)'
    
    ! Create zero matrix
    dense = 0.0_real64
    CALL DCOOFREE(coo, info)
    CALL DCOOALLOC(M, N, 1, coo, info)  ! Allocate minimal space
    coo%nnz = 0  ! No non-zeros
    
    trans = 'N'
    alpha = 1.0_real64
    beta = 2.0_real64
    
    ! Initialize vectors
    CALL init_test_vector(N, x)
    CALL init_test_vector(M, y_sparse)
    y_dense = y_sparse
    
    ! Dense matrix-vector multiply (reference)
    CALL DGEMV(trans, M, N, alpha, dense, M, x, 1, beta, y_dense, 1)
    
    ! Sparse matrix-vector multiply
    CALL DCOOMV(trans, M, N, alpha, coo, x, 1, beta, y_sparse, 1)
    
    ! Compare results
    max_error = 0.0_real64
    DO i = 1, M
        error = ABS(y_sparse(i) - y_dense(i))
        max_error = MAX(max_error, error)
    END DO
    
    test_ok = (max_error < tol)
    IF (test_ok) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,E12.5)') 'PASSED - Max error: ', max_error
    ELSE
        WRITE(error_unit, '(A,E12.5)') 'FAILED - Max error: ', max_error
    END IF
    
    ! Summary
    WRITE(output_unit, '(/A)') '============================================='
    WRITE(output_unit, '(A,I0,A,I0)') 'Tests passed: ', pass_count, ' out of ', test_count
    WRITE(output_unit, '(A)') '============================================='
    
    ! Clean up
    CALL DCOOFREE(coo, info)
    CALL DCSRFREE(csr, info)
    CALL DCSCFREE(csc, info)
    DEALLOCATE(dense, x, y_sparse, y_dense, y_ref)
    
CONTAINS

    SUBROUTINE create_test_matrix(m, n, A)
        INTEGER, INTENT(IN) :: m, n
        REAL(real64), INTENT(OUT) :: A(m,n)
        INTEGER :: i, j
        REAL(real64) :: val
        
        ! Create a sparse matrix with specific pattern
        A = 0.0_real64
        
        ! Diagonal elements
        DO i = 1, MIN(m,n)
            A(i,i) = REAL(i, real64)
        END DO
        
        ! Some off-diagonal elements
        DO i = 1, m-1
            DO j = i+1, MIN(i+3, n)
                val = REAL(i+j, real64) / 10.0_real64
                IF (MOD(i+j, 3) == 0) A(i,j) = val
            END DO
        END DO
        
        DO j = 1, n-1
            DO i = j+1, MIN(j+2, m)
                val = -REAL(i*j, real64) / 20.0_real64
                IF (MOD(i*j, 4) == 0) A(i,j) = val
            END DO
        END DO
    END SUBROUTINE create_test_matrix
    
    SUBROUTINE init_test_vector(n, x)
        INTEGER, INTENT(IN) :: n
        REAL(real64), INTENT(OUT) :: x(n)
        INTEGER :: i
        
        DO i = 1, n
            x(i) = SIN(REAL(i, real64)) + 0.5_real64
        END DO
    END SUBROUTINE init_test_vector
    
    SUBROUTINE print_vector_diff(n, v1, v2)
        INTEGER, INTENT(IN) :: n
        REAL(real64), INTENT(IN) :: v1(n), v2(n)
        INTEGER :: i
        
        WRITE(output_unit, '(A)') 'Vector differences (first 10 elements):'
        DO i = 1, MIN(n, 10)
            WRITE(output_unit, '(I4,3E15.6)') i, v1(i), v2(i), v1(i)-v2(i)
        END DO
    END SUBROUTINE print_vector_diff

END PROGRAM test_spmv_accuracy