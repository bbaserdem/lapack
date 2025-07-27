!> \brief Edge case and boundary condition tests for sparse matrices

PROGRAM DSPEDGE
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64, output_unit
    IMPLICIT NONE
    
    ! Local variables
    INTEGER :: INFO, NTEST, NPASS, NFAIL
    CHARACTER(LEN=80) :: TEST_NAME
    
    ! Initialize counters
    NTEST = 0
    NPASS = 0
    NFAIL = 0
    
    WRITE(output_unit, '(A)') 'Sparse Matrix Edge Case Tests'
    WRITE(output_unit, '(A)') '============================='
    WRITE(output_unit, *)
    
    ! Test 1: Empty matrix (0x0)
    TEST_NAME = 'Empty matrix (0x0)'
    CALL TEST_EMPTY_MATRIX(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
    
    ! Test 2: Zero matrix (NxN with 0 non-zeros)
    TEST_NAME = 'Zero matrix (10x10, 0 nnz)'
    CALL TEST_ZERO_MATRIX(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
    
    ! Test 3: Single element matrix
    TEST_NAME = 'Single element matrix (1x1)'
    CALL TEST_SINGLE_ELEMENT(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
    
    ! Test 4: Row vector (1xN)
    TEST_NAME = 'Row vector sparse matrix (1x100)'
    CALL TEST_ROW_VECTOR(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
    
    ! Test 5: Column vector (Nx1)
    TEST_NAME = 'Column vector sparse matrix (100x1)'
    CALL TEST_COLUMN_VECTOR(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
    
    ! Test 6: Dense matrix as sparse
    TEST_NAME = 'Dense matrix stored as sparse'
    CALL TEST_DENSE_AS_SPARSE(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
    
    ! Test 7: Maximum size allocation
    TEST_NAME = 'Maximum size allocation test'
    CALL TEST_MAX_SIZE(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
    
    ! Test 8: Duplicate entries
    TEST_NAME = 'Duplicate entries handling'
    CALL TEST_DUPLICATES(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
    
    ! Test 9: Numerical edge cases
    TEST_NAME = 'Numerical edge cases (overflow/underflow)'
    CALL TEST_NUMERICAL_EDGES(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
    
    ! Test 10: Pattern matrices
    TEST_NAME = 'Special pattern matrices'
    CALL TEST_PATTERNS(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
    
    ! Print summary
    WRITE(output_unit, *)
    WRITE(output_unit, '(A)') 'Test Summary'
    WRITE(output_unit, '(A)') '============'
    WRITE(output_unit, '(A, I4)') 'Total tests: ', NTEST
    WRITE(output_unit, '(A, I4)') 'Passed:      ', NPASS
    WRITE(output_unit, '(A, I4)') 'Failed:      ', NFAIL
    
    IF (NFAIL == 0) THEN
        WRITE(output_unit, *)
        WRITE(output_unit, '(A)') 'All edge case tests PASSED!'
    END IF
    
CONTAINS

    SUBROUTINE REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS, NFAIL)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(IN) :: INFO
        INTEGER, INTENT(INOUT) :: NTEST, NPASS, NFAIL
        
        NTEST = NTEST + 1
        IF (INFO == 0) THEN
            NPASS = NPASS + 1
            WRITE(output_unit, '(A50, A)') TEST_NAME, ' ... PASSED'
        ELSE
            NFAIL = NFAIL + 1
            WRITE(output_unit, '(A50, A, I6)') TEST_NAME, ' ... FAILED (INFO=', INFO, ')'
        END IF
    END SUBROUTINE REPORT_TEST
    
    !> Test empty 0x0 matrix
    SUBROUTINE TEST_EMPTY_MATRIX(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        TYPE(sparse_csr_d) :: CSR
        REAL(real64) :: X(1), Y(1)
        
        INFO = 0
        
        ! Allocate 0x0 matrix
        CALL DCOOALLOC(0, 0, 0, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Test SpMV with empty matrix
        X(1) = 1.0_real64
        Y(1) = 2.0_real64
        CALL DCOOMV('N', 0, 0, 1.0_real64, COO, X, 1, 0.0_real64, Y, 1)
        
        ! Convert to CSR
        CALL DCOOCONV('CSR', COO, CSR, INFO)
        IF (INFO /= 0) RETURN
        
        ! Clean up
        CALL DCOOFREE(COO, INFO)
        CALL DCSRFREE(CSR, INFO)
        
    END SUBROUTINE TEST_EMPTY_MATRIX
    
    !> Test zero matrix (non-empty dimensions but no non-zeros)
    SUBROUTINE TEST_ZERO_MATRIX(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        REAL(real64) :: X(10), Y(10), Y_REF(10)
        INTEGER :: I
        
        INFO = 0
        
        ! Allocate 10x10 matrix with 0 non-zeros
        CALL DCOOALLOC(10, 10, 0, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Set nnz to 0 explicitly
        COO%nnz = 0
        
        ! Test SpMV - should give zero result
        X = 1.0_real64
        Y = 2.0_real64
        Y_REF = 0.0_real64  ! Expected: y = 0*x + 0*y = 0
        
        CALL DCOOMV('N', 10, 10, 1.0_real64, COO, X, 1, 0.0_real64, Y, 1)
        
        ! Check result
        DO I = 1, 10
            IF (ABS(Y(I) - Y_REF(I)) > 1.0E-14_real64) THEN
                INFO = -1
                RETURN
            END IF
        END DO
        
        CALL DCOOFREE(COO, INFO)
        
    END SUBROUTINE TEST_ZERO_MATRIX
    
    !> Test 1x1 matrix
    SUBROUTINE TEST_SINGLE_ELEMENT(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        INTEGER :: ROWIND(1), COLIND(1)
        REAL(real64) :: VAL(1), X(1), Y(1)
        
        INFO = 0
        
        ! Create 1x1 matrix with value 3.0
        CALL DCOOALLOC(1, 1, 1, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ROWIND(1) = 1
        COLIND(1) = 1
        VAL(1) = 3.0_real64
        
        CALL DCOOINIT(ROWIND, COLIND, VAL, 1, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Test SpMV: y = 3*2 = 6
        X(1) = 2.0_real64
        Y(1) = 0.0_real64
        
        CALL DCOOMV('N', 1, 1, 1.0_real64, COO, X, 1, 0.0_real64, Y, 1)
        
        IF (ABS(Y(1) - 6.0_real64) > 1.0E-14_real64) THEN
            INFO = -1
        END IF
        
        CALL DCOOFREE(COO, INFO)
        
    END SUBROUTINE TEST_SINGLE_ELEMENT
    
    !> Test row vector
    SUBROUTINE TEST_ROW_VECTOR(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        INTEGER :: ROWIND(5), COLIND(5), I
        REAL(real64) :: VAL(5), X(100), Y(1)
        
        INFO = 0
        
        ! Create 1x100 sparse row vector
        CALL DCOOALLOC(1, 100, 5, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Set some non-zeros in the row
        ROWIND = 1
        COLIND = [1, 25, 50, 75, 100]
        VAL = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        
        CALL DCOOINIT(ROWIND, COLIND, VAL, 5, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Test SpMV
        X = 1.0_real64
        Y = 0.0_real64
        
        CALL DCOOMV('N', 1, 100, 1.0_real64, COO, X, 1, 0.0_real64, Y, 1)
        
        ! Check result: sum of values = 15
        IF (ABS(Y(1) - 15.0_real64) > 1.0E-14_real64) THEN
            INFO = -1
        END IF
        
        CALL DCOOFREE(COO, INFO)
        
    END SUBROUTINE TEST_ROW_VECTOR
    
    !> Test column vector
    SUBROUTINE TEST_COLUMN_VECTOR(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        TYPE(sparse_csc_d) :: CSC
        INTEGER :: ROWIND(5), COLIND(5), I
        REAL(real64) :: VAL(5), X(1), Y(100)
        
        INFO = 0
        
        ! Create 100x1 sparse column vector
        CALL DCOOALLOC(100, 1, 5, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Set some non-zeros in the column
        ROWIND = [1, 25, 50, 75, 100]
        COLIND = 1
        VAL = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        
        CALL DCOOINIT(ROWIND, COLIND, VAL, 5, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Convert to CSC (should be efficient for column vector)
        CALL DCOOCONV('CSC', COO, CSC, INFO)
        IF (INFO /= 0) RETURN
        
        ! Test SpMV
        X(1) = 2.0_real64
        Y = 0.0_real64
        
        CALL DCSCMV('N', 100, 1, 1.0_real64, CSC, X, 1, 0.0_real64, Y, 1)
        
        ! Check specific elements
        IF (ABS(Y(1) - 2.0_real64) > 1.0E-14_real64) INFO = -1
        IF (ABS(Y(25) - 4.0_real64) > 1.0E-14_real64) INFO = -1
        IF (ABS(Y(50) - 6.0_real64) > 1.0E-14_real64) INFO = -1
        
        CALL DCOOFREE(COO, INFO)
        CALL DCSCFREE(CSC, INFO)
        
    END SUBROUTINE TEST_COLUMN_VECTOR
    
    !> Test dense matrix stored as sparse
    SUBROUTINE TEST_DENSE_AS_SPARSE(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        TYPE(sparse_csr_d) :: CSR
        INTEGER :: N, I, J, K
        INTEGER, ALLOCATABLE :: ROWIND(:), COLIND(:)
        REAL(real64), ALLOCATABLE :: VAL(:), X(:), Y(:), Y_DENSE(:)
        
        INFO = 0
        N = 10  ! Small dense matrix
        
        ! Allocate for NxN dense matrix
        ALLOCATE(ROWIND(N*N), COLIND(N*N), VAL(N*N))
        ALLOCATE(X(N), Y(N), Y_DENSE(N))
        
        ! Fill as dense matrix
        K = 0
        DO J = 1, N
            DO I = 1, N
                K = K + 1
                ROWIND(K) = I
                COLIND(K) = J
                VAL(K) = REAL(I + J, real64)
            END DO
        END DO
        
        CALL DCOOALLOC(N, N, N*N, COO, INFO)
        IF (INFO /= 0) RETURN
        
        CALL DCOOINIT(ROWIND, COLIND, VAL, N*N, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Convert to CSR (should handle dense case)
        CALL DCOOCONV('CSR', COO, CSR, INFO)
        IF (INFO /= 0) RETURN
        
        ! Compare with dense computation
        X = 1.0_real64
        Y = 0.0_real64
        Y_DENSE = 0.0_real64
        
        ! Dense computation
        DO J = 1, N
            DO I = 1, N
                Y_DENSE(I) = Y_DENSE(I) + REAL(I + J, real64) * X(J)
            END DO
        END DO
        
        ! Sparse computation
        CALL DCSRMV('N', N, N, 1.0_real64, CSR, X, 1, 0.0_real64, Y, 1)
        
        ! Compare
        DO I = 1, N
            IF (ABS(Y(I) - Y_DENSE(I)) > 1.0E-12_real64) THEN
                INFO = -1
                EXIT
            END IF
        END DO
        
        CALL DCOOFREE(COO, INFO)
        CALL DCSRFREE(CSR, INFO)
        DEALLOCATE(ROWIND, COLIND, VAL, X, Y, Y_DENSE)
        
    END SUBROUTINE TEST_DENSE_AS_SPARSE
    
    !> Test maximum size allocation
    SUBROUTINE TEST_MAX_SIZE(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        INTEGER :: N, NNZ
        
        INFO = 0
        
        ! Try to allocate a very large sparse matrix
        N = 10000
        NNZ = 100000
        
        CALL DCOOALLOC(N, N, NNZ, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Check allocation
        IF (COO%nnz_alloc < NNZ) THEN
            INFO = -1
        END IF
        
        CALL DCOOFREE(COO, INFO)
        
    END SUBROUTINE TEST_MAX_SIZE
    
    !> Test duplicate entry handling
    SUBROUTINE TEST_DUPLICATES(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        INTEGER :: ROWIND(5), COLIND(5)
        REAL(real64) :: VAL(5), X(3), Y(3)
        
        INFO = 0
        
        ! Create matrix with duplicate entries
        CALL DCOOALLOC(3, 3, 5, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Intentional duplicates at (2,2)
        ROWIND = [1, 2, 2, 2, 3]
        COLIND = [1, 2, 2, 3, 3]
        VAL = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        
        CALL DCOOINIT(ROWIND, COLIND, VAL, 5, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Test with duplicates - should sum duplicate values
        X = [1.0_real64, 1.0_real64, 1.0_real64]
        Y = 0.0_real64
        
        CALL DCOOMV('N', 3, 3, 1.0_real64, COO, X, 1, 0.0_real64, Y, 1)
        
        ! Clean up with SPCOMP to remove duplicates
        CALL DSPCOMP(COO, INFO)
        
        CALL DCOOFREE(COO, INFO)
        
    END SUBROUTINE TEST_DUPLICATES
    
    !> Test numerical edge cases
    SUBROUTINE TEST_NUMERICAL_EDGES(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        INTEGER :: ROWIND(4), COLIND(4)
        REAL(real64) :: VAL(4), X(2), Y(2)
        REAL(real64) :: HUGE_VAL, TINY_VAL
        
        INFO = 0
        
        ! Get machine constants
        HUGE_VAL = HUGE(1.0_real64) / 1.0E10_real64  ! Avoid overflow
        TINY_VAL = TINY(1.0_real64) * 1.0E10_real64  ! Avoid underflow
        
        ! Create 2x2 matrix with extreme values
        CALL DCOOALLOC(2, 2, 4, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ROWIND = [1, 1, 2, 2]
        COLIND = [1, 2, 1, 2]
        VAL = [HUGE_VAL, TINY_VAL, -TINY_VAL, -HUGE_VAL]
        
        CALL DCOOINIT(ROWIND, COLIND, VAL, 4, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Test with normal values
        X = [1.0_real64, -1.0_real64]
        Y = 0.0_real64
        
        CALL DCOOMV('N', 2, 2, 1.0_real64, COO, X, 1, 0.0_real64, Y, 1)
        
        ! Result should be approximately [HUGE_VAL - TINY_VAL, -TINY_VAL + HUGE_VAL]
        IF (.NOT. (ABS(Y(1) + Y(2)) < 1.0E-10_real64 * HUGE_VAL)) THEN
            INFO = -1
        END IF
        
        CALL DCOOFREE(COO, INFO)
        
    END SUBROUTINE TEST_NUMERICAL_EDGES
    
    !> Test special pattern matrices
    SUBROUTINE TEST_PATTERNS(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        TYPE(sparse_csr_d) :: CSR
        INTEGER :: N, I
        INTEGER, ALLOCATABLE :: ROWIND(:), COLIND(:)
        REAL(real64), ALLOCATABLE :: VAL(:), X(:), Y(:)
        
        INFO = 0
        N = 100
        
        ! Create a banded matrix pattern
        ALLOCATE(ROWIND(3*N-2), COLIND(3*N-2), VAL(3*N-2))
        ALLOCATE(X(N), Y(N))
        
        ! Tridiagonal pattern
        K = 0
        ! Lower diagonal
        DO I = 2, N
            K = K + 1
            ROWIND(K) = I
            COLIND(K) = I-1
            VAL(K) = -1.0_real64
        END DO
        ! Main diagonal
        DO I = 1, N
            K = K + 1
            ROWIND(K) = I
            COLIND(K) = I
            VAL(K) = 2.0_real64
        END DO
        ! Upper diagonal
        DO I = 1, N-1
            K = K + 1
            ROWIND(K) = I
            COLIND(K) = I+1
            VAL(K) = -1.0_real64
        END DO
        
        CALL DCOOALLOC(N, N, 3*N-2, COO, INFO)
        IF (INFO /= 0) RETURN
        
        CALL DCOOINIT(ROWIND, COLIND, VAL, 3*N-2, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Convert to CSR - should be efficient for row-wise access
        CALL DCOOCONV('CSR', COO, CSR, INFO)
        IF (INFO /= 0) RETURN
        
        ! Test with constant vector
        X = 1.0_real64
        Y = 0.0_real64
        
        CALL DCSRMV('N', N, N, 1.0_real64, CSR, X, 1, 0.0_real64, Y, 1)
        
        ! Check interior points (should be 0)
        DO I = 2, N-1
            IF (ABS(Y(I)) > 1.0E-14_real64) THEN
                INFO = -1
                EXIT
            END IF
        END DO
        
        CALL DCOOFREE(COO, INFO)
        CALL DCSRFREE(CSR, INFO)
        DEALLOCATE(ROWIND, COLIND, VAL, X, Y)
        
    END SUBROUTINE TEST_PATTERNS

END PROGRAM DSPEDGE