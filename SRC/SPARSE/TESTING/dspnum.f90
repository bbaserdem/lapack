!> \brief Numerical accuracy tests for sparse matrix routines
!>
!> Tests numerical accuracy, stability, and precision of sparse operations

PROGRAM DSPNUM
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64, output_unit
    IMPLICIT NONE
    
    ! Parameters
    INTEGER, PARAMETER :: NTEST_SIZES = 4
    INTEGER :: TEST_SIZES(NTEST_SIZES) = [10, 50, 100, 500]
    
    ! Local variables
    INTEGER :: I, N, INFO, NTEST, NPASS
    REAL(real64) :: EPS, COND, MAX_ERROR
    CHARACTER(LEN=80) :: TEST_NAME
    
    ! External functions
    REAL(real64), EXTERNAL :: DLAMCH
    
    ! Get machine epsilon
    EPS = DLAMCH('E')
    
    ! Initialize
    NTEST = 0
    NPASS = 0
    
    WRITE(output_unit, '(A)') 'Sparse Matrix Numerical Accuracy Tests'
    WRITE(output_unit, '(A)') '======================================'
    WRITE(output_unit, *)
    WRITE(output_unit, '(A, E12.4)') 'Machine epsilon: ', EPS
    WRITE(output_unit, *)
    
    ! Test 1: Associativity of SpMV
    WRITE(output_unit, '(A)') 'Test 1: Associativity (A*(x+y) = A*x + A*y)'
    WRITE(output_unit, '(A)') '-------------------------------------------'
    DO I = 1, NTEST_SIZES
        N = TEST_SIZES(I)
        CALL TEST_ASSOCIATIVITY(N, MAX_ERROR, INFO)
        NTEST = NTEST + 1
        IF (INFO == 0 .AND. MAX_ERROR < 100.0_real64 * EPS) THEN
            NPASS = NPASS + 1
            WRITE(output_unit, '(A, I4, A, E12.4, A)') &
                'N=', N, ', Max error: ', MAX_ERROR, ' ... PASSED'
        ELSE
            WRITE(output_unit, '(A, I4, A, E12.4, A)') &
                'N=', N, ', Max error: ', MAX_ERROR, ' ... FAILED'
        END IF
    END DO
    WRITE(output_unit, *)
    
    ! Test 2: Transpose consistency
    WRITE(output_unit, '(A)') 'Test 2: Transpose consistency (y^T*A*x = x^T*A^T*y)'
    WRITE(output_unit, '(A)') '---------------------------------------------------'
    DO I = 1, NTEST_SIZES
        N = TEST_SIZES(I)
        CALL TEST_TRANSPOSE(N, MAX_ERROR, INFO)
        NTEST = NTEST + 1
        IF (INFO == 0 .AND. MAX_ERROR < 100.0_real64 * EPS) THEN
            NPASS = NPASS + 1
            WRITE(output_unit, '(A, I4, A, E12.4, A)') &
                'N=', N, ', Max error: ', MAX_ERROR, ' ... PASSED'
        ELSE
            WRITE(output_unit, '(A, I4, A, E12.4, A)') &
                'N=', N, ', Max error: ', MAX_ERROR, ' ... FAILED'
        END IF
    END DO
    WRITE(output_unit, *)
    
    ! Test 3: Format conversion accuracy
    WRITE(output_unit, '(A)') 'Test 3: Format conversion accuracy'
    WRITE(output_unit, '(A)') '----------------------------------'
    DO I = 1, NTEST_SIZES
        N = TEST_SIZES(I)
        CALL TEST_FORMAT_CONVERSION(N, MAX_ERROR, INFO)
        NTEST = NTEST + 1
        IF (INFO == 0 .AND. MAX_ERROR < 10.0_real64 * EPS) THEN
            NPASS = NPASS + 1
            WRITE(output_unit, '(A, I4, A, E12.4, A)') &
                'N=', N, ', Max error: ', MAX_ERROR, ' ... PASSED'
        ELSE
            WRITE(output_unit, '(A, I4, A, E12.4, A)') &
                'N=', N, ', Max error: ', MAX_ERROR, ' ... FAILED'
        END IF
    END DO
    WRITE(output_unit, *)
    
    ! Test 4: Ill-conditioned matrices
    WRITE(output_unit, '(A)') 'Test 4: Ill-conditioned matrices'
    WRITE(output_unit, '(A)') '---------------------------------'
    DO I = 1, 4
        COND = 10.0_real64**(2*I)  ! 10^2, 10^4, 10^6, 10^8
        CALL TEST_ILL_CONDITIONED(50, COND, MAX_ERROR, INFO)
        NTEST = NTEST + 1
        IF (INFO == 0 .AND. MAX_ERROR < COND * EPS * 10.0_real64) THEN
            NPASS = NPASS + 1
            WRITE(output_unit, '(A, E10.2, A, E12.4, A)') &
                'Condition=', COND, ', Max error: ', MAX_ERROR, ' ... PASSED'
        ELSE
            WRITE(output_unit, '(A, E10.2, A, E12.4, A)') &
                'Condition=', COND, ', Max error: ', MAX_ERROR, ' ... FAILED'
        END IF
    END DO
    WRITE(output_unit, *)
    
    ! Test 5: Orthogonal matrix preservation
    WRITE(output_unit, '(A)') 'Test 5: Orthogonal matrix preservation (||Q*x||_2 = ||x||_2)'
    WRITE(output_unit, '(A)') '------------------------------------------------------------'
    DO I = 1, NTEST_SIZES
        N = TEST_SIZES(I)
        CALL TEST_ORTHOGONAL(N, MAX_ERROR, INFO)
        NTEST = NTEST + 1
        IF (INFO == 0 .AND. MAX_ERROR < 100.0_real64 * EPS) THEN
            NPASS = NPASS + 1
            WRITE(output_unit, '(A, I4, A, E12.4, A)') &
                'N=', N, ', Max error: ', MAX_ERROR, ' ... PASSED'
        ELSE
            WRITE(output_unit, '(A, I4, A, E12.4, A)') &
                'N=', N, ', Max error: ', MAX_ERROR, ' ... FAILED'
        END IF
    END DO
    WRITE(output_unit, *)
    
    ! Summary
    WRITE(output_unit, '(A)') 'Summary'
    WRITE(output_unit, '(A)') '-------'
    WRITE(output_unit, '(A, I3, A, I3)') 'Tests passed: ', NPASS, ' out of ', NTEST
    IF (NPASS == NTEST) THEN
        WRITE(output_unit, '(A)') 'All numerical accuracy tests PASSED!'
    END IF
    
CONTAINS

    !> Test associativity of SpMV
    SUBROUTINE TEST_ASSOCIATIVITY(N, MAX_ERROR, INFO)
        INTEGER, INTENT(IN) :: N
        REAL(real64), INTENT(OUT) :: MAX_ERROR
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: A
        REAL(real64), ALLOCATABLE :: X(:), Y(:), Z(:), AX(:), AY(:), AXY(:), AZ(:)
        INTEGER :: I, NNZ
        REAL(real64) :: TEMP
        
        ! External functions
        REAL(real64), EXTERNAL :: DNRM2
        
        INFO = 0
        MAX_ERROR = 0.0_real64
        
        ! Create a random sparse matrix
        NNZ = MIN(N*5, N*N/4)  ! Moderate sparsity
        CALL CREATE_RANDOM_MATRIX(N, N, NNZ, A, INFO)
        IF (INFO /= 0) RETURN
        
        ! Allocate vectors
        ALLOCATE(X(N), Y(N), Z(N), AX(N), AY(N), AXY(N), AZ(N))
        
        ! Initialize random vectors
        DO I = 1, N
            CALL RANDOM_NUMBER(TEMP)
            X(I) = 2.0_real64 * TEMP - 1.0_real64
            CALL RANDOM_NUMBER(TEMP)
            Y(I) = 2.0_real64 * TEMP - 1.0_real64
        END DO
        
        ! Compute z = x + y
        DO I = 1, N
            Z(I) = X(I) + Y(I)
        END DO
        
        ! Compute A*x
        CALL DCOOMV('N', N, N, 1.0_real64, A, X, 1, 0.0_real64, AX, 1)
        
        ! Compute A*y
        CALL DCOOMV('N', N, N, 1.0_real64, A, Y, 1, 0.0_real64, AY, 1)
        
        ! Compute A*(x+y)
        CALL DCOOMV('N', N, N, 1.0_real64, A, Z, 1, 0.0_real64, AZ, 1)
        
        ! Compute A*x + A*y
        DO I = 1, N
            AXY(I) = AX(I) + AY(I)
        END DO
        
        ! Compare A*(x+y) with A*x + A*y
        MAX_ERROR = 0.0_real64
        DO I = 1, N
            TEMP = ABS(AZ(I) - AXY(I))
            IF (TEMP > MAX_ERROR) MAX_ERROR = TEMP
        END DO
        
        ! Normalize by magnitude
        TEMP = DNRM2(N, AZ, 1)
        IF (TEMP > 0.0_real64) MAX_ERROR = MAX_ERROR / TEMP
        
        ! Clean up
        CALL DCOOFREE(A, INFO)
        DEALLOCATE(X, Y, Z, AX, AY, AXY, AZ)
        
    END SUBROUTINE TEST_ASSOCIATIVITY
    
    !> Test transpose consistency
    SUBROUTINE TEST_TRANSPOSE(N, MAX_ERROR, INFO)
        INTEGER, INTENT(IN) :: N
        REAL(real64), INTENT(OUT) :: MAX_ERROR
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: A
        REAL(real64), ALLOCATABLE :: X(:), Y(:), AX(:), ATY(:)
        REAL(real64) :: YTAX, XTATY, TEMP
        INTEGER :: I, NNZ
        
        ! External functions
        REAL(real64), EXTERNAL :: DDOT
        
        INFO = 0
        
        ! Create a random sparse matrix
        NNZ = MIN(N*5, N*N/4)
        CALL CREATE_RANDOM_MATRIX(N, N, NNZ, A, INFO)
        IF (INFO /= 0) RETURN
        
        ! Allocate vectors
        ALLOCATE(X(N), Y(N), AX(N), ATY(N))
        
        ! Initialize random vectors
        DO I = 1, N
            CALL RANDOM_NUMBER(TEMP)
            X(I) = 2.0_real64 * TEMP - 1.0_real64
            CALL RANDOM_NUMBER(TEMP)
            Y(I) = 2.0_real64 * TEMP - 1.0_real64
        END DO
        
        ! Compute A*x
        CALL DCOOMV('N', N, N, 1.0_real64, A, X, 1, 0.0_real64, AX, 1)
        
        ! Compute A^T*y
        CALL DCOOMV('T', N, N, 1.0_real64, A, Y, 1, 0.0_real64, ATY, 1)
        
        ! Compute y^T*(A*x)
        YTAX = DDOT(N, Y, 1, AX, 1)
        
        ! Compute x^T*(A^T*y)
        XTATY = DDOT(N, X, 1, ATY, 1)
        
        ! Compare
        MAX_ERROR = ABS(YTAX - XTATY) / MAX(ABS(YTAX), ABS(XTATY), 1.0_real64)
        
        ! Clean up
        CALL DCOOFREE(A, INFO)
        DEALLOCATE(X, Y, AX, ATY)
        
    END SUBROUTINE TEST_TRANSPOSE
    
    !> Test format conversion accuracy
    SUBROUTINE TEST_FORMAT_CONVERSION(N, MAX_ERROR, INFO)
        INTEGER, INTENT(IN) :: N
        REAL(real64), INTENT(OUT) :: MAX_ERROR
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        TYPE(sparse_csr_d) :: CSR
        TYPE(sparse_csc_d) :: CSC
        REAL(real64), ALLOCATABLE :: X(:), Y_COO(:), Y_CSR(:), Y_CSC(:)
        REAL(real64) :: TEMP, ERR1, ERR2
        INTEGER :: I, NNZ
        
        ! External functions
        REAL(real64), EXTERNAL :: DNRM2
        
        INFO = 0
        
        ! Create test matrix
        NNZ = MIN(N*5, N*N/4)
        CALL CREATE_RANDOM_MATRIX(N, N, NNZ, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Convert to other formats
        CALL DCOOCONV('CSR', COO, CSR, INFO)
        IF (INFO /= 0) RETURN
        
        CALL DCOOCONV('CSC', COO, CSC, INFO)
        IF (INFO /= 0) RETURN
        
        ! Allocate vectors
        ALLOCATE(X(N), Y_COO(N), Y_CSR(N), Y_CSC(N))
        
        ! Random vector
        DO I = 1, N
            CALL RANDOM_NUMBER(TEMP)
            X(I) = 2.0_real64 * TEMP - 1.0_real64
        END DO
        
        ! Compute SpMV with each format
        CALL DCOOMV('N', N, N, 1.0_real64, COO, X, 1, 0.0_real64, Y_COO, 1)
        CALL DCSRMV('N', N, N, 1.0_real64, CSR, X, 1, 0.0_real64, Y_CSR, 1)
        CALL DCSCMV('N', N, N, 1.0_real64, CSC, X, 1, 0.0_real64, Y_CSC, 1)
        
        ! Compare results
        ERR1 = 0.0_real64
        ERR2 = 0.0_real64
        DO I = 1, N
            ERR1 = MAX(ERR1, ABS(Y_COO(I) - Y_CSR(I)))
            ERR2 = MAX(ERR2, ABS(Y_COO(I) - Y_CSC(I)))
        END DO
        
        MAX_ERROR = MAX(ERR1, ERR2)
        TEMP = DNRM2(N, Y_COO, 1)
        IF (TEMP > 0.0_real64) MAX_ERROR = MAX_ERROR / TEMP
        
        ! Clean up
        CALL DCOOFREE(COO, INFO)
        CALL DCSRFREE(CSR, INFO)
        CALL DCSCFREE(CSC, INFO)
        DEALLOCATE(X, Y_COO, Y_CSR, Y_CSC)
        
    END SUBROUTINE TEST_FORMAT_CONVERSION
    
    !> Test with ill-conditioned matrices
    SUBROUTINE TEST_ILL_CONDITIONED(N, COND, MAX_ERROR, INFO)
        INTEGER, INTENT(IN) :: N
        REAL(real64), INTENT(IN) :: COND
        REAL(real64), INTENT(OUT) :: MAX_ERROR
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: A
        REAL(real64), ALLOCATABLE :: X(:), B(:), Y(:)
        REAL(real64) :: TEMP, SCALE
        INTEGER :: I, K
        INTEGER, ALLOCATABLE :: ROW_IND(:), COL_IND(:)
        REAL(real64), ALLOCATABLE :: VALUES(:)
        
        ! External functions
        REAL(real64), EXTERNAL :: DNRM2
        
        INFO = 0
        
        ! Create diagonal matrix with specified condition number
        ALLOCATE(ROW_IND(N), COL_IND(N), VALUES(N))
        ALLOCATE(X(N), B(N), Y(N))
        
        ! Set diagonal values to create specified condition number
        DO I = 1, N
            ROW_IND(I) = I
            COL_IND(I) = I
            ! Logarithmic spacing of singular values
            SCALE = COND**(REAL(I-1, real64) / REAL(N-1, real64))
            VALUES(I) = 1.0_real64 / SCALE
        END DO
        
        ! Create sparse matrix
        CALL DCOOALLOC(N, N, N, A, INFO)
        IF (INFO /= 0) RETURN
        
        CALL DCOOINIT(ROW_IND, COL_IND, VALUES, N, A, INFO)
        IF (INFO /= 0) RETURN
        
        ! Create exact solution
        DO I = 1, N
            X(I) = 1.0_real64 / REAL(I, real64)
        END DO
        
        ! Compute b = A*x
        CALL DCOOMV('N', N, N, 1.0_real64, A, X, 1, 0.0_real64, B, 1)
        
        ! Solve by computing y = A^(-1)*b (since A is diagonal)
        DO I = 1, N
            Y(I) = B(I) / VALUES(I)
        END DO
        
        ! Compute error
        MAX_ERROR = 0.0_real64
        DO I = 1, N
            MAX_ERROR = MAX(MAX_ERROR, ABS(Y(I) - X(I)))
        END DO
        
        TEMP = DNRM2(N, X, 1)
        IF (TEMP > 0.0_real64) MAX_ERROR = MAX_ERROR / TEMP
        
        ! Clean up
        CALL DCOOFREE(A, INFO)
        DEALLOCATE(ROW_IND, COL_IND, VALUES, X, B, Y)
        
    END SUBROUTINE TEST_ILL_CONDITIONED
    
    !> Test orthogonal matrix properties
    SUBROUTINE TEST_ORTHOGONAL(N, MAX_ERROR, INFO)
        INTEGER, INTENT(IN) :: N
        REAL(real64), INTENT(OUT) :: MAX_ERROR
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: Q
        REAL(real64), ALLOCATABLE :: X(:), QX(:)
        REAL(real64) :: NORM_X, NORM_QX, THETA, C, S
        INTEGER :: I, K, NNZ
        INTEGER, ALLOCATABLE :: ROW_IND(:), COL_IND(:)
        REAL(real64), ALLOCATABLE :: VALUES(:)
        
        ! External functions
        REAL(real64), EXTERNAL :: DNRM2
        
        INFO = 0
        
        ! Create a sparse orthogonal matrix (Givens rotations)
        ! Apply rotations to pairs of rows
        NNZ = 4 * (N/2)  ! Each 2x2 rotation has 4 non-zeros
        ALLOCATE(ROW_IND(NNZ), COL_IND(NNZ), VALUES(NNZ))
        ALLOCATE(X(N), QX(N))
        
        K = 0
        DO I = 1, N-1, 2
            ! Random rotation angle
            CALL RANDOM_NUMBER(THETA)
            THETA = THETA * 2.0_real64 * 3.14159265358979323846_real64
            C = COS(THETA)
            S = SIN(THETA)
            
            ! Fill rotation matrix entries
            K = K + 1
            ROW_IND(K) = I
            COL_IND(K) = I
            VALUES(K) = C
            
            K = K + 1
            ROW_IND(K) = I
            COL_IND(K) = I+1
            VALUES(K) = -S
            
            K = K + 1
            ROW_IND(K) = I+1
            COL_IND(K) = I
            VALUES(K) = S
            
            K = K + 1
            ROW_IND(K) = I+1
            COL_IND(K) = I+1
            VALUES(K) = C
        END DO
        
        ! Handle odd N
        IF (MOD(N, 2) == 1) THEN
            K = K + 1
            ROW_IND(K) = N
            COL_IND(K) = N
            VALUES(K) = 1.0_real64
        END IF
        
        ! Create sparse orthogonal matrix
        CALL DCOOALLOC(N, N, K, Q, INFO)
        IF (INFO /= 0) RETURN
        
        CALL DCOOINIT(ROW_IND, COL_IND, VALUES, K, Q, INFO)
        IF (INFO /= 0) RETURN
        
        ! Test with random vector
        DO I = 1, N
            CALL RANDOM_NUMBER(X(I))
            X(I) = 2.0_real64 * X(I) - 1.0_real64
        END DO
        
        ! Compute norms
        NORM_X = DNRM2(N, X, 1)
        
        ! Compute Q*x
        CALL DCOOMV('N', N, N, 1.0_real64, Q, X, 1, 0.0_real64, QX, 1)
        NORM_QX = DNRM2(N, QX, 1)
        
        ! Check norm preservation
        MAX_ERROR = ABS(NORM_QX - NORM_X) / NORM_X
        
        ! Clean up
        CALL DCOOFREE(Q, INFO)
        DEALLOCATE(ROW_IND, COL_IND, VALUES, X, QX)
        
    END SUBROUTINE TEST_ORTHOGONAL
    
    !> Helper routine to create random sparse matrix
    SUBROUTINE CREATE_RANDOM_MATRIX(M, N, NNZ, A, INFO)
        INTEGER, INTENT(IN) :: M, N, NNZ
        TYPE(sparse_coo_d), INTENT(OUT) :: A
        INTEGER, INTENT(OUT) :: INFO
        
        INTEGER :: I, J, K
        INTEGER, ALLOCATABLE :: ROW_IND(:), COL_IND(:)
        REAL(real64), ALLOCATABLE :: VALUES(:)
        REAL(real64) :: TEMP
        LOGICAL :: FOUND
        
        INFO = 0
        
        ALLOCATE(ROW_IND(NNZ), COL_IND(NNZ), VALUES(NNZ))
        
        ! Generate random entries
        DO K = 1, NNZ
            FOUND = .TRUE.
            DO WHILE (FOUND)
                CALL RANDOM_NUMBER(TEMP)
                I = INT(TEMP * M) + 1
                CALL RANDOM_NUMBER(TEMP)
                J = INT(TEMP * N) + 1
                
                ! Check for duplicates
                FOUND = .FALSE.
                DO L = 1, K-1
                    IF (ROW_IND(L) == I .AND. COL_IND(L) == J) THEN
                        FOUND = .TRUE.
                        EXIT
                    END IF
                END DO
            END DO
            
            ROW_IND(K) = I
            COL_IND(K) = J
            CALL RANDOM_NUMBER(TEMP)
            VALUES(K) = 2.0_real64 * TEMP - 1.0_real64
        END DO
        
        ! Create sparse matrix
        CALL DCOOALLOC(M, N, NNZ, A, INFO)
        IF (INFO /= 0) RETURN
        
        CALL DCOOINIT(ROW_IND, COL_IND, VALUES, NNZ, A, INFO)
        
        DEALLOCATE(ROW_IND, COL_IND, VALUES)
        
    END SUBROUTINE CREATE_RANDOM_MATRIX

END PROGRAM DSPNUM