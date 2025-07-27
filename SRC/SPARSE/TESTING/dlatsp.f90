!> \brief \b DLATSP generates test sparse matrices
!>
!> \par Purpose:
!> =============
!>
!> DLATSP generates sparse test matrices of various types for testing
!> sparse matrix routines.

SUBROUTINE DLATSP(IMAT, N, NNZ, COO, WORK, IWORK, INFO)
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64
    IMPLICIT NONE
    
    ! Arguments
    INTEGER :: IMAT, N, NNZ, INFO
    TYPE(sparse_coo_d), INTENT(OUT) :: COO
    DOUBLE PRECISION :: WORK(*)
    INTEGER :: IWORK(*)
    
    ! Local variables
    INTEGER :: I, J, K, NZ_ACTUAL, IERR
    DOUBLE PRECISION :: TEMP, ANORM
    LOGICAL :: UPPER, LOWER, DIAG, SYM
    
    ! External functions
    DOUBLE PRECISION, EXTERNAL :: DLARAN
    INTEGER, EXTERNAL :: IDAMAX
    
    ! Initialize
    INFO = 0
    UPPER = .FALSE.
    LOWER = .FALSE.
    DIAG = .FALSE.
    SYM = .FALSE.
    
    ! Check arguments
    IF (N < 0) THEN
        INFO = -2
        RETURN
    END IF
    IF (NNZ < 0 .OR. NNZ > N*N) THEN
        INFO = -3
        RETURN
    END IF
    
    ! Allocate COO matrix
    CALL DCOOALLOC(N, N, MAX(NNZ, N), COO, IERR)
    IF (IERR /= 0) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Initialize arrays for matrix generation
    K = 0
    
    SELECT CASE (IMAT)
        
    CASE (1)
        ! Random sparse matrix
        NZ_ACTUAL = MIN(NNZ, N*N)
        DO I = 1, NZ_ACTUAL
            ! Generate random position
            IWORK(K+1) = INT(DLARAN(IWORK) * N) + 1  ! Row
            IWORK(K+2) = INT(DLARAN(IWORK) * N) + 1  ! Col
            WORK(K+1) = 2.0_real64 * DLARAN(IWORK) - 1.0_real64  ! Value in [-1,1]
            K = K + 1
        END DO
        
    CASE (2)
        ! Diagonal matrix
        NZ_ACTUAL = N
        DO I = 1, N
            IWORK(K+1) = I  ! Row
            IWORK(K+2) = I  ! Col
            WORK(K+1) = 2.0_real64 * DLARAN(IWORK) - 1.0_real64
            K = K + 1
        END DO
        
    CASE (3)
        ! Tridiagonal matrix
        NZ_ACTUAL = MIN(3*N-2, NNZ)
        ! Main diagonal
        DO I = 1, N
            IWORK(K+1) = I
            IWORK(K+2) = I
            WORK(K+1) = 2.0_real64
            K = K + 1
        END DO
        ! Sub-diagonal
        IF (NZ_ACTUAL >= 2*N-1) THEN
            DO I = 2, N
                IWORK(K+1) = I
                IWORK(K+2) = I-1
                WORK(K+1) = -1.0_real64
                K = K + 1
            END DO
        END IF
        ! Super-diagonal
        IF (NZ_ACTUAL >= 3*N-2) THEN
            DO I = 1, N-1
                IWORK(K+1) = I
                IWORK(K+2) = I+1
                WORK(K+1) = -1.0_real64
                K = K + 1
            END DO
        END IF
        
    CASE (4)
        ! Upper triangular matrix
        NZ_ACTUAL = MIN(N*(N+1)/2, NNZ)
        K = 0
        DO J = 1, N
            DO I = 1, MIN(J, NZ_ACTUAL - K)
                IWORK(K+1) = I
                IWORK(K+2) = J
                WORK(K+1) = 2.0_real64 * DLARAN(IWORK) - 1.0_real64
                K = K + 1
                IF (K >= NZ_ACTUAL) EXIT
            END DO
            IF (K >= NZ_ACTUAL) EXIT
        END DO
        
    CASE (5)
        ! Lower triangular matrix
        NZ_ACTUAL = MIN(N*(N+1)/2, NNZ)
        K = 0
        DO J = 1, N
            DO I = J, MIN(N, J + NZ_ACTUAL - K - 1)
                IWORK(K+1) = I
                IWORK(K+2) = J
                WORK(K+1) = 2.0_real64 * DLARAN(IWORK) - 1.0_real64
                K = K + 1
                IF (K >= NZ_ACTUAL) EXIT
            END DO
            IF (K >= NZ_ACTUAL) EXIT
        END DO
        
    CASE (6)
        ! Symmetric matrix
        NZ_ACTUAL = MIN(N*(N+1)/2, NNZ)
        K = 0
        DO J = 1, N
            DO I = J, MIN(N, J + NZ_ACTUAL - K - 1)
                IWORK(K+1) = I
                IWORK(K+2) = J
                WORK(K+1) = 2.0_real64 * DLARAN(IWORK) - 1.0_real64
                K = K + 1
                IF (K >= NZ_ACTUAL) EXIT
            END DO
            IF (K >= NZ_ACTUAL) EXIT
        END DO
        
    CASE (7)
        ! Ill-conditioned matrix (diagonal with large condition number)
        NZ_ACTUAL = N
        DO I = 1, N
            IWORK(K+1) = I
            IWORK(K+2) = I
            ! Create exponentially decreasing diagonal
            WORK(K+1) = 10.0_real64**(-(I-1)*2.0_real64/N)
            K = K + 1
        END DO
        
    CASE (8)
        ! Identity matrix
        NZ_ACTUAL = N
        DO I = 1, N
            IWORK(K+1) = I
            IWORK(K+2) = I
            WORK(K+1) = 1.0_real64
            K = K + 1
        END DO
        
    CASE (9)
        ! Zero matrix
        NZ_ACTUAL = 0
        
    CASE (10)
        ! Dense matrix converted to sparse
        NZ_ACTUAL = MIN(N*N, NNZ)
        K = 0
        DO J = 1, N
            DO I = 1, N
                IF (K < NZ_ACTUAL) THEN
                    IWORK(K+1) = I
                    IWORK(K+2) = J
                    WORK(K+1) = 2.0_real64 * DLARAN(IWORK) - 1.0_real64
                    K = K + 1
                END IF
            END DO
        END DO
        
    CASE DEFAULT
        INFO = -1
        RETURN
        
    END SELECT
    
    ! Initialize the COO matrix with generated data
    IF (NZ_ACTUAL > 0) THEN
        ! Copy row indices
        DO I = 1, NZ_ACTUAL
            COO%row_ind(I) = IWORK(I*2-1)
            COO%col_ind(I) = IWORK(I*2)
            COO%values(I) = WORK(I)
        END DO
        COO%nnz = NZ_ACTUAL
        
        ! For symmetric matrices, add the transpose elements
        IF (IMAT == 6 .AND. NZ_ACTUAL < NNZ) THEN
            K = NZ_ACTUAL
            DO I = 1, NZ_ACTUAL
                IF (COO%row_ind(I) /= COO%col_ind(I) .AND. K < NNZ) THEN
                    K = K + 1
                    COO%row_ind(K) = COO%col_ind(I)
                    COO%col_ind(K) = COO%row_ind(I)
                    COO%values(K) = COO%values(I)
                END IF
            END DO
            COO%nnz = K
        END IF
    ELSE
        COO%nnz = 0
    END IF
    
    ! Mark as unsorted
    COO%sorted = .FALSE.
    COO%checked = .FALSE.
    
END SUBROUTINE DLATSP