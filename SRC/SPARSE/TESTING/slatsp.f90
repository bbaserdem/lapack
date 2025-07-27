!> \brief \b SLATSP generates test sparse matrices (single precision)

SUBROUTINE SLATSP(IMAT, N, NNZ, COO, WORK, IWORK, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real32
    IMPLICIT NONE
    
    ! Arguments
    INTEGER :: IMAT, N, NNZ, INFO
    TYPE(sparse_coo_s), INTENT(OUT) :: COO
    REAL :: WORK(*)
    INTEGER :: IWORK(*)
    
    ! Local variables
    INTEGER :: I, J, K, NZ_ACTUAL, IERR
    REAL :: TEMP, ANORM
    
    ! External functions (would use SLARAN in real LAPACK)
    REAL :: R
    
    ! Initialize
    INFO = 0
    
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
    CALL SCOOALLOC(N, N, MAX(NNZ, N), COO, IERR)
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
            CALL RANDOM_NUMBER(R)
            IWORK(K+1) = INT(R * N) + 1  ! Row
            CALL RANDOM_NUMBER(R)
            IWORK(K+2) = INT(R * N) + 1  ! Col
            CALL RANDOM_NUMBER(R)
            WORK(K+1) = 2.0_real32 * R - 1.0_real32  ! Value in [-1,1]
            K = K + 1
        END DO
        
    CASE (2)
        ! Diagonal matrix
        NZ_ACTUAL = N
        DO I = 1, N
            IWORK(K+1) = I  ! Row
            IWORK(K+2) = I  ! Col
            CALL RANDOM_NUMBER(R)
            WORK(K+1) = 2.0_real32 * R - 1.0_real32
            K = K + 1
        END DO
        
    ! ... (other cases similar to double precision)
        
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
    ELSE
        COO%nnz = 0
    END IF
    
    ! Mark as unsorted
    COO%sorted = .FALSE.
    COO%checked = .FALSE.
    
END SUBROUTINE SLATSP