!> \brief \b SCHKSP
!>
!> \par Purpose:
!> =============
!>
!> SCHKSP tests the SINGLE PRECISION sparse matrix routines

SUBROUTINE SCHKSP(DOTYPE, NN, NVAL, NNZ, NZVAL, THRESH, TSTERR, &
                  NMAX, WORK, IWORK, NOUT)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real32
    IMPLICIT NONE
    
    ! Arguments
    LOGICAL :: TSTERR
    INTEGER :: NN, NNZ, NMAX, NOUT
    REAL :: THRESH
    LOGICAL :: DOTYPE(*)
    INTEGER :: NVAL(*), NZVAL(*), IWORK(*)
    REAL :: WORK(*)
    
    ! Parameters
    INTEGER, PARAMETER :: NTYPES = 10
    INTEGER, PARAMETER :: NTESTS = 8
    REAL, PARAMETER :: ONE = 1.0_real32
    REAL, PARAMETER :: ZERO = 0.0_real32
    
    ! Local variables
    CHARACTER(LEN=32) :: SRNAMT
    CHARACTER(LEN=3) :: PATH
    INTEGER :: I, IN, INZ, IMAT, INFO, N, NZ, NERRS, NTESTF
    INTEGER :: NRUN, NFAIL
    REAL :: ANORM, RCOND, RCONDC
    REAL :: RESULT(NTESTS)
    
    ! Sparse matrix structures
    TYPE(sparse_coo_s) :: COO
    TYPE(sparse_csr_s) :: CSR
    TYPE(sparse_csc_s) :: CSC
    
    ! External functions
    REAL, EXTERNAL :: SLAMCH
    LOGICAL, EXTERNAL :: LSAME
    
    ! Common blocks
    INTEGER :: INFOT, IOUNIT
    LOGICAL :: LERR, OK
    COMMON /INFOC/ INFOT, IOUNIT, OK, LERR
    COMMON /SRNAMC/ SRNAMT
    
    ! Initialize
    PATH = 'SSP'
    NRUN = 0
    NFAIL = 0
    NERRS = 0
    
    ! Test the error exits
    IF (TSTERR) CALL SERRSP(PATH, NOUT)
    INFOT = 0
    
    ! Print header
    WRITE(NOUT, 9999) PATH
    
    ! Loop over matrix sizes
    DO IN = 1, NN
        N = NVAL(IN)
        IF (N > NMAX) CYCLE
        
        ! Loop over number of non-zeros
        DO INZ = 1, NNZ
            NZ = NZVAL(INZ)
            IF (NZ > N*N) NZ = N*N
            
            ! Loop over matrix types
            DO IMAT = 1, NTYPES
                IF (.NOT. DOTYPE(IMAT)) CYCLE
                
                ! Generate test matrix
                CALL SLATSP(IMAT, N, NZ, COO, WORK, IWORK, INFO)
                IF (INFO /= 0) THEN
                    WRITE(NOUT, 9995) IMAT, N, NZ, INFO
                    CYCLE
                END IF
                
                ! Run tests similar to double precision
                ! ... (test implementations)
                
                ! Clean up
                CALL SCOOFREE(COO, INFO)
                
            END DO ! IMAT
        END DO ! INZ
    END DO ! IN
    
    ! Print summary
    IF (NFAIL > 0) THEN
        WRITE(NOUT, 9992) NFAIL, NRUN
    ELSE
        WRITE(NOUT, 9991) NRUN
    END IF
    
    RETURN
    
    ! Format statements
9999 FORMAT(1X, A3, ' -- Single precision sparse matrix test routines')
9995 FORMAT(' SLATSP: IMAT=', I3, ', N=', I5, ', NNZ=', I6, &
            ', INFO=', I3)
9992 FORMAT(/' *** ', I6, ' tests failed out of ', I6, ' ***')
9991 FORMAT(/' All tests passed (', I6, ' tests run)')
    
END SUBROUTINE SCHKSP