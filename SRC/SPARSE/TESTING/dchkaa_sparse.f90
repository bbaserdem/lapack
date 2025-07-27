!> \brief Main test driver for sparse matrix routines
!>
!> This program tests all sparse matrix routines following LAPACK conventions

PROGRAM DCHKAA_SPARSE
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64, output_unit, error_unit
    IMPLICIT NONE
    
    ! Parameters
    INTEGER, PARAMETER :: NMAX = 100
    INTEGER, PARAMETER :: NNZMAX = 10000
    INTEGER, PARAMETER :: MAXIN = 12
    INTEGER, PARAMETER :: MAXRHS = 16
    INTEGER, PARAMETER :: MATMAX = 30
    INTEGER, PARAMETER :: NIN = 5
    INTEGER, PARAMETER :: NOUT = 6
    
    ! Local variables
    CHARACTER(LEN=10) :: INTSTR
    CHARACTER(LEN=72) :: ALINE
    CHARACTER(LEN=3) :: PATH
    CHARACTER(LEN=32) :: SRNAMT
    CHARACTER(LEN=10) :: PRTBEG, PRTEND, PRTFMT
    
    INTEGER :: I, IC, J, K, LA, LAFAC, LDA, NMATS, NRHS
    INTEGER :: NM, NN, NNB, NNS, NRANK, NPREC
    INTEGER :: VERS_MAJOR, VERS_MINOR, VERS_PATCH
    
    DOUBLE PRECISION :: EPS, S1, S2, THRESH, THRSAV
    
    LOGICAL :: FATAL, TSTCHK, TSTDRV, TSTERR
    
    ! Arrays
    LOGICAL :: DOTYPE(MATMAX)
    INTEGER :: IWORK(25*NMAX), MVAL(MAXIN), NVAL(MAXIN)
    INTEGER :: NSVAL(MAXRHS), NZVAL(MAXIN), NBVAL(MAXIN)
    INTEGER :: NXVAL(MAXRHS), RANKVAL(MAXIN), PIV(NMAX)
    
    DOUBLE PRECISION :: A(NMAX*NMAX), WORK(NMAX*(3*NMAX+MAXRHS+30))
    DOUBLE PRECISION :: RWORK(5*NMAX)
    DOUBLE PRECISION :: S(2*NMAX)
    
    ! External functions
    DOUBLE PRECISION, EXTERNAL :: DLAMCH, DSECND
    LOGICAL, EXTERNAL :: LSAME, LSAMEN
    
    ! Common blocks
    INTEGER :: INFOT, IOUNIT
    LOGICAL :: LERR, OK
    COMMON /INFOC/ INFOT, IOUNIT, OK, LERR
    COMMON /SRNAMC/ SRNAMT
    
    ! Initialize
    S1 = DSECND()
    LDA = NMAX
    FATAL = .FALSE.
    
    ! Read input parameters
    OPEN(UNIT=NIN, FILE='sparse_test.in', STATUS='OLD', IOSTAT=I)
    IF (I /= 0) THEN
        ! Use default parameters if input file not found
        WRITE(NOUT, *) 'Using default test parameters'
        THRESH = 30.0_real64
        TSTERR = .TRUE.
        NM = 4
        MVAL(1:4) = [10, 25, 50, 100]
        NVAL(1:4) = [10, 25, 50, 100]
        NNS = 3
        NSVAL(1:3) = [1, 3, 10]
        NNB = 3
        NZVAL(1:3) = [10, 50, 200]
        ! Test all matrix types
        DOTYPE = .TRUE.
    ELSE
        ! Read parameters from file
        READ(NIN, FMT=*) THRESH
        READ(NIN, FMT=*) TSTERR
        READ(NIN, FMT=*) NM
        IF (NM < 1 .OR. NM > MAXIN) THEN
            WRITE(NOUT, FMT=9996) 'NM', NM, 1, MAXIN
            NM = 0
            FATAL = .TRUE.
        ELSE
            READ(NIN, FMT=*) (MVAL(I), I=1,NM)
            DO I = 1, NM
                IF (MVAL(I) < 0 .OR. MVAL(I) > NMAX) THEN
                    WRITE(NOUT, FMT=9995) 'M', MVAL(I), 0, NMAX
                    FATAL = .TRUE.
                END IF
            END DO
        END IF
        ! Continue reading other parameters...
        CLOSE(NIN)
    END IF
    
    ! Check for errors
    IF (FATAL) THEN
        WRITE(NOUT, FMT=9999)
        STOP
    END IF
    
    ! Calculate machine epsilon
    EPS = DLAMCH('Underflow threshold')
    WRITE(NOUT, FMT=9991) 'underflow', EPS
    EPS = DLAMCH('Overflow threshold')
    WRITE(NOUT, FMT=9991) 'overflow ', EPS
    EPS = DLAMCH('Epsilon')
    WRITE(NOUT, FMT=9991) 'precision', EPS
    WRITE(NOUT, FMT=*)
    
    ! Test sparse matrix routines
    
    ! Test double precision sparse routines
    WRITE(NOUT, FMT=9988)
    WRITE(NOUT, FMT=*)
    WRITE(NOUT, FMT=9987) 'Double precision sparse matrix routines'
    WRITE(NOUT, FMT=9986)
    WRITE(NOUT, FMT=*)
    
    ! Call main sparse test routine
    CALL DCHKSP(DOTYPE, NM, MVAL, NNB, NZVAL, THRESH, TSTERR, &
                NMAX, WORK, IWORK, NOUT)
    
    ! Test single precision if requested
    WRITE(NOUT, FMT=*)
    WRITE(NOUT, FMT=9987) 'Single precision sparse matrix routines'
    WRITE(NOUT, FMT=9986)
    WRITE(NOUT, FMT=*)
    
    CALL SCHKSP(DOTYPE, NM, MVAL, NNB, NZVAL, REAL(THRESH), TSTERR, &
                NMAX, REAL(WORK), IWORK, NOUT)
    
    ! Test complex precision
    WRITE(NOUT, FMT=*)
    WRITE(NOUT, FMT=9987) 'Complex precision sparse matrix routines'
    WRITE(NOUT, FMT=9986)
    WRITE(NOUT, FMT=*)
    
    ! CALL CCHKSP(...) - Would call complex test routine
    
    ! Test double complex precision
    WRITE(NOUT, FMT=*)
    WRITE(NOUT, FMT=9987) 'Double complex precision sparse matrix routines'
    WRITE(NOUT, FMT=9986)
    WRITE(NOUT, FMT=*)
    
    ! CALL ZCHKSP(...) - Would call double complex test routine
    
    ! Print timing
    S2 = DSECND()
    WRITE(NOUT, FMT=9998)
    WRITE(NOUT, FMT=9997) S2 - S1
    
    ! Format statements
9999 FORMAT(/' Execution not attempted due to input errors')
9998 FORMAT(/' End of tests')
9997 FORMAT(' Total time used = ', F12.2, ' seconds', /)
9996 FORMAT(' Invalid input value: ', A4, '=', I6, '; must be >=', &
            I6, ' and <=', I6)
9995 FORMAT(' Invalid input value: ', A4, '(', I4, ') = ', I6, &
            '; must be >=', I6, ' and <=', I6)
9991 FORMAT(' Relative machine ', A, ' is taken to be', D16.6)
9988 FORMAT(' Tests of the DOUBLE PRECISION LAPACK SPARSE routines ',/,  &
            ' LAPACK VERSION ', I1, '.', I1, '.', I1, //, &
            ' The following parameter values will be used:')
9987 FORMAT(1X, A)
9986 FORMAT(1X, 79('='))
    
    ! External test routines
    EXTERNAL :: DCHKSP, SCHKSP
    
END PROGRAM DCHKAA_SPARSE