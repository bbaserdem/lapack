!> \brief \b DCHKSP
!>
!> \par Purpose:
!> =============
!>
!> DCHKSP tests the DOUBLE PRECISION sparse matrix routines:
!> COO format: DCOOALLOC, DCOOINIT, DCOOCONV, DCOOMV, etc.
!> CSR format: DCSRINIT, DCSRMV, DCSRCSC, etc.
!> CSC format: DCSCINIT, DCSCMV, etc.
!>
!> \param[in] DOTYPE
!>          DOTYPE is LOGICAL array, dimension (NTYPES)
!>          The matrix types to be used for testing.
!>
!> \param[in] NN
!>          NN is INTEGER
!>          The number of values of N contained in the vector NVAL.
!>
!> \param[in] NVAL
!>          NVAL is INTEGER array, dimension (NN)
!>          The values of the matrix dimension N.
!>
!> \param[in] NNZ
!>          NNZ is INTEGER
!>          The number of values of NNZ contained in the vector NZVAL.
!>
!> \param[in] NZVAL
!>          NZVAL is INTEGER array, dimension (NNZ)
!>          The values of the number of non-zeros.
!>
!> \param[in] THRESH
!>          THRESH is DOUBLE PRECISION
!>          The threshold value for the test ratios.
!>
!> \param[in] TSTERR
!>          TSTERR is LOGICAL
!>          Flag that indicates whether error exits are to be tested.
!>
!> \param[in] NMAX
!>          NMAX is INTEGER
!>          The maximum value permitted for N.
!>
!> \param[out] WORK
!>          WORK is DOUBLE PRECISION array
!>
!> \param[out] IWORK
!>          IWORK is INTEGER array
!>
!> \param[in] NOUT
!>          NOUT is INTEGER
!>          The unit number for output.

SUBROUTINE DCHKSP(DOTYPE, NN, NVAL, NNZ, NZVAL, THRESH, TSTERR, &
                  NMAX, WORK, IWORK, NOUT)
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64
    IMPLICIT NONE
    
    ! Arguments
    LOGICAL :: TSTERR
    INTEGER :: NN, NNZ, NMAX, NOUT
    DOUBLE PRECISION :: THRESH
    LOGICAL :: DOTYPE(*)
    INTEGER :: NVAL(*), NZVAL(*), IWORK(*)
    DOUBLE PRECISION :: WORK(*)
    
    ! Parameters
    INTEGER, PARAMETER :: NTYPES = 10
    INTEGER, PARAMETER :: NTESTS = 8
    DOUBLE PRECISION, PARAMETER :: ONE = 1.0_real64
    DOUBLE PRECISION, PARAMETER :: ZERO = 0.0_real64
    
    ! Local variables
    CHARACTER(LEN=32) :: SRNAMT
    CHARACTER(LEN=3) :: PATH
    INTEGER :: I, IN, INZ, IMAT, INFO, N, NZ, NERRS, NTESTF
    INTEGER :: NRUN, NFAIL
    DOUBLE PRECISION :: ANORM, RCOND, RCONDC
    DOUBLE PRECISION :: RESULT(NTESTS)
    
    ! Sparse matrix structures
    TYPE(sparse_coo_d) :: COO
    TYPE(sparse_csr_d) :: CSR
    TYPE(sparse_csc_d) :: CSC
    
    ! External functions
    DOUBLE PRECISION, EXTERNAL :: DLAMCH
    LOGICAL, EXTERNAL :: LSAME
    
    ! Intrinsic functions
    INTRINSIC :: MAX, MIN, SQRT
    
    ! Common blocks for error handling
    INTEGER :: INFOT, IOUNIT
    LOGICAL :: LERR, OK
    COMMON /INFOC/ INFOT, IOUNIT, OK, LERR
    COMMON /SRNAMC/ SRNAMT
    
    ! Initialize
    PATH = 'DSP'
    NRUN = 0
    NFAIL = 0
    NERRS = 0
    
    ! Test the error exits
    IF (TSTERR) CALL DERRSP(PATH, NOUT)
    INFOT = 0
    
    ! Print header
    WRITE(NOUT, 9999) PATH
    WRITE(NOUT, 9998)
    WRITE(NOUT, 9997)
    WRITE(NOUT, 9996)
    
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
                CALL DLATSP(IMAT, N, NZ, COO, WORK, IWORK, INFO)
                IF (INFO /= 0) THEN
                    WRITE(NOUT, 9995) IMAT, N, NZ, INFO
                    CYCLE
                END IF
                
                ! Test COO operations
                CALL DSPTEST01(COO, RESULT(1))  ! Test initialization
                CALL DSPTEST02(COO, RESULT(2))  ! Test SpMV
                CALL DSPTEST03(COO, RESULT(3))  ! Test conversions
                
                ! Convert to CSR and test
                CALL DCOOCONV('CSR', COO, CSR, INFO)
                IF (INFO == 0) THEN
                    CALL DSPTEST04(CSR, RESULT(4))  ! Test CSR SpMV
                    CALL DSPTEST05(CSR, RESULT(5))  ! Test CSR operations
                END IF
                
                ! Convert to CSC and test
                CALL DCOOCONV('CSC', COO, CSC, INFO)
                IF (INFO == 0) THEN
                    CALL DSPTEST06(CSC, RESULT(6))  ! Test CSC SpMV
                    CALL DSPTEST07(CSC, RESULT(7))  ! Test CSC operations
                END IF
                
                ! Test advanced operations
                CALL DSPTEST08(COO, CSR, CSC, RESULT(8))
                
                ! Print test results
                DO I = 1, NTESTS
                    IF (RESULT(I) >= THRESH) THEN
                        IF (NFAIL == 0) WRITE(NOUT, 9994)
                        WRITE(NOUT, 9993) IMAT, N, NZ, I, RESULT(I)
                        NFAIL = NFAIL + 1
                    END IF
                END DO
                NRUN = NRUN + NTESTS
                
                ! Clean up
                CALL DCOOFREE(COO, INFO)
                CALL DCSRFREE(CSR, INFO)
                CALL DCSCFREE(CSC, INFO)
                
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
9999 FORMAT(1X, A3, ' -- Double precision sparse matrix test routines')
9998 FORMAT(' Matrix types:')
9997 FORMAT('    1: Random sparse matrix',/, &
            '    2: Diagonal matrix',/, &
            '    3: Tridiagonal matrix',/, &
            '    4: Upper triangular',/, &
            '    5: Lower triangular',/, &
            '    6: Symmetric matrix',/, &
            '    7: Ill-conditioned matrix',/, &
            '    8: Identity matrix',/, &
            '    9: Zero matrix',/, &
            '   10: Dense converted to sparse')
9996 FORMAT(/' Test ratios:',/, &
            '    1: ||A - COO||_F / (||A||_F * eps)',/, &
            '    2: ||Ax - b||_∞ / (||A||_∞ * ||x||_∞ * eps)',/, &
            '    3: ||CSR - COO||_F / (||COO||_F * eps)',/, &
            '    4: ||CSR*x - COO*x||_∞ / (||COO*x||_∞ * eps)',/, &
            '    5: CSR operation accuracy',/, &
            '    6: ||CSC*x - COO*x||_∞ / (||COO*x||_∞ * eps)',/, &
            '    7: CSC operation accuracy',/, &
            '    8: Advanced operation accuracy')
9995 FORMAT(' DLATSP: IMAT=', I3, ', N=', I5, ', NNZ=', I6, &
            ', INFO=', I3)
9994 FORMAT(/' *** Tests failed ***')
9993 FORMAT(' IMAT=', I3, ', N=', I5, ', NNZ=', I6, ', test(', I2, &
            ')=', G12.5)
9992 FORMAT(/' *** ', I6, ' tests failed out of ', I6, ' ***')
9991 FORMAT(/' All tests passed (', I6, ' tests run)')
    
END SUBROUTINE DCHKSP