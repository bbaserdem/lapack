!> \brief \b SERRSP tests error exits for single precision sparse routines

SUBROUTINE SERRSP(PATH, NUNIT)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real32
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER(LEN=3) :: PATH
    INTEGER :: NUNIT
    
    ! Local variables
    CHARACTER(LEN=32) :: SRNAMT
    INTEGER :: INFO, I, J
    TYPE(sparse_coo_s) :: COO
    TYPE(sparse_csr_s) :: CSR
    TYPE(sparse_csc_s) :: CSC
    
    ! Arrays for testing
    INTEGER :: ROWIND(10), COLIND(10), ROWPTR(10), COLPTR(10)
    REAL :: VAL(10), X(10), Y(10)
    
    ! Common blocks
    INTEGER :: INFOT, IOUNIT
    LOGICAL :: LERR, OK
    COMMON /INFOC/ INFOT, IOUNIT, OK, LERR
    COMMON /SRNAMC/ SRNAMT
    
    ! External subroutines
    EXTERNAL :: ALAESM, CHKXER
    
    ! Initialize
    IOUNIT = NUNIT
    OK = .TRUE.
    
    ! Set up test data
    DO I = 1, 10
        ROWIND(I) = I
        COLIND(I) = I
        VAL(I) = REAL(I)
        X(I) = 1.0_real32
        Y(I) = 0.0_real32
    END DO
    ROWPTR(1:6) = [1, 3, 5, 7, 9, 11]
    COLPTR(1:6) = [1, 3, 5, 7, 9, 11]
    
    WRITE(NUNIT, *)
    WRITE(NUNIT, *) 'Testing error exits for single precision sparse matrix routines:'
    WRITE(NUNIT, *)
    
    ! Test SCOOALLOC error exits
    SRNAMT = 'SCOOALLOC'
    INFOT = 1
    CALL SCOOALLOC(-1, 10, 10, COO, INFO)
    CALL CHKXER('SCOOALLOC', INFOT, NUNIT, LERR, OK)
    
    INFOT = 2
    CALL SCOOALLOC(10, -1, 10, COO, INFO)
    CALL CHKXER('SCOOALLOC', INFOT, NUNIT, LERR, OK)
    
    INFOT = 3
    CALL SCOOALLOC(10, 10, -1, COO, INFO)
    CALL CHKXER('SCOOALLOC', INFOT, NUNIT, LERR, OK)
    
    ! Allocate COO for further tests
    CALL SCOOALLOC(10, 10, 10, COO, INFO)
    
    ! Initialize COO properly for SpMV tests
    CALL SCOOINIT(ROWIND, COLIND, VAL, 10, COO, INFO)
    
    ! Test SCOOMV error exits
    SRNAMT = 'SCOOMV'
    INFOT = 1
    CALL SCOOMV('X', 10, 10, 1.0_real32, COO, X, 1, 0.0_real32, Y, 1)
    CALL CHKXER('SCOOMV', INFOT, NUNIT, LERR, OK)
    
    ! Clean up
    CALL SCOOFREE(COO, INFO)
    
    ! Print summary
    IF (OK) THEN
        WRITE(NUNIT, 9999) PATH
    ELSE
        WRITE(NUNIT, 9998) PATH
    END IF
    
9999 FORMAT(1X, A3, ' routines passed the tests of the error exits')
9998 FORMAT(' *** ', A3, ' routines failed the tests of the error exits ***')
    
END SUBROUTINE SERRSP