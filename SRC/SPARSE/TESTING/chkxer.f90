!> \brief \b CHKXER tests error exits
!>
!> Tests that the error detection code works correctly

SUBROUTINE CHKXER(SRNAMT, INFOT, NOUT, LERR, OK)
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER(LEN=*) :: SRNAMT
    INTEGER :: INFOT, NOUT
    LOGICAL :: LERR, OK
    
    ! Common blocks
    INTEGER :: INFOT_COMMON, IOUNIT
    LOGICAL :: LERR_COMMON, OK_COMMON
    CHARACTER(LEN=32) :: SRNAME_COMMON
    COMMON /INFOC/ INFOT_COMMON, IOUNIT, OK_COMMON, LERR_COMMON
    COMMON /SRNAMC/ SRNAME_COMMON
    
    IF (.NOT. LERR) THEN
        WRITE(NOUT, 9999) INFOT, SRNAMT
        OK = .FALSE.
    END IF
    LERR = .FALSE.
    RETURN
    
9999 FORMAT(' *** Illegal value of parameter number ', I2, ' not detected', &
            ' by ', A6, ' ***')
    
END SUBROUTINE CHKXER

!> \brief \b ALAESM prints error messages

SUBROUTINE ALAESM(PATH, OK, NOUT)
    IMPLICIT NONE
    
    ! Arguments  
    CHARACTER(LEN=3) :: PATH
    LOGICAL :: OK
    INTEGER :: NOUT
    
    IF (OK) THEN
        WRITE(NOUT, 9999) PATH
    ELSE
        WRITE(NOUT, 9998) PATH
    END IF
    
9999 FORMAT(1X, A3, ' routines passed the tests of the error exits')
9998 FORMAT(' *** ', A3, ' routines failed the tests of the error exits', &
            ' ***')
    
END SUBROUTINE ALAESM

!> \brief \b DLARAN returns a random number

DOUBLE PRECISION FUNCTION DLARAN(ISEED)
    IMPLICIT NONE
    INTEGER :: ISEED(4)
    
    ! Simple random number generator (placeholder)
    ! In real LAPACK, this would be a proper RNG
    DOUBLE PRECISION :: R
    
    CALL RANDOM_NUMBER(R)
    DLARAN = R
    
END FUNCTION DLARAN

!> \brief \b SLARAN returns a random number (single precision)

REAL FUNCTION SLARAN(ISEED)
    IMPLICIT NONE
    INTEGER :: ISEED(4)
    
    ! Simple random number generator (placeholder)
    REAL :: R
    
    CALL RANDOM_NUMBER(R)
    SLARAN = R
    
END FUNCTION SLARAN