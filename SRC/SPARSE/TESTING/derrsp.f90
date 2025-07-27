!> \brief \b DERRSP
!>
!> \par Purpose:
!> =============
!>
!> DERRSP tests the error exits for the DOUBLE PRECISION sparse
!> matrix routines.

SUBROUTINE DERRSP(PATH, NUNIT)
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER(LEN=3) :: PATH
    INTEGER :: NUNIT
    
    ! Local variables
    CHARACTER(LEN=32) :: SRNAMT
    INTEGER :: INFO, I, J
    TYPE(sparse_coo_d) :: COO
    TYPE(sparse_csr_d) :: CSR
    TYPE(sparse_csc_d) :: CSC
    
    ! Arrays for testing
    INTEGER :: ROWIND(10), COLIND(10), ROWPTR(10), COLPTR(10)
    DOUBLE PRECISION :: VAL(10), X(10), Y(10)
    
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
        VAL(I) = DBLE(I)
        X(I) = 1.0_real64
        Y(I) = 0.0_real64
    END DO
    ROWPTR(1:6) = [1, 3, 5, 7, 9, 11]
    COLPTR(1:6) = [1, 3, 5, 7, 9, 11]
    
    WRITE(NUNIT, *)
    WRITE(NUNIT, *) 'Testing error exits for sparse matrix routines:'
    WRITE(NUNIT, *)
    
    ! Test DCOOALLOC error exits
    SRNAMT = 'DCOOALLOC'
    INFOT = 1
    CALL DCOOALLOC(-1, 10, 10, COO, INFO)
    CALL CHKXER('DCOOALLOC', INFOT, NUNIT, LERR, OK)
    
    INFOT = 2
    CALL DCOOALLOC(10, -1, 10, COO, INFO)
    CALL CHKXER('DCOOALLOC', INFOT, NUNIT, LERR, OK)
    
    INFOT = 3
    CALL DCOOALLOC(10, 10, -1, COO, INFO)
    CALL CHKXER('DCOOALLOC', INFOT, NUNIT, LERR, OK)
    
    ! Allocate COO for further tests
    CALL DCOOALLOC(10, 10, 10, COO, INFO)
    
    ! Test DCOOINIT error exits
    SRNAMT = 'DCOOINIT'
    INFOT = 4
    CALL DCOOINIT(ROWIND, COLIND, VAL, -1, COO, INFO)
    CALL CHKXER('DCOOINIT', INFOT, NUNIT, LERR, OK)
    
    ! Initialize with invalid indices
    ROWIND(1) = 0  ! Invalid row index
    INFOT = 1
    CALL DCOOINIT(ROWIND, COLIND, VAL, 10, COO, INFO)
    CALL CHKXER('DCOOINIT', INFOT, NUNIT, LERR, OK)
    ROWIND(1) = 1  ! Restore
    
    COLIND(1) = 11  ! Invalid column index
    INFOT = 1
    CALL DCOOINIT(ROWIND, COLIND, VAL, 10, COO, INFO)
    CALL CHKXER('DCOOINIT', INFOT, NUNIT, LERR, OK)
    COLIND(1) = 1  ! Restore
    
    ! Initialize COO properly for SpMV tests
    CALL DCOOINIT(ROWIND, COLIND, VAL, 10, COO, INFO)
    
    ! Test DCOOMV error exits
    SRNAMT = 'DCOOMV'
    INFOT = 1
    CALL DCOOMV('X', 10, 10, 1.0_real64, COO, X, 1, 0.0_real64, Y, 1)
    CALL CHKXER('DCOOMV', INFOT, NUNIT, LERR, OK)
    
    INFOT = 2
    CALL DCOOMV('N', -1, 10, 1.0_real64, COO, X, 1, 0.0_real64, Y, 1)
    CALL CHKXER('DCOOMV', INFOT, NUNIT, LERR, OK)
    
    INFOT = 3
    CALL DCOOMV('N', 10, -1, 1.0_real64, COO, X, 1, 0.0_real64, Y, 1)
    CALL CHKXER('DCOOMV', INFOT, NUNIT, LERR, OK)
    
    INFOT = 7
    CALL DCOOMV('N', 10, 10, 1.0_real64, COO, X, 0, 0.0_real64, Y, 1)
    CALL CHKXER('DCOOMV', INFOT, NUNIT, LERR, OK)
    
    INFOT = 10
    CALL DCOOMV('N', 10, 10, 1.0_real64, COO, X, 1, 0.0_real64, Y, 0)
    CALL CHKXER('DCOOMV', INFOT, NUNIT, LERR, OK)
    
    ! Test DCSRINIT error exits
    SRNAMT = 'DCSRINIT'
    INFOT = 4
    CALL DCSRINIT(ROWPTR, COLIND, VAL, -1, 10, CSR, INFO)
    CALL CHKXER('DCSRINIT', INFOT, NUNIT, LERR, OK)
    
    INFOT = 5
    CALL DCSRINIT(ROWPTR, COLIND, VAL, 5, -1, CSR, INFO)
    CALL CHKXER('DCSRINIT', INFOT, NUNIT, LERR, OK)
    
    ! Test with invalid row pointers
    ROWPTR(1) = 0  ! Should be 1
    INFOT = 1
    CALL DCSRINIT(ROWPTR, COLIND, VAL, 5, 10, CSR, INFO)
    CALL CHKXER('DCSRINIT', INFOT, NUNIT, LERR, OK)
    ROWPTR(1) = 1  ! Restore
    
    ! Initialize CSR properly
    CALL DCSRINIT(ROWPTR, COLIND, VAL, 5, 10, CSR, INFO)
    
    ! Test DCSRMV error exits
    SRNAMT = 'DCSRMV'
    INFOT = 1
    CALL DCSRMV('X', 5, 5, 1.0_real64, CSR, X, 1, 0.0_real64, Y, 1)
    CALL CHKXER('DCSRMV', INFOT, NUNIT, LERR, OK)
    
    INFOT = 2
    CALL DCSRMV('N', -1, 5, 1.0_real64, CSR, X, 1, 0.0_real64, Y, 1)
    CALL CHKXER('DCSRMV', INFOT, NUNIT, LERR, OK)
    
    ! Test DCSCINIT error exits
    SRNAMT = 'DCSCINIT'
    INFOT = 4
    CALL DCSCINIT(COLPTR, ROWIND, VAL, -1, 10, CSC, INFO)
    CALL CHKXER('DCSCINIT', INFOT, NUNIT, LERR, OK)
    
    INFOT = 5
    CALL DCSCINIT(COLPTR, ROWIND, VAL, 5, -1, CSC, INFO)
    CALL CHKXER('DCSCINIT', INFOT, NUNIT, LERR, OK)
    
    ! Initialize CSC properly
    CALL DCSCINIT(COLPTR, ROWIND, VAL, 5, 10, CSC, INFO)
    
    ! Test DCSCMV error exits
    SRNAMT = 'DCSCMV'
    INFOT = 1
    CALL DCSCMV('X', 5, 5, 1.0_real64, CSC, X, 1, 0.0_real64, Y, 1)
    CALL CHKXER('DCSCMV', INFOT, NUNIT, LERR, OK)
    
    ! Test DCOOCONV error exits
    SRNAMT = 'DCOOCONV'
    INFOT = 1
    CALL DCOOCONV('XSR', COO, CSR, INFO)  ! Invalid format
    CALL CHKXER('DCOOCONV', INFOT, NUNIT, LERR, OK)
    
    ! Clean up
    CALL DCOOFREE(COO, INFO)
    CALL DCSRFREE(CSR, INFO)
    CALL DCSCFREE(CSC, INFO)
    
    ! Print summary
    IF (OK) THEN
        WRITE(NUNIT, 9999) PATH
    ELSE
        WRITE(NUNIT, 9998) PATH
    END IF
    
9999 FORMAT(1X, A3, ' routines passed the tests of the error exits')
9998 FORMAT(' *** ', A3, ' routines failed the tests of the error exits ***')
    
END SUBROUTINE DERRSP