!> \brief Test sparse matrix utility functions
!>
!> This program tests utility functions like get/set, sorting, etc.

PROGRAM test_utils
    USE sparse_types
    USE sparse_constants
    USE DSPGET_MODULE
    USE ISO_FORTRAN_ENV, ONLY: real64
    IMPLICIT NONE
    
    ! Variables
    TYPE(sparse_coo_d) :: COO
    TYPE(sparse_csr_d) :: CSR
    TYPE(sparse_csc_d) :: CSC
    INTEGER :: INFO, I, J
    INTEGER, PARAMETER :: N = 3, NNZ = 5
    INTEGER :: ROW_IND(NNZ) = [1, 1, 2, 3, 3]
    INTEGER :: COL_IND(NNZ) = [1, 3, 2, 1, 3] 
    REAL(real64) :: VALUES(NNZ) = [1.0_real64, 3.0_real64, 2.0_real64, 4.0_real64, 5.0_real64]
    REAL(real64) :: VAL
    
    WRITE(*,*) 'Testing sparse matrix utility functions...'
    
    ! Set up matrices
    CALL DCOOALLOC(N, N, NNZ, COO, INFO)
    CALL DCOOINIT(ROW_IND, COL_IND, VALUES, NNZ, COO, INFO)
    
    CALL DCSRALLOC(N, N, NNZ, CSR, INFO)
    CALL DCOO2CSR(COO, CSR, INFO)
    
    CALL DCSCALLOC(N, N, NNZ, CSC, INFO)
    CALL DCOO2CSC(COO, CSC, INFO)
    
    ! Test getting elements
    WRITE(*,*) 'Test 1: Getting matrix elements'
    WRITE(*,*) 'Matrix representation:'
    DO I = 1, N
        WRITE(*,'(A,I0,A)', ADVANCE='NO') 'Row ', I, ': '
        DO J = 1, N
            VAL = DSPGET_COO(COO, I, J)
            WRITE(*,'(F5.1,X)', ADVANCE='NO') VAL
        END DO
        WRITE(*,*)
    END DO
    
    ! Test specific element lookups
    WRITE(*,*) 'Test 2: Specific element lookups'
    WRITE(*,'(A,F5.1)') 'COO(1,1) = ', DSPGET_COO(COO, 1, 1)
    WRITE(*,'(A,F5.1)') 'CSR(1,1) = ', DSPGET_CSR(CSR, 1, 1) 
    WRITE(*,'(A,F5.1)') 'CSC(1,1) = ', DSPGET_CSC(CSC, 1, 1)
    
    WRITE(*,'(A,F5.1)') 'COO(2,3) = ', DSPGET_COO(COO, 2, 3)  ! Should be 0
    WRITE(*,'(A,F5.1)') 'CSR(2,3) = ', DSPGET_CSR(CSR, 2, 3)  ! Should be 0
    WRITE(*,'(A,F5.1)') 'CSC(2,3) = ', DSPGET_CSC(CSC, 2, 3)  ! Should be 0
    
    WRITE(*,'(A,F5.1)') 'COO(3,3) = ', DSPGET_COO(COO, 3, 3)
    WRITE(*,'(A,F5.1)') 'CSR(3,3) = ', DSPGET_CSR(CSR, 3, 3)
    WRITE(*,'(A,F5.1)') 'CSC(3,3) = ', DSPGET_CSC(CSC, 3, 3)
    
    ! Test 3: Verify matrix consistency across formats
    WRITE(*,*) 'Test 3: Verifying consistency across formats'
    DO I = 1, N
        DO J = 1, N
            IF (ABS(DSPGET_COO(COO, I, J) - DSPGET_CSR(CSR, I, J)) > 1.0E-12_real64 .OR. &
                ABS(DSPGET_COO(COO, I, J) - DSPGET_CSC(CSC, I, J)) > 1.0E-12_real64) THEN
                WRITE(*,*) 'ERROR: Inconsistent values at (', I, ',', J, ')'
                STOP 1
            END IF
        END DO
    END DO
    WRITE(*,*) 'All formats are consistent!'
    
    ! Cleanup
    CALL DCOOFREE(COO, INFO)
    CALL DCSRFREE(CSR, INFO)
    CALL DCSCFREE(CSC, INFO)
    
    WRITE(*,*)
    WRITE(*,*) 'All utility tests completed!'
    
END PROGRAM test_utils