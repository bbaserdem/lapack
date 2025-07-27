!> \brief Test sparse matrix-vector multiplication
!>
!> This program tests SpMV operations in all three formats

PROGRAM test_spmv
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64
    IMPLICIT NONE
    
    ! Variables
    TYPE(sparse_coo_d) :: COO
    TYPE(sparse_csr_d) :: CSR
    TYPE(sparse_csc_d) :: CSC
    INTEGER :: INFO, I
    INTEGER, PARAMETER :: N = 3, NNZ = 5
    INTEGER :: ROW_IND(NNZ) = [1, 1, 2, 3, 3]
    INTEGER :: COL_IND(NNZ) = [1, 3, 2, 1, 3] 
    REAL(real64) :: VALUES(NNZ) = [1.0_real64, 3.0_real64, 2.0_real64, 4.0_real64, 5.0_real64]
    REAL(real64) :: X(N) = [1.0_real64, 2.0_real64, 3.0_real64]
    REAL(real64) :: Y_COO(N), Y_CSR(N), Y_CSC(N)
    REAL(real64), PARAMETER :: ALPHA = 1.0_real64, BETA = 0.0_real64
    
    WRITE(*,*) 'Testing sparse matrix-vector multiplication...'
    
    ! Set up matrices
    CALL DCOOALLOC(N, N, NNZ, COO, INFO)
    CALL DCOOINIT(ROW_IND, COL_IND, VALUES, NNZ, COO, INFO)
    
    CALL DCSRALLOC(N, N, NNZ, CSR, INFO)
    CALL DCOO2CSR(COO, CSR, INFO)
    
    CALL DCSCALLOC(N, N, NNZ, CSC, INFO)
    CALL DCOO2CSC(COO, CSC, INFO)
    
    ! Initialize result vectors
    Y_COO = 0.0_real64
    Y_CSR = 0.0_real64  
    Y_CSC = 0.0_real64
    
    WRITE(*,*) 'Input vector X:', (X(I), I=1,N)
    
    ! Test COO SpMV: y = A*x
    WRITE(*,*) 'Test 1: COO SpMV'
    CALL DCOOMV('N', N, N, ALPHA, COO, X, 1, BETA, Y_COO, 1)
    WRITE(*,*) 'COO result Y:', (Y_COO(I), I=1,N)
    
    ! Test CSR SpMV: y = A*x  
    WRITE(*,*) 'Test 2: CSR SpMV'
    CALL DCSRMV('N', N, N, ALPHA, CSR, X, 1, BETA, Y_CSR, 1)
    WRITE(*,*) 'CSR result Y:', (Y_CSR(I), I=1,N)
    
    ! Test CSC SpMV: y = A*x
    WRITE(*,*) 'Test 3: CSC SpMV'
    CALL DCSCMV('N', N, N, ALPHA, CSC, X, 1, BETA, Y_CSC, 1)
    WRITE(*,*) 'CSC result Y:', (Y_CSC(I), I=1,N)
    
    ! Verify results match
    WRITE(*,*) 'Test 4: Verifying results match'
    DO I = 1, N
        IF (ABS(Y_COO(I) - Y_CSR(I)) > 1.0E-12_real64 .OR. &
            ABS(Y_COO(I) - Y_CSC(I)) > 1.0E-12_real64) THEN
            WRITE(*,*) 'ERROR: Results do not match at position', I
            WRITE(*,*) 'COO:', Y_COO(I), 'CSR:', Y_CSR(I), 'CSC:', Y_CSC(I)
            STOP 1
        END IF
    END DO
    WRITE(*,*) 'All SpMV results match!'
    
    ! Expected result: A*x where A = [1 0 3; 0 2 0; 4 0 5], x = [1; 2; 3]
    ! Should give: [1*1+0*2+3*3, 0*1+2*2+0*3, 4*1+0*2+5*3] = [10, 4, 19]
    WRITE(*,*) 'Expected result: [10, 4, 19]'
    
    ! Cleanup
    CALL DCOOFREE(COO, INFO)
    CALL DCSRFREE(CSR, INFO)
    CALL DCSCFREE(CSC, INFO)
    
    WRITE(*,*)
    WRITE(*,*) 'All SpMV tests passed successfully!'
    
END PROGRAM test_spmv