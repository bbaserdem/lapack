!> \brief Simple test of basic sparse matrix functionality
!>
!> This program tests basic allocation, initialization, and conversion
!> operations for the sparse matrix library.

PROGRAM test_basic
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64
    IMPLICIT NONE
    
    ! Variables
    TYPE(sparse_coo_d) :: COO
    TYPE(sparse_csr_d) :: CSR  
    INTEGER :: INFO, I
    INTEGER, PARAMETER :: N = 3, NNZ = 5
    INTEGER :: ROW_IND(NNZ) = [1, 1, 2, 3, 3]
    INTEGER :: COL_IND(NNZ) = [1, 3, 2, 1, 3] 
    REAL(real64) :: VALUES(NNZ) = [1.0_real64, 3.0_real64, 2.0_real64, 4.0_real64, 5.0_real64]
    
    WRITE(*,*) 'Testing basic sparse matrix functionality...'
    
    ! Test 1: Allocate COO matrix
    WRITE(*,*) 'Test 1: Allocating COO matrix'
    CALL DCOOALLOC(N, N, NNZ, COO, INFO)
    IF (INFO /= SPARSE_SUCCESS) THEN
        WRITE(*,*) 'ERROR: COO allocation failed with INFO =', INFO
        STOP 1
    END IF
    WRITE(*,*) 'COO allocation successful'
    
    ! Test 2: Initialize COO matrix
    WRITE(*,*) 'Test 2: Initializing COO matrix'
    CALL DCOOINIT(ROW_IND, COL_IND, VALUES, NNZ, COO, INFO)
    IF (INFO /= SPARSE_SUCCESS) THEN
        WRITE(*,*) 'ERROR: COO initialization failed with INFO =', INFO
        STOP 1
    END IF
    WRITE(*,*) 'COO initialization successful'
    WRITE(*,*) 'Matrix dimensions:', COO%nrows, 'x', COO%ncols
    WRITE(*,*) 'Number of non-zeros:', COO%nnz
    
    ! Test 3: Print matrix elements
    WRITE(*,*) 'Test 3: Matrix elements'
    DO I = 1, COO%nnz
        WRITE(*,'(A,I0,A,I0,A,F8.3)') &
            'Element (', COO%row_ind(I), ',', COO%col_ind(I), ') = ', COO%values(I)
    END DO
    
    ! Test 4: Allocate CSR matrix  
    WRITE(*,*) 'Test 4: Allocating CSR matrix'
    CALL DCSRALLOC(N, N, NNZ, CSR, INFO)
    IF (INFO /= SPARSE_SUCCESS) THEN
        WRITE(*,*) 'ERROR: CSR allocation failed with INFO =', INFO
        STOP 1
    END IF
    WRITE(*,*) 'CSR allocation successful'
    
    ! Test 5: Convert COO to CSR
    WRITE(*,*) 'Test 5: Converting COO to CSR'
    CALL DCOO2CSR(COO, CSR, INFO)
    IF (INFO /= SPARSE_SUCCESS) THEN
        WRITE(*,*) 'ERROR: COO to CSR conversion failed with INFO =', INFO
        STOP 1
    END IF
    WRITE(*,*) 'COO to CSR conversion successful'
    WRITE(*,*) 'CSR matrix dimensions:', CSR%nrows, 'x', CSR%ncols
    WRITE(*,*) 'CSR number of non-zeros:', CSR%nnz
    
    ! Test 6: Cleanup
    WRITE(*,*) 'Test 6: Freeing matrices'
    CALL DCOOFREE(COO, INFO)
    CALL DCSRFREE(CSR, INFO)
    WRITE(*,*) 'Cleanup successful'
    
    WRITE(*,*)
    WRITE(*,*) 'All basic tests passed successfully!'
    
END PROGRAM test_basic