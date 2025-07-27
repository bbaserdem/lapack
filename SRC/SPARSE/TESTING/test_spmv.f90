!> \brief Test suite for sparse matrix-vector multiplication routines
!>
!> This program tests the SpMV routines for COO, CSR, and CSC formats:
!> - DCOOMV: COO matrix-vector multiplication
!> - DCSRMV: CSR matrix-vector multiplication  
!> - DCSCMV: CSC matrix-vector multiplication
!>
!> It also tests element access operations:
!> - DSPGET: Element retrieval
!> - DSPSET: Element setting
!> - DCOOTRANS: In-place transpose

PROGRAM test_spmv
    USE sparse_types
    USE sparse_constants
    USE DSPGET_MODULE
    USE ISO_FORTRAN_ENV, ONLY: output_unit, error_unit
    IMPLICIT NONE
    
    ! Test matrix size
    INTEGER, PARAMETER :: M = 5, N = 4
    INTEGER, PARAMETER :: MAX_NNZ = 20
    
    ! Sparse matrices
    TYPE(sparse_coo_d) :: coo
    TYPE(sparse_csr_d) :: csr
    TYPE(sparse_csc_d) :: csc
    
    ! Test vectors
    REAL(real64), ALLOCATABLE :: x(:), y(:), y_ref(:)
    
    ! Test parameters
    REAL(real64) :: alpha, beta
    CHARACTER :: trans
    
    ! Local variables
    INTEGER :: info, i, j, k, test_count, pass_count
    REAL(real64) :: error, tol, test_val
    LOGICAL :: test_passed
    
    ! External subroutines
    EXTERNAL :: DCOOMV, DCSRMV, DCSCMV, DCOOTRANS
    EXTERNAL :: DCOOALLOC, DCOOINIT, DCSRALLOC, DCSRINIT, DCSCALLOC, DCSCINIT
    EXTERNAL :: DCOO2CSR, DCOO2CSC
    EXTERNAL :: DSPSET_COO
    
    ! Initialize test counters
    test_count = 0
    pass_count = 0
    tol = 1.0E-14_real64
    
    WRITE(output_unit, '(A)') '========================================='
    WRITE(output_unit, '(A)') 'Testing Sparse Matrix-Vector Operations'
    WRITE(output_unit, '(A)') '========================================='
    
    ! Allocate vectors
    ALLOCATE(x(MAX(M,N)), y(MAX(M,N)), y_ref(MAX(M,N)))
    
    ! Initialize test matrix in COO format
    ! Matrix:
    ! [ 1.0  0.0  2.0  0.0 ]
    ! [ 0.0  3.0  0.0  0.0 ]
    ! [ 4.0  0.0  5.0  6.0 ]
    ! [ 0.0  7.0  0.0  0.0 ]
    ! [ 8.0  0.0  0.0  9.0 ]
    
    CALL DCOOINIT_TEST(coo, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to initialize COO matrix, INFO = ', info
        STOP 1
    END IF
    
    ! Test 1: COO matrix-vector multiplication (no transpose)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 1: DCOOMV - y = alpha*A*x + beta*y'
    
    trans = 'N'
    alpha = 2.0_real64
    beta = 1.0_real64
    
    ! Initialize x and y
    x(1:N) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
    y(1:M) = [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]
    
    ! Expected result: y = 2*A*x + y
    ! A*x = [7, 6, 43, 14, 44]
    ! y = 2*[7, 6, 43, 14, 44] + [1, 1, 1, 1, 1] = [15, 13, 87, 29, 89]
    y_ref(1:M) = [15.0_real64, 13.0_real64, 87.0_real64, 29.0_real64, 89.0_real64]
    
    CALL DCOOMV(trans, M, N, alpha, coo, x, 1, beta, y, 1)
    
    error = MAXVAL(ABS(y(1:M) - y_ref(1:M)))
    test_passed = (error < tol)
    IF (test_passed) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,E12.5)') '  PASSED - Max error: ', error
    ELSE
        WRITE(output_unit, '(A,E12.5)') '  FAILED - Max error: ', error
    END IF
    
    ! Test 2: COO matrix-vector multiplication (transpose)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 2: DCOOMV - y = alpha*A^T*x + beta*y'
    
    trans = 'T'
    alpha = 1.0_real64
    beta = 0.0_real64
    
    ! Initialize x and y
    x(1:M) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
    y(1:N) = 0.0_real64
    
    ! Expected result: y = A^T*x
    ! A^T*x = [1*1+4*3+8*5, 3*2+7*4, 2*1+5*3, 6*3+9*5] = [53, 34, 17, 63]
    y_ref(1:N) = [53.0_real64, 34.0_real64, 17.0_real64, 63.0_real64]
    
    CALL DCOOMV(trans, M, N, alpha, coo, x, 1, beta, y, 1)
    
    error = MAXVAL(ABS(y(1:N) - y_ref(1:N)))
    test_passed = (error < tol)
    IF (test_passed) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,E12.5)') '  PASSED - Max error: ', error
    ELSE
        WRITE(output_unit, '(A,E12.5)') '  FAILED - Max error: ', error
    END IF
    
    ! Allocate CSR matrix
    CALL DCSRALLOC(M, N, coo%nnz, csr, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to allocate CSR matrix, INFO = ', info
        STOP 1
    END IF
    
    ! Convert COO to CSR
    CALL DCOO2CSR(coo, csr, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to convert COO to CSR, INFO = ', info
        STOP 1
    END IF
    
    ! Test 3: CSR matrix-vector multiplication (no transpose)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 3: DCSRMV - y = alpha*A*x + beta*y'
    
    trans = 'N'
    alpha = 2.0_real64
    beta = 1.0_real64
    
    ! Use same input as Test 1
    x(1:N) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
    y(1:M) = [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]
    
    ! Reset expected values (same as Test 1)
    y_ref(1:M) = [15.0_real64, 13.0_real64, 87.0_real64, 29.0_real64, 89.0_real64]
    
    CALL DCSRMV(trans, M, N, alpha, csr, x, 1, beta, y, 1)
    
    error = MAXVAL(ABS(y(1:M) - y_ref(1:M)))
    test_passed = (error < tol)
    IF (test_passed) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,E12.5)') '  PASSED - Max error: ', error
    ELSE
        WRITE(output_unit, '(A,E12.5)') '  FAILED - Max error: ', error
    END IF
    
    ! Allocate CSC matrix
    CALL DCSCALLOC(M, N, coo%nnz, csc, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to allocate CSC matrix, INFO = ', info
        STOP 1
    END IF
    
    ! Convert COO to CSC
    CALL DCOO2CSC(coo, csc, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to convert COO to CSC, INFO = ', info
        STOP 1
    END IF
    
    ! Test 4: CSC matrix-vector multiplication (no transpose)
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 4: DCSCMV - y = alpha*A*x + beta*y'
    
    trans = 'N'
    alpha = 2.0_real64
    beta = 1.0_real64
    
    ! Use same input as Test 1
    x(1:N) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
    y(1:M) = [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]
    
    ! Reset expected values (same as Test 1)
    y_ref(1:M) = [15.0_real64, 13.0_real64, 87.0_real64, 29.0_real64, 89.0_real64]
    
    CALL DCSCMV(trans, M, N, alpha, csc, x, 1, beta, y, 1)
    
    error = MAXVAL(ABS(y(1:M) - y_ref(1:M)))
    test_passed = (error < tol)
    IF (test_passed) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,E12.5)') '  PASSED - Max error: ', error
    ELSE
        WRITE(output_unit, '(A,E12.5)') '  FAILED - Max error: ', error
    END IF
    
    ! Test 5: DSPGET - Element retrieval
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 5: DSPGET - Element retrieval'
    
    test_passed = .TRUE.
    
    ! Test known elements
    test_val = DSPGET_COO(coo, 1, 1)
    IF (ABS(test_val - 1.0_real64) > tol) test_passed = .FALSE.
    
    test_val = DSPGET_COO(coo, 3, 3)
    IF (ABS(test_val - 5.0_real64) > tol) test_passed = .FALSE.
    
    test_val = DSPGET_COO(coo, 5, 4)
    IF (ABS(test_val - 9.0_real64) > tol) test_passed = .FALSE.
    
    ! Test zero element
    test_val = DSPGET_COO(coo, 1, 2)
    IF (ABS(test_val) > tol) test_passed = .FALSE.
    
    IF (test_passed) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A)') '  PASSED - All element retrievals correct'
    ELSE
        WRITE(output_unit, '(A)') '  FAILED - Element retrieval errors'
    END IF
    
    ! Test 6: DSPSET - Element setting
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 6: DSPSET - Element setting'
    
    ! Set a new element
    CALL DSPSET_COO(coo, 1, 2, 10.0_real64, info)
    test_val = DSPGET_COO(coo, 1, 2)
    test_passed = (ABS(test_val - 10.0_real64) < tol) .AND. (info == 0)
    
    IF (test_passed) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A)') '  PASSED - Element setting successful'
    ELSE
        WRITE(output_unit, '(A)') '  FAILED - Element setting failed'
    END IF
    
    ! Test 7: DCOOTRANS - In-place transpose
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 7: DCOOTRANS - In-place transpose'
    
    ! Store original dimensions
    i = coo%nrows
    j = coo%ncols
    
    ! Transpose the matrix
    CALL DCOOTRANS(coo)
    
    ! Check dimensions are swapped
    test_passed = (coo%nrows == j) .AND. (coo%ncols == i)
    
    ! Transpose back
    CALL DCOOTRANS(coo)
    
    ! Check dimensions are restored
    test_passed = test_passed .AND. (coo%nrows == i) .AND. (coo%ncols == j)
    
    IF (test_passed) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A)') '  PASSED - Transpose operations successful'
    ELSE
        WRITE(output_unit, '(A)') '  FAILED - Transpose failed'
    END IF
    
    ! Summary
    WRITE(output_unit, '(/A)') '========================================='
    WRITE(output_unit, '(A,I0,A,I0)') 'Tests passed: ', pass_count, ' out of ', test_count
    WRITE(output_unit, '(A)') '========================================='
    
    ! Cleanup
    DEALLOCATE(x, y, y_ref)
    
    IF (pass_count == test_count) THEN
        STOP 0
    ELSE
        STOP 1
    END IF
    
CONTAINS

    SUBROUTINE DCOOINIT_TEST(COO, INFO)
        TYPE(sparse_coo_d), INTENT(OUT) :: COO
        INTEGER, INTENT(OUT) :: INFO
        
        INTEGER :: nnz
        INTEGER, ALLOCATABLE :: row_ind(:), col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        
        EXTERNAL :: DCOOALLOC
        
        nnz = 9
        ALLOCATE(row_ind(nnz), col_ind(nnz), values(nnz))
        
        ! Matrix elements in COO format
        row_ind = [1, 1, 2, 3, 3, 3, 4, 5, 5]
        col_ind = [1, 3, 2, 1, 3, 4, 2, 1, 4]
        values = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, &
                  5.0_real64, 6.0_real64, 7.0_real64, 8.0_real64, 9.0_real64]
        
        ! First allocate the COO matrix
        CALL DCOOALLOC(M, N, nnz, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Then initialize with data
        CALL DCOOINIT(row_ind, col_ind, values, nnz, COO, INFO)
        
        DEALLOCATE(row_ind, col_ind, values)
    END SUBROUTINE DCOOINIT_TEST

END PROGRAM test_spmv