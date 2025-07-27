!> \brief Test program for sparse matrix conversion routines
!>
!> This program tests all conversion routines between different sparse formats
!> (COO, CSR, CSC) and between sparse and dense formats.

PROGRAM test_conversions
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64, output_unit, error_unit
    IMPLICIT NONE
    
    ! Test matrices
    TYPE(sparse_coo_d) :: coo, coo_result
    TYPE(sparse_csr_d) :: csr
    TYPE(sparse_csc_d) :: csc
    REAL(real64), ALLOCATABLE :: dense(:,:), dense_result(:,:)
    
    ! Test parameters
    INTEGER, PARAMETER :: M = 5  ! rows
    INTEGER, PARAMETER :: N = 6  ! columns
    INTEGER, PARAMETER :: LDA = M
    INTEGER, PARAMETER :: MAX_NNZ = 30
    
    ! Local variables
    INTEGER :: info, i, j, k
    INTEGER :: test_count, test_passed
    LOGICAL :: test_ok
    REAL(real64) :: error_tol = 1.0e-14_real64
    
    ! External routines
    EXTERNAL :: DCOOALLOC, DCSRALLOC, DCSCALLOC
    EXTERNAL :: DCOO2CSR, DCOO2CSC, DCSR2COO, DCSC2COO
    EXTERNAL :: DDEN2COO, DCOO2DEN
    EXTERNAL :: DCOOFREE, DCSRFREE, DCSCFREE
    
    WRITE(output_unit, '(A)') 'Testing Sparse Matrix Conversion Routines'
    WRITE(output_unit, '(A)') '========================================='
    
    test_count = 0
    test_passed = 0
    
    ! Allocate test matrices
    ALLOCATE(dense(M,N), dense_result(M,N))
    
    ! Initialize dense test matrix with specific pattern
    dense = 0.0_real64
    dense(1,1) = 1.0_real64
    dense(1,3) = 2.0_real64
    dense(2,2) = 3.0_real64
    dense(3,1) = 4.0_real64
    dense(3,4) = 5.0_real64
    dense(4,5) = 6.0_real64
    dense(5,3) = 7.0_real64
    dense(5,6) = 8.0_real64
    
    WRITE(output_unit, '(A)') ''
    WRITE(output_unit, '(A)') 'Test dense matrix:'
    DO i = 1, M
        WRITE(output_unit, '(6F8.2)') (dense(i,j), j=1,N)
    END DO
    
    ! Test 1: Dense to COO conversion
    test_count = test_count + 1
    WRITE(output_unit, '(A)') ''
    WRITE(output_unit, '(A)') 'Test 1: DDEN2COO (Dense to COO)'
    
    CALL DCOOALLOC(M, N, MAX_NNZ, coo, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'Error in DCOOALLOC: ', info
        STOP 1
    END IF
    
    CALL DDEN2COO(dense, LDA, M, N, coo, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'Error in DDEN2COO: ', info
    ELSE
        WRITE(output_unit, '(A,I0)') 'Number of non-zeros found: ', coo%nnz
        test_passed = test_passed + 1
        
        ! Print COO entries
        WRITE(output_unit, '(A)') 'COO entries (row, col, value):'
        DO k = 1, coo%nnz
            WRITE(output_unit, '(2I4,F10.4)') coo%row_ind(k), coo%col_ind(k), coo%values(k)
        END DO
    END IF
    
    ! Test 2: COO to Dense conversion
    test_count = test_count + 1
    WRITE(output_unit, '(A)') ''
    WRITE(output_unit, '(A)') 'Test 2: DCOO2DEN (COO to Dense)'
    
    dense_result = 0.0_real64
    CALL DCOO2DEN(coo, dense_result, LDA, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'Error in DCOO2DEN: ', info
    ELSE
        ! Check if conversion is correct
        test_ok = .TRUE.
        DO j = 1, N
            DO i = 1, M
                IF (ABS(dense(i,j) - dense_result(i,j)) > error_tol) THEN
                    test_ok = .FALSE.
                    WRITE(error_unit, '(A,2I4,2F12.6)') 'Mismatch at: ', i, j, &
                          dense(i,j), dense_result(i,j)
                END IF
            END DO
        END DO
        
        IF (test_ok) THEN
            WRITE(output_unit, '(A)') 'Dense matrix reconstructed correctly!'
            test_passed = test_passed + 1
        ELSE
            WRITE(error_unit, '(A)') 'Dense matrix reconstruction FAILED!'
        END IF
    END IF
    
    ! Test 3: COO to CSR conversion
    test_count = test_count + 1
    WRITE(output_unit, '(A)') ''
    WRITE(output_unit, '(A)') 'Test 3: DCOO2CSR (COO to CSR)'
    
    CALL DCSRALLOC(M, N, MAX_NNZ, csr, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'Error in DCSRALLOC: ', info
        STOP 1
    END IF
    
    CALL DCOO2CSR(coo, csr, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'Error in DCOO2CSR: ', info
    ELSE
        WRITE(output_unit, '(A)') 'CSR format created:'
        WRITE(output_unit, '(A,10I4)') 'Row pointers: ', (csr%row_ptr(i), i=1,M+1)
        WRITE(output_unit, '(A,20I4)') 'Column indices: ', (csr%col_ind(i), i=1,csr%nnz)
        WRITE(output_unit, '(A)') 'Values:'
        WRITE(output_unit, '(8F10.4)') (csr%values(i), i=1,csr%nnz)
        test_passed = test_passed + 1
    END IF
    
    ! Test 4: CSR to COO conversion
    test_count = test_count + 1
    WRITE(output_unit, '(A)') ''
    WRITE(output_unit, '(A)') 'Test 4: DCSR2COO (CSR to COO)'
    
    CALL DCOOALLOC(M, N, MAX_NNZ, coo_result, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'Error in DCOOALLOC for result: ', info
        STOP 1
    END IF
    
    CALL DCSR2COO(csr, coo_result, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'Error in DCSR2COO: ', info
    ELSE
        ! Verify by converting back to dense
        dense_result = 0.0_real64
        CALL DCOO2DEN(coo_result, dense_result, LDA, info)
        
        test_ok = .TRUE.
        DO j = 1, N
            DO i = 1, M
                IF (ABS(dense(i,j) - dense_result(i,j)) > error_tol) THEN
                    test_ok = .FALSE.
                END IF
            END DO
        END DO
        
        IF (test_ok) THEN
            WRITE(output_unit, '(A)') 'CSR->COO->Dense conversion correct!'
            test_passed = test_passed + 1
        ELSE
            WRITE(error_unit, '(A)') 'CSR->COO->Dense conversion FAILED!'
        END IF
    END IF
    
    ! Test 5: COO to CSC conversion
    test_count = test_count + 1
    WRITE(output_unit, '(A)') ''
    WRITE(output_unit, '(A)') 'Test 5: DCOO2CSC (COO to CSC)'
    
    CALL DCSCALLOC(M, N, MAX_NNZ, csc, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'Error in DCSCALLOC: ', info
        STOP 1
    END IF
    
    CALL DCOO2CSC(coo, csc, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'Error in DCOO2CSC: ', info
    ELSE
        WRITE(output_unit, '(A)') 'CSC format created:'
        WRITE(output_unit, '(A,10I4)') 'Column pointers: ', (csc%col_ptr(i), i=1,N+1)
        WRITE(output_unit, '(A,20I4)') 'Row indices: ', (csc%row_ind(i), i=1,csc%nnz)
        WRITE(output_unit, '(A)') 'Values:'
        WRITE(output_unit, '(8F10.4)') (csc%values(i), i=1,csc%nnz)
        test_passed = test_passed + 1
    END IF
    
    ! Test 6: CSC to COO conversion
    test_count = test_count + 1
    WRITE(output_unit, '(A)') ''
    WRITE(output_unit, '(A)') 'Test 6: DCSC2COO (CSC to COO)'
    
    CALL DCOOFREE(coo_result, info)  ! Free previous result
    CALL DCOOALLOC(M, N, MAX_NNZ, coo_result, info)
    
    CALL DCSC2COO(csc, coo_result, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'Error in DCSC2COO: ', info
    ELSE
        ! Verify by converting back to dense
        dense_result = 0.0_real64
        CALL DCOO2DEN(coo_result, dense_result, LDA, info)
        
        test_ok = .TRUE.
        DO j = 1, N
            DO i = 1, M
                IF (ABS(dense(i,j) - dense_result(i,j)) > error_tol) THEN
                    test_ok = .FALSE.
                END IF
            END DO
        END DO
        
        IF (test_ok) THEN
            WRITE(output_unit, '(A)') 'CSC->COO->Dense conversion correct!'
            test_passed = test_passed + 1
        ELSE
            WRITE(error_unit, '(A)') 'CSC->COO->Dense conversion FAILED!'
        END IF
    END IF
    
    ! Summary
    WRITE(output_unit, '(A)') ''
    WRITE(output_unit, '(A)') '========================================='
    WRITE(output_unit, '(A,I0,A,I0)') 'Tests passed: ', test_passed, ' out of ', test_count
    
    IF (test_passed == test_count) THEN
        WRITE(output_unit, '(A)') 'ALL TESTS PASSED!'
    ELSE
        WRITE(error_unit, '(A)') 'SOME TESTS FAILED!'
    END IF
    
    ! Clean up
    CALL DCOOFREE(coo, info)
    CALL DCOOFREE(coo_result, info)
    CALL DCSRFREE(csr, info)
    CALL DCSCFREE(csc, info)
    DEALLOCATE(dense, dense_result)
    
END PROGRAM test_conversions