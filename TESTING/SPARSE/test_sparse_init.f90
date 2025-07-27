!> \brief Test program for sparse matrix initialization routines
!>
!> This program tests the basic functionality of COO, CSR, and CSC
!> sparse matrix types and their initialization routines.

PROGRAM test_sparse_init
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64
    IMPLICIT NONE
    
    ! Test result tracking
    INTEGER :: total_tests = 0
    INTEGER :: passed_tests = 0
    INTEGER :: failed_tests = 0
    
    ! Test sparse matrices
    TYPE(sparse_coo_d) :: coo_mat
    TYPE(sparse_csr_d) :: csr_mat, csr_conv
    TYPE(sparse_csc_d) :: csc_mat
    
    ! Test data - small 4x5 matrix with 6 non-zeros:
    ! [1.0  0   2.0  0   0  ]
    ! [0    3.0 0    0   0  ]
    ! [0    0   0    4.0 0  ]
    ! [5.0  0   0    0   6.0]
    INTEGER, PARAMETER :: nrows = 4, ncols = 5, nnz = 6
    INTEGER :: row_ind(6) = [1, 1, 2, 3, 4, 4]
    INTEGER :: col_ind(6) = [1, 3, 2, 4, 1, 5]
    REAL(real64) :: values(6) = [1.0_real64, 2.0_real64, 3.0_real64, &
                                  4.0_real64, 5.0_real64, 6.0_real64]
    
    ! CSR format data
    INTEGER :: row_ptr(5) = [1, 3, 4, 5, 7]  ! nrows+1
    INTEGER :: csr_col_ind(6) = [1, 3, 2, 4, 1, 5]
    
    ! CSC format data  
    INTEGER :: col_ptr(6) = [1, 3, 4, 5, 6, 7]  ! ncols+1
    INTEGER :: csc_row_ind(6) = [1, 4, 2, 1, 3, 4]
    REAL(real64) :: csc_values(6) = [1.0_real64, 5.0_real64, 3.0_real64, &
                                      2.0_real64, 4.0_real64, 6.0_real64]
    
    ! Local variables
    INTEGER :: info, i
    
    ! External routines
    EXTERNAL :: DCOOALLOC, DCOOINIT
    EXTERNAL :: DCSRALLOC, DCSRINIT
    EXTERNAL :: DCSCALLOC, DCSCINIT
    EXTERNAL :: DCOO2CSR
    
    PRINT *, "======================================"
    PRINT *, "Sparse Matrix Initialization Tests"
    PRINT *, "======================================"
    PRINT *, ""
    
    ! Test 1: COO allocation and initialization
    PRINT *, "Test 1: COO allocation and initialization"
    total_tests = total_tests + 1
    
    CALL DCOOALLOC(nrows, ncols, nnz, coo_mat, info)
    IF (info /= SPARSE_SUCCESS) THEN
        PRINT *, "  FAILED: DCOOALLOC returned info =", info
        failed_tests = failed_tests + 1
    ELSE
        CALL DCOOINIT(row_ind, col_ind, values, nnz, coo_mat, info)
        IF (info /= SPARSE_SUCCESS) THEN
            PRINT *, "  FAILED: DCOOINIT returned info =", info
            failed_tests = failed_tests + 1
        ELSE
            IF (coo_mat%nrows == nrows .AND. coo_mat%ncols == ncols .AND. &
                coo_mat%nnz == nnz) THEN
                PRINT *, "  PASSED"
                passed_tests = passed_tests + 1
            ELSE
                PRINT *, "  FAILED: Incorrect dimensions or nnz"
                failed_tests = failed_tests + 1
            END IF
        END IF
    END IF
    
    ! Test 2: CSR allocation and initialization
    PRINT *, "Test 2: CSR allocation and initialization"
    total_tests = total_tests + 1
    
    CALL DCSRALLOC(nrows, ncols, nnz, csr_mat, info)
    IF (info /= SPARSE_SUCCESS) THEN
        PRINT *, "  FAILED: DCSRALLOC returned info =", info
        failed_tests = failed_tests + 1
    ELSE
        CALL DCSRINIT(row_ptr, csr_col_ind, values, nrows, nnz, csr_mat, info)
        IF (info /= SPARSE_SUCCESS) THEN
            PRINT *, "  FAILED: DCSRINIT returned info =", info
            failed_tests = failed_tests + 1
        ELSE
            IF (csr_mat%nrows == nrows .AND. csr_mat%ncols == ncols .AND. &
                csr_mat%nnz == nnz) THEN
                PRINT *, "  PASSED"
                passed_tests = passed_tests + 1
            ELSE
                PRINT *, "  FAILED: Incorrect dimensions or nnz"
                failed_tests = failed_tests + 1
            END IF
        END IF
    END IF
    
    ! Test 3: CSC allocation and initialization
    PRINT *, "Test 3: CSC allocation and initialization"
    total_tests = total_tests + 1
    
    CALL DCSCALLOC(nrows, ncols, nnz, csc_mat, info)
    IF (info /= SPARSE_SUCCESS) THEN
        PRINT *, "  FAILED: DCSCALLOC returned info =", info
        failed_tests = failed_tests + 1
    ELSE
        CALL DCSCINIT(col_ptr, csc_row_ind, csc_values, ncols, nnz, csc_mat, info)
        IF (info /= SPARSE_SUCCESS) THEN
            PRINT *, "  FAILED: DCSCINIT returned info =", info
            failed_tests = failed_tests + 1
        ELSE
            IF (csc_mat%nrows == nrows .AND. csc_mat%ncols == ncols .AND. &
                csc_mat%nnz == nnz) THEN
                PRINT *, "  PASSED"
                passed_tests = passed_tests + 1
            ELSE
                PRINT *, "  FAILED: Incorrect dimensions or nnz"
                failed_tests = failed_tests + 1
            END IF
        END IF
    END IF
    
    ! Test 4: COO to CSR conversion
    PRINT *, "Test 4: COO to CSR conversion"
    total_tests = total_tests + 1
    
    CALL DCSRALLOC(nrows, ncols, nnz, csr_conv, info)
    IF (info /= SPARSE_SUCCESS) THEN
        PRINT *, "  FAILED: DCSRALLOC for conversion returned info =", info
        failed_tests = failed_tests + 1
    ELSE
        CALL DCOO2CSR(coo_mat, csr_conv, info)
        IF (info /= SPARSE_SUCCESS) THEN
            PRINT *, "  FAILED: DCOO2CSR returned info =", info
            failed_tests = failed_tests + 1
        ELSE
            ! Verify the conversion by checking row pointers
            IF (ALL(csr_conv%row_ptr(1:nrows+1) == row_ptr)) THEN
                PRINT *, "  PASSED"
                passed_tests = passed_tests + 1
            ELSE
                PRINT *, "  FAILED: Incorrect row pointers after conversion"
                PRINT *, "  Expected:", row_ptr
                PRINT *, "  Got:     ", csr_conv%row_ptr(1:nrows+1)
                failed_tests = failed_tests + 1
            END IF
        END IF
    END IF
    
    ! Test 5: Error handling - invalid dimensions
    PRINT *, "Test 5: Error handling - invalid dimensions"
    total_tests = total_tests + 1
    
    CALL DCOOALLOC(-1, ncols, nnz, coo_mat, info)
    IF (info == -1) THEN
        PRINT *, "  PASSED"
        passed_tests = passed_tests + 1
    ELSE
        PRINT *, "  FAILED: Expected info = -1, got", info
        failed_tests = failed_tests + 1
    END IF
    
    ! Test 6: DCOOCHECK - valid matrix
    PRINT *, "Test 6: DCOOCHECK - valid matrix"
    total_tests = total_tests + 1
    
    ! Reinitialize with valid data
    CALL DCOOALLOC(nrows, ncols, nnz, coo_mat, info)
    CALL DCOOINIT(row_ind, col_ind, values, nnz, coo_mat, info)
    CALL DCOOCHECK(coo_mat, info)
    IF (info == SPARSE_SUCCESS) THEN
        PRINT *, "  PASSED"
        passed_tests = passed_tests + 1
    ELSE
        PRINT *, "  FAILED: Expected info = 0, got", info
        failed_tests = failed_tests + 1
    END IF
    
    ! Test 7: DCOOCHECK - duplicate entries
    PRINT *, "Test 7: DCOOCHECK - duplicate entries"
    total_tests = total_tests + 1
    
    ! Create matrix with duplicate entry
    coo_mat%row_ind(2) = 1  ! Same as first entry
    coo_mat%col_ind(2) = 1  ! Same as first entry
    CALL DCOOCHECK(coo_mat, info)
    IF (info == SPARSE_ERR_INVALID) THEN
        PRINT *, "  PASSED"
        passed_tests = passed_tests + 1
    ELSE
        PRINT *, "  FAILED: Expected duplicate error, got", info
        failed_tests = failed_tests + 1
    END IF
    
    ! Test 8: DCOOFREE
    PRINT *, "Test 8: DCOOFREE - deallocate matrix"
    total_tests = total_tests + 1
    
    CALL DCOOFREE(coo_mat, info)
    IF (info == SPARSE_SUCCESS .AND. coo_mat%nnz == 0 .AND. &
        .NOT. ALLOCATED(coo_mat%row_ind)) THEN
        PRINT *, "  PASSED"
        passed_tests = passed_tests + 1
    ELSE
        PRINT *, "  FAILED: Matrix not properly deallocated"
        failed_tests = failed_tests + 1
    END IF
    
    ! Summary
    PRINT *, ""
    PRINT *, "======================================"
    PRINT *, "Test Summary:"
    PRINT *, "Total tests:  ", total_tests
    PRINT *, "Passed tests: ", passed_tests
    PRINT *, "Failed tests: ", failed_tests
    PRINT *, "======================================"
    
    IF (failed_tests == 0) THEN
        PRINT *, "ALL TESTS PASSED!"
        STOP 0
    ELSE
        PRINT *, "SOME TESTS FAILED!"
        STOP 1
    END IF
    
END PROGRAM test_sparse_init