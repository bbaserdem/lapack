!> \brief Memory allocation and management tests for sparse matrices
!>
!> Tests proper allocation, deallocation, and memory management

PROGRAM DSPMEM
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64, output_unit, int64
    IMPLICIT NONE
    
    ! Local variables
    INTEGER :: INFO, NTEST, NPASS, I
    CHARACTER(LEN=80) :: TEST_NAME
    INTEGER(int64) :: MEM_START, MEM_END, MEM_DIFF
    
    ! Initialize
    NTEST = 0
    NPASS = 0
    
    WRITE(output_unit, '(A)') 'Sparse Matrix Memory Management Tests'
    WRITE(output_unit, '(A)') '====================================='
    WRITE(output_unit, *)
    
    ! Test 1: Basic allocation and deallocation
    TEST_NAME = 'Basic allocation/deallocation cycle'
    CALL TEST_BASIC_ALLOC_DEALLOC(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS)
    
    ! Test 2: Multiple allocations
    TEST_NAME = 'Multiple simultaneous allocations'
    CALL TEST_MULTIPLE_ALLOC(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS)
    
    ! Test 3: Reallocation stress test
    TEST_NAME = 'Reallocation stress test'
    CALL TEST_REALLOC_STRESS(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS)
    
    ! Test 4: Format conversion memory
    TEST_NAME = 'Format conversion memory management'
    CALL TEST_CONVERSION_MEMORY(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS)
    
    ! Test 5: Large allocation test
    TEST_NAME = 'Large matrix allocation'
    CALL TEST_LARGE_ALLOC(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS)
    
    ! Test 6: Memory fragmentation test
    TEST_NAME = 'Memory fragmentation test'
    CALL TEST_FRAGMENTATION(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS)
    
    ! Test 7: Allocation failure handling
    TEST_NAME = 'Allocation failure handling'
    CALL TEST_ALLOC_FAILURE(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS)
    
    ! Test 8: Memory consistency after operations
    TEST_NAME = 'Memory consistency check'
    CALL TEST_MEMORY_CONSISTENCY(TEST_NAME, INFO)
    CALL REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS)
    
    ! Summary
    WRITE(output_unit, *)
    WRITE(output_unit, '(A)') 'Memory Test Summary'
    WRITE(output_unit, '(A)') '==================='
    WRITE(output_unit, '(A, I4)') 'Total tests: ', NTEST
    WRITE(output_unit, '(A, I4)') 'Passed:      ', NPASS
    WRITE(output_unit, '(A, I4)') 'Failed:      ', NTEST - NPASS
    
    IF (NPASS == NTEST) THEN
        WRITE(output_unit, *)
        WRITE(output_unit, '(A)') 'All memory management tests PASSED!'
    END IF
    
CONTAINS

    SUBROUTINE REPORT_TEST(TEST_NAME, INFO, NTEST, NPASS)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(IN) :: INFO
        INTEGER, INTENT(INOUT) :: NTEST, NPASS
        
        NTEST = NTEST + 1
        IF (INFO == 0) THEN
            NPASS = NPASS + 1
            WRITE(output_unit, '(A50, A)') TEST_NAME, ' ... PASSED'
        ELSE
            WRITE(output_unit, '(A50, A, I6)') TEST_NAME, ' ... FAILED (INFO=', INFO, ')'
        END IF
    END SUBROUTINE REPORT_TEST
    
    !> Test basic allocation and deallocation
    SUBROUTINE TEST_BASIC_ALLOC_DEALLOC(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        TYPE(sparse_csr_d) :: CSR
        TYPE(sparse_csc_d) :: CSC
        INTEGER :: N, NNZ, I
        
        INFO = 0
        N = 100
        NNZ = 500
        
        ! Test COO allocation/deallocation
        CALL DCOOALLOC(N, N, NNZ, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Verify allocation
        IF (.NOT. ALLOCATED(COO%row_ind)) THEN
            INFO = -1
            RETURN
        END IF
        IF (.NOT. ALLOCATED(COO%col_ind)) THEN
            INFO = -2
            RETURN
        END IF
        IF (.NOT. ALLOCATED(COO%values)) THEN
            INFO = -3
            RETURN
        END IF
        IF (SIZE(COO%row_ind) /= NNZ) THEN
            INFO = -4
            RETURN
        END IF
        
        ! Fill with some data
        DO I = 1, MIN(10, NNZ)
            COO%row_ind(I) = I
            COO%col_ind(I) = I
            COO%values(I) = REAL(I, real64)
        END DO
        COO%nnz = MIN(10, NNZ)
        
        ! Deallocate
        CALL DCOOFREE(COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Verify deallocation
        IF (ALLOCATED(COO%row_ind)) THEN
            INFO = -5
            RETURN
        END IF
        IF (ALLOCATED(COO%col_ind)) THEN
            INFO = -6
            RETURN
        END IF
        IF (ALLOCATED(COO%values)) THEN
            INFO = -7
            RETURN
        END IF
        
        ! Test CSR allocation/deallocation
        ALLOCATE(CSR%row_ptr(N+1))
        ALLOCATE(CSR%col_ind(NNZ))
        ALLOCATE(CSR%values(NNZ))
        CSR%nrows = N
        CSR%ncols = N
        CSR%nnz = NNZ
        
        CALL DCSRFREE(CSR, INFO)
        IF (INFO /= 0) RETURN
        
        IF (ALLOCATED(CSR%row_ptr) .OR. ALLOCATED(CSR%col_ind) .OR. &
            ALLOCATED(CSR%values)) THEN
            INFO = -8
            RETURN
        END IF
        
    END SUBROUTINE TEST_BASIC_ALLOC_DEALLOC
    
    !> Test multiple simultaneous allocations
    SUBROUTINE TEST_MULTIPLE_ALLOC(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        INTEGER, PARAMETER :: NMAT = 10
        TYPE(sparse_coo_d) :: COO_ARRAY(NMAT)
        INTEGER :: I, N, NNZ
        
        INFO = 0
        
        ! Allocate multiple matrices
        DO I = 1, NMAT
            N = 10 * I
            NNZ = 5 * N
            CALL DCOOALLOC(N, N, NNZ, COO_ARRAY(I), INFO)
            IF (INFO /= 0) THEN
                INFO = -I
                RETURN
            END IF
        END DO
        
        ! Verify all are allocated
        DO I = 1, NMAT
            IF (.NOT. ALLOCATED(COO_ARRAY(I)%row_ind)) THEN
                INFO = -100 - I
                RETURN
            END IF
        END DO
        
        ! Deallocate in reverse order
        DO I = NMAT, 1, -1
            CALL DCOOFREE(COO_ARRAY(I), INFO)
            IF (INFO /= 0) RETURN
        END DO
        
        ! Verify all are deallocated
        DO I = 1, NMAT
            IF (ALLOCATED(COO_ARRAY(I)%row_ind)) THEN
                INFO = -200 - I
                RETURN
            END IF
        END DO
        
    END SUBROUTINE TEST_MULTIPLE_ALLOC
    
    !> Stress test with many reallocations
    SUBROUTINE TEST_REALLOC_STRESS(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        INTEGER :: I, J, N, NNZ, CYCLE
        
        INFO = 0
        
        ! Perform multiple allocation/deallocation cycles
        DO CYCLE = 1, 100
            ! Varying sizes
            N = 10 + MOD(CYCLE * 17, 90)
            NNZ = N * MOD(CYCLE * 13, 10)
            
            CALL DCOOALLOC(N, N, NNZ, COO, INFO)
            IF (INFO /= 0) RETURN
            
            ! Fill with dummy data
            DO I = 1, MIN(10, NNZ)
                COO%row_ind(I) = MOD(I, N) + 1
                COO%col_ind(I) = MOD(I*2, N) + 1
                COO%values(I) = REAL(I, real64)
            END DO
            COO%nnz = MIN(10, NNZ)
            
            CALL DCOOFREE(COO, INFO)
            IF (INFO /= 0) RETURN
        END DO
        
    END SUBROUTINE TEST_REALLOC_STRESS
    
    !> Test memory in format conversions
    SUBROUTINE TEST_CONVERSION_MEMORY(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        TYPE(sparse_csr_d) :: CSR
        TYPE(sparse_csc_d) :: CSC
        INTEGER :: N, NNZ, I
        INTEGER, ALLOCATABLE :: ROW_IND(:), COL_IND(:)
        REAL(real64), ALLOCATABLE :: VALUES(:)
        
        INFO = 0
        N = 50
        NNZ = 200
        
        ! Create test data
        ALLOCATE(ROW_IND(NNZ), COL_IND(NNZ), VALUES(NNZ))
        DO I = 1, NNZ
            ROW_IND(I) = MOD(I-1, N) + 1
            COL_IND(I) = MOD(I*3-1, N) + 1
            VALUES(I) = REAL(I, real64)
        END DO
        
        ! Allocate and initialize COO
        CALL DCOOALLOC(N, N, NNZ, COO, INFO)
        IF (INFO /= 0) RETURN
        
        CALL DCOOINIT(ROW_IND, COL_IND, VALUES, NNZ, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Convert to CSR
        CALL DCOOCONV('CSR', COO, CSR, INFO)
        IF (INFO /= 0) RETURN
        
        ! Verify CSR is allocated
        IF (.NOT. ALLOCATED(CSR%row_ptr) .OR. &
            .NOT. ALLOCATED(CSR%col_ind) .OR. &
            .NOT. ALLOCATED(CSR%values)) THEN
            INFO = -1
            RETURN
        END IF
        
        ! Convert to CSC
        CALL DCOOCONV('CSC', COO, CSC, INFO)
        IF (INFO /= 0) RETURN
        
        ! Verify CSC is allocated
        IF (.NOT. ALLOCATED(CSC%col_ptr) .OR. &
            .NOT. ALLOCATED(CSC%row_ind) .OR. &
            .NOT. ALLOCATED(CSC%values)) THEN
            INFO = -2
            RETURN
        END IF
        
        ! Clean up
        CALL DCOOFREE(COO, INFO)
        CALL DCSRFREE(CSR, INFO)
        CALL DCSCFREE(CSC, INFO)
        DEALLOCATE(ROW_IND, COL_IND, VALUES)
        
    END SUBROUTINE TEST_CONVERSION_MEMORY
    
    !> Test large allocation
    SUBROUTINE TEST_LARGE_ALLOC(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        INTEGER :: N, NNZ
        
        INFO = 0
        
        ! Try large allocation
        N = 10000
        NNZ = 1000000
        
        CALL DCOOALLOC(N, N, NNZ, COO, INFO)
        IF (INFO == SPARSE_ERR_ALLOC) THEN
            ! Allocation failed due to memory - this is acceptable
            INFO = 0
            RETURN
        END IF
        
        IF (INFO /= 0) RETURN
        
        ! Verify size
        IF (SIZE(COO%row_ind) /= NNZ) THEN
            INFO = -1
        END IF
        
        CALL DCOOFREE(COO, INFO)
        
    END SUBROUTINE TEST_LARGE_ALLOC
    
    !> Test memory fragmentation scenario
    SUBROUTINE TEST_FRAGMENTATION(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO1, COO2, COO3
        INTEGER :: I
        
        INFO = 0
        
        ! Allocate in pattern: small, large, small
        CALL DCOOALLOC(10, 10, 50, COO1, INFO)
        IF (INFO /= 0) RETURN
        
        CALL DCOOALLOC(100, 100, 5000, COO2, INFO)
        IF (INFO /= 0) RETURN
        
        CALL DCOOALLOC(10, 10, 50, COO3, INFO)
        IF (INFO /= 0) RETURN
        
        ! Free the middle one
        CALL DCOOFREE(COO2, INFO)
        IF (INFO /= 0) RETURN
        
        ! Try to allocate a large one again
        CALL DCOOALLOC(100, 100, 5000, COO2, INFO)
        IF (INFO == SPARSE_ERR_ALLOC) THEN
            ! Memory fragmentation might prevent allocation
            INFO = 0
        END IF
        
        ! Clean up
        CALL DCOOFREE(COO1, INFO)
        IF (ALLOCATED(COO2%row_ind)) CALL DCOOFREE(COO2, INFO)
        CALL DCOOFREE(COO3, INFO)
        
    END SUBROUTINE TEST_FRAGMENTATION
    
    !> Test allocation failure handling
    SUBROUTINE TEST_ALLOC_FAILURE(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        INTEGER :: N, NNZ
        
        INFO = 0
        
        ! Try impossibly large allocation
        N = HUGE(N) / 10
        NNZ = HUGE(NNZ) / 10
        
        CALL DCOOALLOC(N, N, NNZ, COO, INFO)
        
        ! Should fail with allocation error
        IF (INFO == SPARSE_ERR_ALLOC) THEN
            INFO = 0  ! Expected behavior
        ELSE IF (INFO == 0) THEN
            ! If it somehow succeeded, clean up
            CALL DCOOFREE(COO, INFO)
            INFO = -1  ! Unexpected success
        END IF
        
    END SUBROUTINE TEST_ALLOC_FAILURE
    
    !> Test memory consistency after operations
    SUBROUTINE TEST_MEMORY_CONSISTENCY(TEST_NAME, INFO)
        CHARACTER(LEN=*), INTENT(IN) :: TEST_NAME
        INTEGER, INTENT(OUT) :: INFO
        
        TYPE(sparse_coo_d) :: COO
        INTEGER :: N, NNZ, I, J
        INTEGER, ALLOCATABLE :: ROW_IND(:), COL_IND(:)
        REAL(real64), ALLOCATABLE :: VALUES(:), X(:), Y(:)
        REAL(real64) :: SUM_BEFORE, SUM_AFTER
        
        INFO = 0
        N = 100
        NNZ = 500
        
        ! Create test matrix
        ALLOCATE(ROW_IND(NNZ), COL_IND(NNZ), VALUES(NNZ))
        ALLOCATE(X(N), Y(N))
        
        DO I = 1, NNZ
            ROW_IND(I) = MOD(I-1, N) + 1
            COL_IND(I) = MOD(I*7-1, N) + 1
            VALUES(I) = REAL(I, real64) / REAL(NNZ, real64)
        END DO
        
        ! Calculate checksum
        SUM_BEFORE = SUM(VALUES)
        
        CALL DCOOALLOC(N, N, NNZ, COO, INFO)
        IF (INFO /= 0) RETURN
        
        CALL DCOOINIT(ROW_IND, COL_IND, VALUES, NNZ, COO, INFO)
        IF (INFO /= 0) RETURN
        
        ! Perform operations
        X = 1.0_real64
        Y = 0.0_real64
        
        DO J = 1, 10
            CALL DCOOMV('N', N, N, 1.0_real64, COO, X, 1, 0.0_real64, Y, 1)
            CALL DCOOMV('T', N, N, 1.0_real64, COO, Y, 1, 0.0_real64, X, 1)
        END DO
        
        ! Check matrix values haven't been corrupted
        SUM_AFTER = SUM(COO%values(1:COO%nnz))
        
        IF (ABS(SUM_AFTER - SUM_BEFORE) > 1.0E-12_real64 * ABS(SUM_BEFORE)) THEN
            INFO = -1
        END IF
        
        ! Clean up
        CALL DCOOFREE(COO, INFO)
        DEALLOCATE(ROW_IND, COL_IND, VALUES, X, Y)
        
    END SUBROUTINE TEST_MEMORY_CONSISTENCY

END PROGRAM DSPMEM