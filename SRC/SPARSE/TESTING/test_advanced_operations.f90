!> \brief Test suite for advanced sparse matrix operations
!>
!> Tests the following routines:
!> - DCSRCSC: CSR × CSC multiplication
!> - DGECSR: Dense × CSR multiplication
!> - DSPADD2: Sparse matrix addition
!> - DSPSORT: Index sorting
!> - DSPCOMP: Duplicate/zero removal
!> - DSPSIZE: Dynamic reallocation

PROGRAM test_advanced_operations
    USE sparse_types
    USE ISO_FORTRAN_ENV, ONLY: int32, real64, output_unit
    IMPLICIT NONE
    
    ! Test counters
    INTEGER :: tests_passed = 0
    INTEGER :: tests_failed = 0
    
    ! Interface declarations for advanced operations
    INTERFACE
        SUBROUTINE DCSRCSC(TRANSA, TRANSB, M, N, K, ALPHA, A, B, BETA, C)
            USE sparse_types
            USE ISO_FORTRAN_ENV, ONLY: int32, real64
            CHARACTER, INTENT(IN) :: TRANSA, TRANSB
            INTEGER(int32), INTENT(IN) :: M, N, K
            REAL(real64), INTENT(IN) :: ALPHA, BETA
            TYPE(sparse_csr_d), INTENT(IN) :: A
            TYPE(sparse_csc_d), INTENT(IN) :: B
            TYPE(sparse_csr_d), INTENT(INOUT) :: C
        END SUBROUTINE DCSRCSC
        
        SUBROUTINE DGECSR(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, BETA, C, LDC)
            USE sparse_types
            USE ISO_FORTRAN_ENV, ONLY: int32, real64
            CHARACTER, INTENT(IN) :: TRANSA, TRANSB
            INTEGER(int32), INTENT(IN) :: M, N, K, LDA, LDC
            REAL(real64), INTENT(IN) :: ALPHA, BETA
            REAL(real64), INTENT(IN) :: A(LDA,*)
            TYPE(sparse_csr_d), INTENT(IN) :: B
            REAL(real64), INTENT(INOUT) :: C(LDC,*)
        END SUBROUTINE DGECSR
        
        SUBROUTINE DSPADD2(FORMAT, M, N, ALPHA, A, BETA, B, C, INFO)
            USE ISO_FORTRAN_ENV, ONLY: int32, real64
            CHARACTER, INTENT(IN) :: FORMAT
            INTEGER(int32), INTENT(IN) :: M, N
            REAL(real64), INTENT(IN) :: ALPHA, BETA
            CLASS(*), INTENT(IN) :: A, B
            CLASS(*), INTENT(OUT) :: C
            INTEGER(int32), INTENT(OUT) :: INFO
        END SUBROUTINE DSPADD2
        
        SUBROUTINE DSPSORT(FORMAT, SPARSE, INFO)
            USE ISO_FORTRAN_ENV, ONLY: int32
            CHARACTER, INTENT(IN) :: FORMAT
            CLASS(*), INTENT(INOUT) :: SPARSE
            INTEGER(int32), INTENT(OUT) :: INFO
        END SUBROUTINE DSPSORT
        
        SUBROUTINE DSPCOMP(FORMAT, SPARSE, TOL, INFO)
            USE ISO_FORTRAN_ENV, ONLY: int32, real64
            CHARACTER, INTENT(IN) :: FORMAT
            CLASS(*), INTENT(INOUT) :: SPARSE
            REAL(real64), INTENT(IN), OPTIONAL :: TOL
            INTEGER(int32), INTENT(OUT) :: INFO
        END SUBROUTINE DSPCOMP
        
        SUBROUTINE DSPSIZE(FORMAT, SPARSE, NEW_NNZ, INFO)
            USE ISO_FORTRAN_ENV, ONLY: int32
            CHARACTER, INTENT(IN) :: FORMAT
            CLASS(*), INTENT(INOUT) :: SPARSE
            INTEGER(int32), INTENT(IN) :: NEW_NNZ
            INTEGER(int32), INTENT(OUT) :: INFO
        END SUBROUTINE DSPSIZE
    END INTERFACE
    
    ! Run all tests
    WRITE(output_unit, '(A)') 'Testing Advanced Sparse Matrix Operations'
    WRITE(output_unit, '(A)') '========================================='
    
    CALL test_dcsrcsc()
    CALL test_dgecsr()
    CALL test_dspadd2()
    CALL test_dspsort()
    CALL test_dspcomp()
    CALL test_dspsize()
    
    ! Summary
    WRITE(output_unit, '(A)') ''
    WRITE(output_unit, '(A,I0,A,I0,A)') 'Tests: ', tests_passed, ' passed, ', &
                                         tests_failed, ' failed'
    
    IF (tests_failed > 0) THEN
        STOP 1
    END IF
    
CONTAINS

    !> Test DCSRCSC sparse-sparse multiplication
    SUBROUTINE test_dcsrcsc()
        TYPE(sparse_csr_d) :: A, C
        TYPE(sparse_csc_d) :: B
        REAL(real64) :: alpha, beta
        INTEGER(int32) :: i, info
        LOGICAL :: test_passed
    
    ! External function declaration
    LOGICAL, EXTERNAL :: LSAME
        
        WRITE(output_unit, '(A)') ''
        WRITE(output_unit, '(A)') 'Testing DCSRCSC (CSR × CSC multiplication)...'
        
        ! Create test matrices
        ! A = [1 2 0]    B = [5 0]
        !     [0 3 4]        [6 7]
        !                    [0 8]
        
        ! Initialize CSR matrix A
        A%nrows = 2
        A%ncols = 3
        A%nnz = 4
        A%nnz_alloc = 4
        ALLOCATE(A%row_ptr(3), A%col_ind(4), A%values(4))
        A%row_ptr = [1, 3, 5]
        A%col_ind = [1, 2, 2, 3]
        A%values = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        
        ! Initialize CSC matrix B
        B%nrows = 3
        B%ncols = 2
        B%nnz = 4
        B%nnz_alloc = 4
        ALLOCATE(B%col_ptr(3), B%row_ind(4), B%values(4))
        B%col_ptr = [1, 3, 5]
        B%row_ind = [1, 2, 2, 3]
        B%values = [5.0_real64, 6.0_real64, 7.0_real64, 8.0_real64]
        
        ! Test multiplication C = A*B
        alpha = 1.0_real64
        beta = 0.0_real64
        
        CALL DCSRCSC('N', 'N', 2, 2, 3, alpha, A, B, beta, C)
        
        ! Expected result C = [17 14]
        !                     [18 53]
        test_passed = .TRUE.
        
        ! Check dimensions
        IF (C%nrows /= 2 .OR. C%ncols /= 2 .OR. C%nnz /= 4) THEN
            test_passed = .FALSE.
            WRITE(output_unit, '(A)') '  FAILED: Incorrect dimensions'
        END IF
        
        ! Check values (assuming sorted)
        IF (test_passed) THEN
            ! Row 1: (1,1)=17, (1,2)=14
            IF (C%row_ptr(1) /= 1 .OR. C%row_ptr(2) /= 3) THEN
                test_passed = .FALSE.
            ELSE IF (C%col_ind(1) /= 1 .OR. ABS(C%values(1) - 17.0_real64) > 1e-10) THEN
                test_passed = .FALSE.
            ELSE IF (C%col_ind(2) /= 2 .OR. ABS(C%values(2) - 14.0_real64) > 1e-10) THEN
                test_passed = .FALSE.
            END IF
            
            ! Row 2: (2,1)=18, (2,2)=53
            IF (C%row_ptr(3) /= 5) THEN
                test_passed = .FALSE.
            ELSE IF (C%col_ind(3) /= 1 .OR. ABS(C%values(3) - 18.0_real64) > 1e-10) THEN
                test_passed = .FALSE.
            ELSE IF (C%col_ind(4) /= 2 .OR. ABS(C%values(4) - 53.0_real64) > 1e-10) THEN
                test_passed = .FALSE.
            END IF
        END IF
        
        IF (test_passed) THEN
            WRITE(output_unit, '(A)') '  PASSED'
            tests_passed = tests_passed + 1
        ELSE
            WRITE(output_unit, '(A)') '  FAILED: Incorrect result'
            tests_failed = tests_failed + 1
        END IF
        
        ! Clean up
        DEALLOCATE(A%row_ptr, A%col_ind, A%values)
        DEALLOCATE(B%col_ptr, B%row_ind, B%values)
        DEALLOCATE(C%row_ptr, C%col_ind, C%values)
        
    END SUBROUTINE test_dcsrcsc
    
    !> Test DGECSR dense-sparse multiplication
    SUBROUTINE test_dgecsr()
        TYPE(sparse_csr_d) :: B
        REAL(real64) :: A(2,3), C(2,2), C_expected(2,2)
        REAL(real64) :: alpha, beta
        INTEGER(int32) :: info
        LOGICAL :: test_passed
        
        WRITE(output_unit, '(A)') ''
        WRITE(output_unit, '(A)') 'Testing DGECSR (Dense × CSR multiplication)...'
        
        ! Create test matrices
        ! A = [1 2 3]    B = [5 0]
        !     [4 5 6]        [6 7]
        !                    [0 8]
        
        ! Initialize dense matrix A (column-major)
        A(1,1) = 1.0_real64; A(2,1) = 4.0_real64
        A(1,2) = 2.0_real64; A(2,2) = 5.0_real64
        A(1,3) = 3.0_real64; A(2,3) = 6.0_real64
        
        ! Initialize CSR matrix B
        B%nrows = 3
        B%ncols = 2
        B%nnz = 4
        B%nnz_alloc = 4
        ALLOCATE(B%row_ptr(4), B%col_ind(4), B%values(4))
        B%row_ptr = [1, 2, 4, 5]
        B%col_ind = [1, 1, 2, 2]
        B%values = [5.0_real64, 6.0_real64, 7.0_real64, 8.0_real64]
        
        ! Test multiplication C = A*B
        alpha = 1.0_real64
        beta = 0.0_real64
        C = 0.0_real64
        
        CALL DGECSR('N', 'N', 2, 2, 3, alpha, A, 2, B, beta, C, 2)
        
        ! Expected result C = [17 38]
        !                     [50 107]
        C_expected(1,1) = 17.0_real64; C_expected(2,1) = 50.0_real64
        C_expected(1,2) = 38.0_real64; C_expected(2,2) = 107.0_real64
        
        test_passed = .TRUE.
        IF (ABS(C(1,1) - C_expected(1,1)) > 1e-10 .OR. &
            ABS(C(2,1) - C_expected(2,1)) > 1e-10 .OR. &
            ABS(C(1,2) - C_expected(1,2)) > 1e-10 .OR. &
            ABS(C(2,2) - C_expected(2,2)) > 1e-10) THEN
            test_passed = .FALSE.
        END IF
        
        IF (test_passed) THEN
            WRITE(output_unit, '(A)') '  PASSED'
            tests_passed = tests_passed + 1
        ELSE
            WRITE(output_unit, '(A)') '  FAILED: Incorrect result'
            tests_failed = tests_failed + 1
        END IF
        
        ! Clean up
        DEALLOCATE(B%row_ptr, B%col_ind, B%values)
        
    END SUBROUTINE test_dgecsr
    
    !> Test DSPADD2 sparse matrix addition
    SUBROUTINE test_dspadd2()
        TYPE(sparse_csr_d) :: A, B, C
        REAL(real64) :: alpha, beta
        INTEGER(int32) :: info, i
        LOGICAL :: test_passed
        
        WRITE(output_unit, '(A)') ''
        WRITE(output_unit, '(A)') 'Testing DSPADD2 (Sparse matrix addition)...'
        
        ! Create test matrices
        ! A = [1 0 2]    B = [0 3 0]
        !     [0 3 0]        [4 0 5]
        
        ! Initialize CSR matrix A
        A%nrows = 2
        A%ncols = 3
        A%nnz = 3
        A%nnz_alloc = 3
        ALLOCATE(A%row_ptr(3), A%col_ind(3), A%values(3))
        A%row_ptr = [1, 3, 4]
        A%col_ind = [1, 3, 2]
        A%values = [1.0_real64, 2.0_real64, 3.0_real64]
        
        ! Initialize CSR matrix B
        B%nrows = 2
        B%ncols = 3
        B%nnz = 3
        B%nnz_alloc = 3
        ALLOCATE(B%row_ptr(3), B%col_ind(3), B%values(3))
        B%row_ptr = [1, 2, 4]
        B%col_ind = [2, 1, 3]
        B%values = [3.0_real64, 4.0_real64, 5.0_real64]
        
        ! Test addition C = 2*A + 3*B
        alpha = 2.0_real64
        beta = 3.0_real64
        
        CALL DSPADD2('R', 2, 3, alpha, A, beta, B, C, info)
        
        test_passed = (info == 0)
        
        ! Expected result C = [2 9 4]
        !                     [12 6 15]
        ! Total 6 non-zeros
        
        IF (test_passed .AND. C%nnz /= 6) THEN
            test_passed = .FALSE.
            WRITE(output_unit, '(A,I0,A)') '  FAILED: Expected 6 non-zeros, got ', C%nnz
        END IF
        
        IF (test_passed) THEN
            WRITE(output_unit, '(A)') '  PASSED'
            tests_passed = tests_passed + 1
        ELSE
            WRITE(output_unit, '(A)') '  FAILED'
            tests_failed = tests_failed + 1
        END IF
        
        ! Clean up
        DEALLOCATE(A%row_ptr, A%col_ind, A%values)
        DEALLOCATE(B%row_ptr, B%col_ind, B%values)
        IF (ALLOCATED(C%row_ptr)) DEALLOCATE(C%row_ptr, C%col_ind, C%values)
        
    END SUBROUTINE test_dspadd2
    
    !> Test DSPSORT index sorting
    SUBROUTINE test_dspsort()
        TYPE(sparse_coo_d) :: coo
        INTEGER(int32) :: info, i
        LOGICAL :: test_passed
        
        WRITE(output_unit, '(A)') ''
        WRITE(output_unit, '(A)') 'Testing DSPSORT (Index sorting)...'
        
        ! Create unsorted COO matrix
        coo%nrows = 3
        coo%ncols = 3
        coo%nnz = 5
        coo%nnz_alloc = 5
        ALLOCATE(coo%row_ind(5), coo%col_ind(5), coo%values(5))
        
        ! Unsorted entries
        coo%row_ind = [2, 1, 3, 1, 2]
        coo%col_ind = [2, 3, 1, 1, 1]
        coo%values = [2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        coo%sorted = .FALSE.
        
        CALL DSPSORT('O', coo, info)
        
        test_passed = (info == 0 .AND. coo%sorted)
        
        ! Check if sorted correctly (lexicographic order)
        IF (test_passed) THEN
            DO i = 2, coo%nnz
                IF (coo%row_ind(i-1) > coo%row_ind(i) .OR. &
                    (coo%row_ind(i-1) == coo%row_ind(i) .AND. &
                     coo%col_ind(i-1) > coo%col_ind(i))) THEN
                    test_passed = .FALSE.
                    EXIT
                END IF
            END DO
        END IF
        
        IF (test_passed) THEN
            WRITE(output_unit, '(A)') '  PASSED'
            tests_passed = tests_passed + 1
        ELSE
            WRITE(output_unit, '(A)') '  FAILED: Not properly sorted'
            tests_failed = tests_failed + 1
        END IF
        
        ! Clean up
        DEALLOCATE(coo%row_ind, coo%col_ind, coo%values)
        
    END SUBROUTINE test_dspsort
    
    !> Test DSPCOMP duplicate and zero removal
    SUBROUTINE test_dspcomp()
        TYPE(sparse_coo_d) :: coo
        INTEGER(int32) :: info
        LOGICAL :: test_passed
        
        WRITE(output_unit, '(A)') ''
        WRITE(output_unit, '(A)') 'Testing DSPCOMP (Duplicate/zero removal)...'
        
        ! Create COO matrix with duplicates and zeros
        coo%nrows = 3
        coo%ncols = 3
        coo%nnz = 7
        coo%nnz_alloc = 7
        ALLOCATE(coo%row_ind(7), coo%col_ind(7), coo%values(7))
        
        ! Include duplicates and a zero
        coo%row_ind = [1, 1, 2, 2, 2, 3, 3]
        coo%col_ind = [1, 1, 2, 2, 3, 1, 2]
        coo%values = [2.0_real64, 3.0_real64, 1.0_real64, -1.0_real64, &
                      4.0_real64, 5.0_real64, 0.0_real64]
        coo%sorted = .FALSE.
        coo%checked = .FALSE.
        
        CALL DSPCOMP('O', coo, 1e-12_real64, info)
        
        test_passed = (info == 0 .AND. coo%checked)
        
        ! Should have 4 non-zeros after compression:
        ! (1,1)=5, (2,3)=4, (3,1)=5, zero at (2,2) and (3,2) removed
        IF (test_passed .AND. coo%nnz /= 3) THEN
            test_passed = .FALSE.
            WRITE(output_unit, '(A,I0,A)') '  FAILED: Expected 3 non-zeros, got ', coo%nnz
        END IF
        
        IF (test_passed) THEN
            WRITE(output_unit, '(A)') '  PASSED'
            tests_passed = tests_passed + 1
        ELSE
            WRITE(output_unit, '(A)') '  FAILED'
            tests_failed = tests_failed + 1
        END IF
        
        ! Clean up
        DEALLOCATE(coo%row_ind, coo%col_ind, coo%values)
        
    END SUBROUTINE test_dspcomp
    
    !> Test DSPSIZE dynamic reallocation
    SUBROUTINE test_dspsize()
        TYPE(sparse_csr_d) :: csr
        INTEGER(int32) :: info, old_size, new_size
        LOGICAL :: test_passed
        
        WRITE(output_unit, '(A)') ''
        WRITE(output_unit, '(A)') 'Testing DSPSIZE (Dynamic reallocation)...'
        
        ! Create CSR matrix
        csr%nrows = 2
        csr%ncols = 3
        csr%nnz = 3
        csr%nnz_alloc = 5
        ALLOCATE(csr%row_ptr(3), csr%col_ind(5), csr%values(5))
        csr%row_ptr = [1, 3, 4]
        csr%col_ind(1:3) = [1, 3, 2]
        csr%values(1:3) = [1.0_real64, 2.0_real64, 3.0_real64]
        
        old_size = csr%nnz_alloc
        new_size = 10
        
        CALL DSPSIZE('R', csr, new_size, info)
        
        test_passed = (info == 0 .AND. csr%nnz_alloc == new_size)
        
        ! Check data preserved
        IF (test_passed) THEN
            IF (csr%nnz /= 3 .OR. &
                csr%col_ind(1) /= 1 .OR. csr%col_ind(2) /= 3 .OR. csr%col_ind(3) /= 2 .OR. &
                ABS(csr%values(1) - 1.0_real64) > 1e-10 .OR. &
                ABS(csr%values(2) - 2.0_real64) > 1e-10 .OR. &
                ABS(csr%values(3) - 3.0_real64) > 1e-10) THEN
                test_passed = .FALSE.
                WRITE(output_unit, '(A)') '  FAILED: Data not preserved'
            END IF
        END IF
        
        ! Test shrinking
        IF (test_passed) THEN
            CALL DSPSIZE('R', csr, 3, info)
            test_passed = (info == 0 .AND. csr%nnz_alloc == 3)
        END IF
        
        IF (test_passed) THEN
            WRITE(output_unit, '(A)') '  PASSED'
            tests_passed = tests_passed + 1
        ELSE
            WRITE(output_unit, '(A)') '  FAILED'
            tests_failed = tests_failed + 1
        END IF
        
        ! Clean up
        DEALLOCATE(csr%row_ptr, csr%col_ind, csr%values)
        
    END SUBROUTINE test_dspsize

END PROGRAM test_advanced_operations