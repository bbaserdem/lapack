!> \brief Test program for sparse matrix I/O routines
!>
!> This program tests DSPREAD and DSPWRITE routines for Matrix Market format

PROGRAM test_io
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64, output_unit, error_unit
    IMPLICIT NONE
    
    ! Test matrices
    TYPE(sparse_coo_d) :: coo_write, coo_read
    
    ! Test parameters
    INTEGER, PARAMETER :: M = 10, N = 8
    CHARACTER(LEN=100) :: test_file = 'test_matrix.mtx'
    CHARACTER(LEN=100) :: test_file2 = 'test_symmetric.mtx'
    
    ! Local variables
    INTEGER :: info, i, j, k
    INTEGER :: test_count, pass_count
    LOGICAL :: test_ok, file_exists
    REAL(real64) :: tol = 1.0E-14_real64
    
    ! External routines
    EXTERNAL :: DCOOALLOC, DCOOINIT, DCOOFREE
    EXTERNAL :: DSPREAD, DSPWRITE
    
    WRITE(output_unit, '(A)') '========================================='
    WRITE(output_unit, '(A)') 'Testing Sparse Matrix I/O Routines'
    WRITE(output_unit, '(A)') '========================================='
    
    test_count = 0
    pass_count = 0
    
    ! Test 1: Write and read a general sparse matrix
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 1: Write/Read general sparse matrix'
    
    ! Create a test matrix
    CALL create_test_matrix(M, N, coo_write, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to create test matrix, INFO = ', info
        STOP 1
    END IF
    
    WRITE(output_unit, '(A,I0,A,I0)') 'Matrix size: ', M, ' x ', N
    WRITE(output_unit, '(A,I0)') 'Non-zeros: ', coo_write%nnz
    
    ! Write matrix to file
    CALL DSPWRITE(coo_write, test_file, 'MM', info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to write matrix, INFO = ', info
    ELSE
        WRITE(output_unit, '(A,A)') 'Matrix written to: ', TRIM(test_file)
        
        ! Check if file exists
        INQUIRE(FILE=test_file, EXIST=file_exists)
        IF (file_exists) THEN
            ! Read matrix back
            CALL DSPREAD(test_file, 'MM', coo_read, info)
            IF (info /= 0) THEN
                WRITE(error_unit, '(A,I0)') 'ERROR: Failed to read matrix, INFO = ', info
            ELSE
                ! Compare matrices
                test_ok = compare_coo_matrices(coo_write, coo_read)
                IF (test_ok) THEN
                    pass_count = pass_count + 1
                    WRITE(output_unit, '(A)') 'PASSED: Matrix read matches written matrix'
                ELSE
                    WRITE(error_unit, '(A)') 'FAILED: Matrix read does not match written matrix'
                END IF
                CALL DCOOFREE(coo_read, info)
            END IF
            
            ! Delete test file
            CALL delete_file(test_file)
        ELSE
            WRITE(error_unit, '(A)') 'ERROR: Output file was not created'
        END IF
    END IF
    
    CALL DCOOFREE(coo_write, info)
    
    ! Test 2: Write and read a symmetric sparse matrix
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 2: Write/Read symmetric sparse matrix'
    
    ! Create a symmetric test matrix
    CALL create_symmetric_matrix(N, coo_write, info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to create symmetric matrix, INFO = ', info
        STOP 1
    END IF
    
    WRITE(output_unit, '(A,I0,A,I0)') 'Matrix size: ', N, ' x ', N
    WRITE(output_unit, '(A,I0)') 'Non-zeros: ', coo_write%nnz
    
    ! Write matrix to file
    CALL DSPWRITE(coo_write, test_file2, 'MM', info)
    IF (info /= 0) THEN
        WRITE(error_unit, '(A,I0)') 'ERROR: Failed to write symmetric matrix, INFO = ', info
    ELSE
        WRITE(output_unit, '(A,A)') 'Symmetric matrix written to: ', TRIM(test_file2)
        
        ! Check if file exists
        INQUIRE(FILE=test_file2, EXIST=file_exists)
        IF (file_exists) THEN
            ! Read matrix back
            CALL DSPREAD(test_file2, 'MM', coo_read, info)
            IF (info /= 0) THEN
                WRITE(error_unit, '(A,I0)') 'ERROR: Failed to read symmetric matrix, INFO = ', info
            ELSE
                ! Compare matrices
                test_ok = compare_coo_matrices(coo_write, coo_read)
                IF (test_ok) THEN
                    pass_count = pass_count + 1
                    WRITE(output_unit, '(A)') 'PASSED: Symmetric matrix read matches written matrix'
                ELSE
                    WRITE(error_unit, '(A)') 'FAILED: Symmetric matrix read does not match'
                END IF
                CALL DCOOFREE(coo_read, info)
            END IF
            
            ! Delete test file
            CALL delete_file(test_file2)
        ELSE
            WRITE(error_unit, '(A)') 'ERROR: Symmetric output file was not created'
        END IF
    END IF
    
    CALL DCOOFREE(coo_write, info)
    
    ! Test 3: Error handling - invalid format
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 3: Error handling - invalid format'
    
    CALL create_test_matrix(5, 5, coo_write, info)
    CALL DSPWRITE(coo_write, 'dummy.txt', 'INVALID', info)
    IF (info < 0) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,I0)') 'PASSED: Invalid format detected, INFO = ', info
    ELSE
        WRITE(error_unit, '(A)') 'FAILED: Invalid format not detected'
    END IF
    CALL DCOOFREE(coo_write, info)
    
    ! Test 4: Error handling - non-existent file
    test_count = test_count + 1
    WRITE(output_unit, '(/A)') 'Test 4: Error handling - non-existent file'
    
    CALL DSPREAD('non_existent_file.mtx', 'MM', coo_read, info)
    IF (info == 1) THEN
        pass_count = pass_count + 1
        WRITE(output_unit, '(A,I0)') 'PASSED: Non-existent file detected, INFO = ', info
    ELSE
        WRITE(error_unit, '(A)') 'FAILED: Non-existent file not properly handled'
    END IF
    
    ! Summary
    WRITE(output_unit, '(/A)') '========================================='
    WRITE(output_unit, '(A,I0,A,I0)') 'Tests passed: ', pass_count, ' out of ', test_count
    WRITE(output_unit, '(A)') '========================================='
    
CONTAINS

    SUBROUTINE create_test_matrix(m, n, coo, info)
        INTEGER, INTENT(IN) :: m, n
        TYPE(sparse_coo_d), INTENT(OUT) :: coo
        INTEGER, INTENT(OUT) :: info
        
        INTEGER :: nnz, idx
        INTEGER, ALLOCATABLE :: row_ind(:), col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        
        EXTERNAL :: DCOOALLOC, DCOOINIT
        
        ! Create a sparse matrix with specific pattern
        nnz = MIN(m, n) + 2*(MIN(m,n)-1) + 5  ! diagonal + some off-diagonal
        
        ALLOCATE(row_ind(nnz), col_ind(nnz), values(nnz))
        
        idx = 0
        
        ! Diagonal elements
        DO i = 1, MIN(m, n)
            idx = idx + 1
            row_ind(idx) = i
            col_ind(idx) = i
            values(idx) = REAL(i, real64)
        END DO
        
        ! Sub-diagonal
        DO i = 2, MIN(m, n)
            idx = idx + 1
            row_ind(idx) = i
            col_ind(idx) = i-1
            values(idx) = -0.5_real64
        END DO
        
        ! Super-diagonal
        DO i = 1, MIN(m, n)-1
            idx = idx + 1
            row_ind(idx) = i
            col_ind(idx) = i+1
            values(idx) = -0.5_real64
        END DO
        
        ! Some additional elements
        IF (m >= 3 .AND. n >= 5) THEN
            idx = idx + 1
            row_ind(idx) = 3
            col_ind(idx) = 5
            values(idx) = 1.5_real64
        END IF
        
        IF (m >= 5 .AND. n >= 3) THEN
            idx = idx + 1
            row_ind(idx) = 5
            col_ind(idx) = 3
            values(idx) = 2.5_real64
        END IF
        
        IF (m >= 1 .AND. n >= 4) THEN
            idx = idx + 1
            row_ind(idx) = 1
            col_ind(idx) = 4
            values(idx) = 3.5_real64
        END IF
        
        IF (m >= 4 .AND. n >= 1) THEN
            idx = idx + 1
            row_ind(idx) = 4
            col_ind(idx) = 1
            values(idx) = 4.5_real64
        END IF
        
        IF (m >= 2 .AND. n >= 6) THEN
            idx = idx + 1
            row_ind(idx) = 2
            col_ind(idx) = 6
            values(idx) = 5.5_real64
        END IF
        
        nnz = idx
        
        ! Allocate and initialize COO matrix
        CALL DCOOALLOC(m, n, nnz, coo, info)
        IF (info /= 0) RETURN
        
        CALL DCOOINIT(row_ind(1:nnz), col_ind(1:nnz), values(1:nnz), nnz, coo, info)
        
        DEALLOCATE(row_ind, col_ind, values)
    END SUBROUTINE create_test_matrix
    
    SUBROUTINE create_symmetric_matrix(n, coo, info)
        INTEGER, INTENT(IN) :: n
        TYPE(sparse_coo_d), INTENT(OUT) :: coo
        INTEGER, INTENT(OUT) :: info
        
        INTEGER :: nnz, idx, i, j
        INTEGER, ALLOCATABLE :: row_ind(:), col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        
        EXTERNAL :: DCOOALLOC, DCOOINIT
        
        ! Create symmetric tridiagonal + some elements
        nnz = n + 2*(n-1) + 4  ! diagonal + sub/super-diagonal + extras
        
        ALLOCATE(row_ind(nnz), col_ind(nnz), values(nnz))
        
        idx = 0
        
        ! Diagonal
        DO i = 1, n
            idx = idx + 1
            row_ind(idx) = i
            col_ind(idx) = i
            values(idx) = 2.0_real64
        END DO
        
        ! Sub and super diagonals (symmetric)
        DO i = 1, n-1
            idx = idx + 1
            row_ind(idx) = i
            col_ind(idx) = i+1
            values(idx) = -1.0_real64
            
            idx = idx + 1
            row_ind(idx) = i+1
            col_ind(idx) = i
            values(idx) = -1.0_real64
        END DO
        
        ! Additional symmetric elements
        IF (n >= 5) THEN
            idx = idx + 1
            row_ind(idx) = 1
            col_ind(idx) = 5
            values(idx) = 0.5_real64
            
            idx = idx + 1
            row_ind(idx) = 5
            col_ind(idx) = 1
            values(idx) = 0.5_real64
        END IF
        
        nnz = idx
        
        ! Allocate and initialize COO matrix
        CALL DCOOALLOC(n, n, nnz, coo, info)
        IF (info /= 0) RETURN
        
        CALL DCOOINIT(row_ind(1:nnz), col_ind(1:nnz), values(1:nnz), nnz, coo, info)
        
        DEALLOCATE(row_ind, col_ind, values)
    END SUBROUTINE create_symmetric_matrix
    
    LOGICAL FUNCTION compare_coo_matrices(coo1, coo2) RESULT(are_equal)
        TYPE(sparse_coo_d), INTENT(IN) :: coo1, coo2
        INTEGER :: i, j, k1, k2
        REAL(real64) :: val1, val2
        LOGICAL :: found
        
        are_equal = .TRUE.
        
        ! Check dimensions
        IF (coo1%nrows /= coo2%nrows .OR. coo1%ncols /= coo2%ncols) THEN
            are_equal = .FALSE.
            RETURN
        END IF
        
        ! Check number of non-zeros
        IF (coo1%nnz /= coo2%nnz) THEN
            are_equal = .FALSE.
            RETURN
        END IF
        
        ! Check each element in coo1 exists in coo2 with same value
        DO k1 = 1, coo1%nnz
            i = coo1%row_ind(k1)
            j = coo1%col_ind(k1)
            val1 = coo1%values(k1)
            
            found = .FALSE.
            DO k2 = 1, coo2%nnz
                IF (coo2%row_ind(k2) == i .AND. coo2%col_ind(k2) == j) THEN
                    val2 = coo2%values(k2)
                    IF (ABS(val1 - val2) < tol) THEN
                        found = .TRUE.
                        EXIT
                    END IF
                END IF
            END DO
            
            IF (.NOT. found) THEN
                are_equal = .FALSE.
                RETURN
            END IF
        END DO
    END FUNCTION compare_coo_matrices
    
    SUBROUTINE delete_file(filename)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        INTEGER :: unit, ios
        
        unit = 99
        OPEN(UNIT=unit, FILE=filename, STATUS='OLD', IOSTAT=ios)
        IF (ios == 0) THEN
            CLOSE(UNIT=unit, STATUS='DELETE')
        END IF
    END SUBROUTINE delete_file

END PROGRAM test_io