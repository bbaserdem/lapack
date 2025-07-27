!> \brief Debug program for CSR conversion and SpMV

PROGRAM debug_csr
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: output_unit
    IMPLICIT NONE
    
    ! Sparse matrices
    TYPE(sparse_coo_d) :: coo
    TYPE(sparse_csr_d) :: csr
    
    ! Vectors
    REAL(real64), ALLOCATABLE :: x(:), y(:)
    
    ! Local variables
    INTEGER :: info, i, j
    REAL(real64) :: alpha, beta
    
    ! External subroutines
    EXTERNAL :: DCOOALLOC, DCOOINIT, DCSRALLOC, DCOO2CSR, DCSRMV
    
    ! Simple 3x3 matrix test
    INTEGER, PARAMETER :: M = 3, N = 3, NNZ = 5
    INTEGER :: row_ind(5), col_ind(5)
    REAL(real64) :: values(5)
    
    WRITE(output_unit, '(A)') 'Debug CSR Test'
    WRITE(output_unit, '(A)') '=============='
    
    ! Allocate vectors
    ALLOCATE(x(N), y(M))
    
    ! Initialize matrix data
    row_ind = [1, 1, 2, 3, 3]
    col_ind = [1, 3, 2, 1, 3]
    values = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
    
    ! Allocate and initialize COO matrix
    CALL DCOOALLOC(M, N, NNZ, coo, info)
    CALL DCOOINIT(row_ind, col_ind, values, NNZ, coo, info)
    
    WRITE(output_unit, '(/A)') 'COO Matrix:'
    DO i = 1, coo%nnz
        WRITE(output_unit, '(A,I0,A,I0,A,F6.2)') '  (', coo%row_ind(i), ',', &
              coo%col_ind(i), ') = ', coo%values(i)
    END DO
    
    ! Allocate CSR matrix
    CALL DCSRALLOC(M, N, NNZ, csr, info)
    IF (info /= 0) THEN
        WRITE(output_unit, '(A,I0)') 'ERROR: DCSRALLOC failed with INFO = ', info
        STOP 1
    END IF
    
    ! Convert to CSR
    CALL DCOO2CSR(coo, csr, info)
    IF (info /= 0) THEN
        WRITE(output_unit, '(A,I0)') 'ERROR: DCOO2CSR failed with INFO = ', info
        STOP 1
    END IF
    
    WRITE(output_unit, '(/A)') 'CSR Matrix:'
    WRITE(output_unit, '(A,I0)') 'nrows = ', csr%nrows
    WRITE(output_unit, '(A,I0)') 'ncols = ', csr%ncols
    WRITE(output_unit, '(A,I0)') 'nnz = ', csr%nnz
    WRITE(output_unit, '(A)', advance='no') 'row_ptr = '
    DO i = 1, csr%nrows + 1
        WRITE(output_unit, '(I3)', advance='no') csr%row_ptr(i)
    END DO
    WRITE(output_unit, *)
    
    WRITE(output_unit, '(A)') 'CSR elements:'
    DO i = 1, csr%nrows
        WRITE(output_unit, '(A,I0,A)', advance='no') 'Row ', i, ': '
        DO j = csr%row_ptr(i), csr%row_ptr(i+1) - 1
            WRITE(output_unit, '(A,I0,A,F6.2,A)', advance='no') &
                  '(', csr%col_ind(j), ',', csr%values(j), ') '
        END DO
        WRITE(output_unit, *)
    END DO
    
    ! Test SpMV
    x = [1.0_real64, 2.0_real64, 3.0_real64]
    y = 0.0_real64
    alpha = 1.0_real64
    beta = 0.0_real64
    
    WRITE(output_unit, '(/A)') 'Testing CSR SpMV: y = A*x'
    WRITE(output_unit, '(A,3F6.2)') 'x = ', x
    
    CALL DCSRMV('N', M, N, alpha, csr, x, 1, beta, y, 1)
    
    WRITE(output_unit, '(A,3F6.2)') 'y = ', y
    WRITE(output_unit, '(A,3F6.2)') 'Expected: ', 7.0_real64, 6.0_real64, 19.0_real64
    
    ! Cleanup
    DEALLOCATE(x, y)
    
END PROGRAM debug_csr