!> \brief Debug program for sparse matrix-vector multiplication

PROGRAM debug_spmv
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: output_unit
    IMPLICIT NONE
    
    ! Sparse matrices
    TYPE(sparse_coo_d) :: coo
    
    ! Vectors
    REAL(real64), ALLOCATABLE :: x(:), y(:), y_ref(:)
    
    ! Local variables
    INTEGER :: info, i
    REAL(real64) :: alpha, beta
    
    ! External subroutines
    EXTERNAL :: DCOOALLOC, DCOOINIT, DCOOMV
    
    ! Simple 3x3 matrix test
    ! [ 1  0  2 ]
    ! [ 0  3  0 ]  
    ! [ 4  0  5 ]
    
    INTEGER, PARAMETER :: M = 3, N = 3, NNZ = 5
    INTEGER :: row_ind(5), col_ind(5)
    REAL(real64) :: values(5)
    
    WRITE(output_unit, '(A)') 'Debug SpMV Test'
    WRITE(output_unit, '(A)') '==============='
    
    ! Allocate vectors
    ALLOCATE(x(N), y(M), y_ref(M))
    
    ! Initialize matrix data
    row_ind = [1, 1, 2, 3, 3]
    col_ind = [1, 3, 2, 1, 3]
    values = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
    
    ! Allocate and initialize COO matrix
    CALL DCOOALLOC(M, N, NNZ, coo, info)
    IF (info /= 0) THEN
        WRITE(output_unit, '(A,I0)') 'ERROR: DCOOALLOC failed with INFO = ', info
        STOP 1
    END IF
    
    CALL DCOOINIT(row_ind, col_ind, values, NNZ, coo, info)
    IF (info /= 0) THEN
        WRITE(output_unit, '(A,I0)') 'ERROR: DCOOINIT failed with INFO = ', info
        STOP 1
    END IF
    
    WRITE(output_unit, '(A,I0)') 'Matrix dimensions: ', coo%nrows, ' x ', coo%ncols
    WRITE(output_unit, '(A,I0)') 'Number of non-zeros: ', coo%nnz
    WRITE(output_unit, '(A)') 'Matrix elements:'
    DO i = 1, coo%nnz
        WRITE(output_unit, '(A,I0,A,I0,A,F6.2)') '  A(', coo%row_ind(i), ',', &
              coo%col_ind(i), ') = ', coo%values(i)
    END DO
    
    ! Test y = 2*A*x + y with x = [1, 2, 3], y = [1, 1, 1]
    alpha = 2.0_real64
    beta = 1.0_real64
    x = [1.0_real64, 2.0_real64, 3.0_real64]
    y = [1.0_real64, 1.0_real64, 1.0_real64]
    
    WRITE(output_unit, '(/A)') 'Test: y = 2*A*x + y'
    WRITE(output_unit, '(A,3F6.2)') 'x = ', x
    WRITE(output_unit, '(A,3F6.2)') 'y (before) = ', y
    
    ! Manual calculation:
    ! A*x = [1*1 + 0*2 + 2*3, 0*1 + 3*2 + 0*3, 4*1 + 0*2 + 5*3] = [7, 6, 19]
    ! y = 2*[7, 6, 19] + [1, 1, 1] = [15, 13, 39]
    y_ref = [15.0_real64, 13.0_real64, 39.0_real64]
    
    CALL DCOOMV('N', M, N, alpha, coo, x, 1, beta, y, 1)
    
    WRITE(output_unit, '(A,3F6.2)') 'y (after) = ', y
    WRITE(output_unit, '(A,3F6.2)') 'y (expected) = ', y_ref
    WRITE(output_unit, '(A,3F6.2)') 'Error = ', y - y_ref
    
    ! Cleanup
    DEALLOCATE(x, y, y_ref)
    
END PROGRAM debug_spmv