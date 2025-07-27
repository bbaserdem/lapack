!> Simple test for DCSRCSC to debug segfault

PROGRAM test_dcsrcsc_simple
    USE sparse_types
    USE ISO_FORTRAN_ENV, ONLY: int32, real64, output_unit
    IMPLICIT NONE
    
    TYPE(sparse_csr_d) :: A, C
    TYPE(sparse_csc_d) :: B
    
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
    END INTERFACE
    
    WRITE(output_unit, '(A)') 'Starting simple DCSRCSC test...'
    
    ! Create very simple test matrices
    ! A = [1 0]  (1x2)
    ! B = [1]    (2x1)
    !     [0]
    
    WRITE(output_unit, '(A)') 'Initializing matrix A...'
    A%nrows = 1
    A%ncols = 2
    A%nnz = 1
    A%nnz_alloc = 1
    ALLOCATE(A%row_ptr(2), A%col_ind(1), A%values(1))
    A%row_ptr = [1, 2]
    A%col_ind = [1]
    A%values = [1.0_real64]
    
    WRITE(output_unit, '(A)') 'Initializing matrix B...'
    B%nrows = 2
    B%ncols = 1
    B%nnz = 1
    B%nnz_alloc = 1
    ALLOCATE(B%col_ptr(2), B%row_ind(1), B%values(1))
    B%col_ptr = [1, 2]
    B%row_ind = [1]
    B%values = [1.0_real64]
    
    WRITE(output_unit, '(A)') 'Calling DCSRCSC...'
    
    CALL DCSRCSC('N', 'N', 1, 1, 2, 1.0_real64, A, B, 0.0_real64, C)
    
    WRITE(output_unit, '(A)') 'DCSRCSC completed successfully!'
    WRITE(output_unit, '(A,I0)') 'Result C has nnz = ', C%nnz
    
    ! Clean up
    DEALLOCATE(A%row_ptr, A%col_ind, A%values)
    DEALLOCATE(B%col_ptr, B%row_ind, B%values)
    IF (ALLOCATED(C%row_ptr)) DEALLOCATE(C%row_ptr, C%col_ind, C%values)

END PROGRAM test_dcsrcsc_simple