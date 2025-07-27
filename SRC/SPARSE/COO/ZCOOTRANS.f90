!> \brief \b DCOOTRANS performs in-place transpose of a COO sparse matrix
!>
!> \par Purpose:
!> =============
!>
!> DCOOTRANS transposes a sparse matrix in COO format in-place by swapping
!> row and column indices. This operation is very efficient for COO format
!> as it only requires swapping index arrays.
!>
!> The operation performed is: A := A**T
!>
!> \param[in,out] COO
!>          COO is TYPE(sparse_coo_z)
!>          On entry, the sparse matrix A in COO format.
!>          On exit, the transposed matrix A**T in COO format.
!>          The dimensions are swapped: nrows becomes ncols and vice versa.

SUBROUTINE ZCOOTRANS(COO)
    USE sparse_types_extended
    USE sparse_constants
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_coo_z), INTENT(INOUT) :: COO
    
    ! Local variables
    INTEGER :: k, temp_dim
    INTEGER :: temp_ind
    
    ! Swap dimensions
    temp_dim = COO%nrows
    COO%nrows = COO%ncols
    COO%ncols = temp_dim
    
    ! Swap row and column indices for each non-zero element
    DO k = 1, COO%nnz
        temp_ind = COO%row_ind(k)
        COO%row_ind(k) = COO%col_ind(k)
        COO%col_ind(k) = temp_ind
    END DO
    
    ! Mark as unsorted after transpose
    COO%sorted = .FALSE.
    
    RETURN
END SUBROUTINE ZCOOTRANS