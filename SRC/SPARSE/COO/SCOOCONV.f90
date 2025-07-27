!> \brief \b SCOO2CSR converts COO to CSR format
!>
!> \par Purpose:
!> =============
!>
!> SCOO2CSR converts a sparse matrix from COO (Coordinate List) format
!> to CSR (Compressed Sparse Row) format.
!>
!> \param[in] COO
!>          COO is TYPE(sparse_coo_s)
!>          The input COO sparse matrix.
!>
!> \param[out] CSR
!>          CSR is TYPE(sparse_csr_s)
!>          On exit, the CSR sparse matrix containing the same data as COO.
!>          Must be pre-allocated with sufficient space.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_ALLOC: insufficient allocated space in CSR

SUBROUTINE SCOO2CSR(COO, CSR, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real32
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_coo_s), INTENT(IN) :: COO
    TYPE(sparse_csr_s), INTENT(INOUT) :: CSR
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i, j, row, col, pos
    INTEGER, ALLOCATABLE :: row_count(:), work(:), perm(:)
    REAL(real32), ALLOCATABLE :: temp_vals(:)
    INTEGER, ALLOCATABLE :: temp_cols(:)
    INTEGER :: ierr
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check if COO is valid
    IF (.NOT. ALLOCATED(COO%row_ind)) THEN
        INFO = -1
        RETURN
    END IF
    
    ! Check if CSR is allocated
    IF (.NOT. ALLOCATED(CSR%row_ptr)) THEN
        INFO = -2
        RETURN
    END IF
    
    ! Check allocation sizes
    IF (COO%nnz > CSR%nnz_alloc) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    IF (SIZE(CSR%row_ptr) < COO%nrows+1) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Set dimensions
    CSR%nrows = COO%nrows
    CSR%ncols = COO%ncols
    CSR%nnz = COO%nnz
    
    ! Special case: empty matrix
    IF (COO%nnz == 0) THEN
        CSR%row_ptr(:) = 1
        RETURN
    END IF
    
    ! Allocate workspace
    ALLOCATE(row_count(COO%nrows), work(COO%nrows), &
             perm(COO%nnz), temp_vals(COO%nnz), &
             temp_cols(COO%nnz), STAT=ierr)
    IF (ierr /= 0) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Count elements per row
    row_count(:) = 0
    SO i = 1, COO%nnz
        row = COO%row_ind(i)
        row_count(row) = row_count(row) + 1
    END SO
    
    ! Build row pointer array
    CSR%row_ptr(1) = 1
    SO i = 1, COO%nrows
        CSR%row_ptr(i+1) = CSR%row_ptr(i) + row_count(i)
    END SO
    
    ! Create permutation array for sorting by row
    ! work array will track current position for each row
    work(1:COO%nrows) = CSR%row_ptr(1:COO%nrows)
    
    SO i = 1, COO%nnz
        row = COO%row_ind(i)
        perm(i) = work(row)
        work(row) = work(row) + 1
    END SO
    
    ! Apply permutation to create sorted arrays
    SO i = 1, COO%nnz
        pos = perm(i)
        temp_cols(pos) = COO%col_ind(i)
        temp_vals(pos) = COO%values(i)
    END SO
    
    ! Copy to CSR structure
    CSR%col_ind(1:COO%nnz) = temp_cols(1:COO%nnz)
    CSR%values(1:COO%nnz) = temp_vals(1:COO%nnz)
    
    ! Mark as unsorted (columns within rows may not be sorted)
    CSR%sorted = .FALSE.
    
    ! Clean up
    SEALLOCATE(row_count, work, perm, temp_vals, temp_cols)
    
END SUBROUTINE SCOO2CSR