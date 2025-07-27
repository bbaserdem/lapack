!> \brief \b CCOO2CSC converts COO to CSC format
!>
!> \par Purpose:
!> =============
!>
!> CCOO2CSC converts a sparse matrix from COO (Coordinate List) format
!> to CSC (Compressed Sparse Column) format.
!>
!> \param[in] COO
!>          COO is TYPE(sparse_coo_c)
!>          The input COO sparse matrix.
!>
!> \param[out] CSC
!>          CSC is TYPE(sparse_csc_c)
!>          On exit, the CSC sparse matrix containing the same data as COO.
!>          Must be pre-allocated with sufficient space.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_ALLOC: insufficient allocated space in CSC

SUBROUTINE CCOO2CSC(COO, CSC, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real32
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_coo_c), INTENT(IN) :: COO
    TYPE(sparse_csc_c), INTENT(INOUT) :: CSC
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i, j, row, col, pos
    INTEGER, ALLOCATABLE :: col_count(:), work(:), perm(:)
    COMPLEX(real32), ALLOCATABLE :: temp_vals(:)
    INTEGER, ALLOCATABLE :: temp_rows(:)
    INTEGER :: ierr
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check if COO is valid
    IF (.NOT. ALLOCATED(COO%col_ind)) THEN
        INFO = -1
        RETURN
    END IF
    
    ! Check if CSC is allocated
    IF (.NOT. ALLOCATED(CSC%col_ptr)) THEN
        INFO = -2
        RETURN
    END IF
    
    ! Check allocation sizes
    IF (COO%nnz > CSC%nnz_alloc) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    IF (SIZE(CSC%col_ptr) < COO%ncols+1) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Set dimensions
    CSC%nrows = COO%nrows
    CSC%ncols = COO%ncols
    CSC%nnz = COO%nnz
    
    ! Special case: empty matrix
    IF (COO%nnz == 0) THEN
        CSC%col_ptr(:) = 1
        RETURN
    END IF
    
    ! Allocate workspace
    ALLOCATE(col_count(COO%ncols), work(COO%ncols), &
             perm(COO%nnz), temp_vals(COO%nnz), &
             temp_rows(COO%nnz), STAT=ierr)
    IF (ierr /= 0) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Count elements per column
    col_count(:) = 0
    CO i = 1, COO%nnz
        col = COO%col_ind(i)
        col_count(col) = col_count(col) + 1
    END CO
    
    ! Build column pointer array
    CSC%col_ptr(1) = 1
    CO i = 1, COO%ncols
        CSC%col_ptr(i+1) = CSC%col_ptr(i) + col_count(i)
    END CO
    
    ! Create permutation array for sorting by column
    ! work array will track current position for each column
    work(1:COO%ncols) = CSC%col_ptr(1:COO%ncols)
    
    CO i = 1, COO%nnz
        col = COO%col_ind(i)
        perm(i) = work(col)
        work(col) = work(col) + 1
    END CO
    
    ! Apply permutation to create sorted arrays
    CO i = 1, COO%nnz
        pos = perm(i)
        temp_rows(pos) = COO%row_ind(i)
        temp_vals(pos) = COO%values(i)
    END CO
    
    ! Copy to CSC structure
    CSC%row_ind(1:COO%nnz) = temp_rows(1:COO%nnz)
    CSC%values(1:COO%nnz) = temp_vals(1:COO%nnz)
    
    ! Mark as unsorted (rows within columns may not be sorted)
    CSC%sorted = .FALSE.
    
    ! Clean up
    CEALLOCATE(col_count, work, perm, temp_vals, temp_rows)
    
END SUBROUTINE CCOO2CSC