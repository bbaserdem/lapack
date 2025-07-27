!> \brief \b DCSC2COO converts CSC to COO format
!>
!> \par Purpose:
!> =============
!>
!> DCSC2COO converts a sparse matrix from CSC (Compressed Sparse Column) format
!> to COO (Coordinate List) format.
!>
!> \param[in] CSC
!>          CSC is TYPE(sparse_csc_c)
!>          The input CSC sparse matrix.
!>
!> \param[out] COO
!>          COO is TYPE(sparse_coo_c)
!>          On exit, the COO sparse matrix containing the same data as CSC.
!>          Must be pre-allocated with sufficient space.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_ALLOC: insufficient allocated space in COO

SUBROUTINE CCSC2COO(CSC, COO, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real32
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_csc_c), INTENT(IN) :: CSC
    TYPE(sparse_coo_c), INTENT(INOUT) :: COO
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i, j, k, nnz_count
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check if CSC is valid
    IF (.NOT. ALLOCATED(CSC%col_ptr)) THEN
        INFO = -1
        RETURN
    END IF
    
    ! Check if COO is allocated
    IF (.NOT. ALLOCATED(COO%row_ind)) THEN
        INFO = -2
        RETURN
    END IF
    
    ! Check allocation sizes
    IF (CSC%nnz > COO%nnz_alloc) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Set dimensions
    COO%nrows = CSC%nrows
    COO%ncols = CSC%ncols
    COO%nnz = CSC%nnz
    
    ! Special case: empty matrix
    IF (CSC%nnz == 0) THEN
        RETURN
    END IF
    
    ! Convert CSC to COO
    nnz_count = 0
    DO j = 1, CSC%ncols
        ! Process all elements in column j
        DO i = CSC%col_ptr(j), CSC%col_ptr(j+1)-1
            nnz_count = nnz_count + 1
            COO%row_ind(nnz_count) = CSC%row_ind(i)
            COO%col_ind(nnz_count) = j
            COO%values(nnz_count) = CSC%values(i)
        END DO
    END DO
    
    ! Verify we processed all elements
    IF (nnz_count /= CSC%nnz) THEN
        INFO = SPARSE_ERR_NNZ
        RETURN
    END IF
    
    ! Mark as unsorted (CSC is column-ordered, but COO typically expects row-ordering)
    COO%sorted = .FALSE.
    COO%checked = .FALSE.
    
END SUBROUTINE CCSC2COO