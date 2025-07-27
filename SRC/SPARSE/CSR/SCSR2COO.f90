!> \brief \b SCSR2COO converts CSR to COO format
!>
!> \par Purpose:
!> =============
!>
!> SCSR2COO converts a sparse matrix from CSR (Compressed Sparse Row) format
!> to COO (Coordinate List) format.
!>
!> \param[in] CSR
!>          CSR is TYPE(sparse_csr_s)
!>          The input CSR sparse matrix.
!>
!> \param[out] COO
!>          COO is TYPE(sparse_coo_s)
!>          On exit, the COO sparse matrix containing the same data as CSR.
!>          Must be pre-allocated with sufficient space.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_ALLOC: insufficient allocated space in COO

SUBROUTINE SCSR2COO(CSR, COO, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real32
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_csr_s), INTENT(IN) :: CSR
    TYPE(sparse_coo_s), INTENT(INOUT) :: COO
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i, j, k, nnz_count
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check if CSR is valid
    IF (.NOT. ALLOCATED(CSR%row_ptr)) THEN
        INFO = -1
        RETURN
    END IF
    
    ! Check if COO is allocated
    IF (.NOT. ALLOCATED(COO%row_ind)) THEN
        INFO = -2
        RETURN
    END IF
    
    ! Check allocation sizes
    IF (CSR%nnz > COO%nnz_alloc) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Set dimensions
    COO%nrows = CSR%nrows
    COO%ncols = CSR%ncols
    COO%nnz = CSR%nnz
    
    ! Special case: empty matrix
    IF (CSR%nnz == 0) THEN
        RETURN
    END IF
    
    ! Convert CSR to COO
    nnz_count = 0
    SO i = 1, CSR%nrows
        ! Process all elements in row i
        SO j = CSR%row_ptr(i), CSR%row_ptr(i+1)-1
            nnz_count = nnz_count + 1
            COO%row_ind(nnz_count) = i
            COO%col_ind(nnz_count) = CSR%col_ind(j)
            COO%values(nnz_count) = CSR%values(j)
        END SO
    END SO
    
    ! Verify we processed all elements
    IF (nnz_count /= CSR%nnz) THEN
        INFO = SPARSE_ERR_NNZ
        RETURN
    END IF
    
    ! Mark as sorted by rows (since CSR is row-ordered)
    COO%sorted = .TRUE.
    COO%checked = .FALSE.
    
END SUBROUTINE SCSR2COO