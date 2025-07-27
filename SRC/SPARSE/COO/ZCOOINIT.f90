!> \brief \b ZCOOALLOC allocates a COO sparse matrix
!>
!> \par Purpose:
!> =============
!>
!> ZCOOALLOC allocates memory for a COO format sparse matrix with specified
!> dimensions and estimated number of non-zeros.
!>
!> \param[in] NROWS
!>          NROWS is INTEGER
!>          The number of rows of the matrix. NROWS >= 0.
!>
!> \param[in] NCOLS  
!>          NCOLS is INTEGER
!>          The number of columns of the matrix. NCOLS >= 0.
!>
!> \param[in] NNZ
!>          NNZ is INTEGER
!>          The estimated number of non-zero elements. NNZ >= 0.
!>          If NNZ = 0, a default size will be allocated.
!>
!> \param[out] COO
!>          COO is TYPE(sparse_coo_z)
!>          On exit, the allocated COO sparse matrix structure.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_ALLOC: memory allocation failed

SUBROUTINE ZCOOALLOC(NROWS, NCOLS, NNZ, COO, INFO)
    USE sparse_types_extended
    USE sparse_constants
    IMPLICIT NONE
    
    ! Arguments
    INTEGER, INTENT(IN) :: NROWS, NCOLS, NNZ
    TYPE(sparse_coo_z), INTENT(OUT) :: COO
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: alloc_size, ierr
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check arguments
    IF (NROWS < 0) THEN
        INFO = -1
        RETURN
    END IF
    
    IF (NCOLS < 0) THEN
        INFO = -2
        RETURN
    END IF
    
    IF (NNZ < 0) THEN
        INFO = -3
        RETURN
    END IF
    
    ! Set dimensions
    COO%nrows = NROWS
    COO%ncols = NCOLS
    COO%nnz = 0  ! No elements yet
    
    ! Determine allocation size
    IF (NNZ > 0) THEN
        alloc_size = NNZ
    ELSE
        alloc_size = SPARSE_INIT_SIZE
    END IF
    COO%nnz_alloc = alloc_size
    
    ! Allocate arrays
    ALLOCATE(COO%row_ind(alloc_size), &
             COO%col_ind(alloc_size), &
             COO%values(alloc_size), STAT=ierr)
    
    IF (ierr /= 0) THEN
        INFO = SPARSE_ERR_ALLOC
        COO%nnz_alloc = 0
        RETURN
    END IF
    
    ! Initialize flags
    COO%sorted = .FALSE.
    COO%checked = .FALSE.
    
END SUBROUTINE ZCOOALLOC

!> \brief \b ZCOOINIT initializes a COO sparse matrix from arrays
!>
!> \par Purpose:
!> =============
!>
!> ZCOOINIT initializes a COO format sparse matrix from provided arrays
!> of row indices, column indices, and values.
!>
!> \param[in] ROWIND
!>          ROWIND is INTEGER array, dimension (NNZ)
!>          The row indices (1-based) of the non-zero elements.
!>
!> \param[in] COLIND
!>          COLIND is INTEGER array, dimension (NNZ)
!>          The column indices (1-based) of the non-zero elements.
!>
!> \param[in] VAL
!>          VAL is ZOUBLE PRECISION array, dimension (NNZ)
!>          The non-zero values.
!>
!> \param[in] NNZ
!>          NNZ is INTEGER
!>          The number of non-zero elements. NNZ >= 0.
!>
!> \param[in,out] COO
!>          COO is TYPE(sparse_coo_z)
!>          On entry, an allocated COO matrix structure.
!>          On exit, the initialized COO matrix.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_DIM: invalid index found
!>          = SPARSE_ERR_ALLOC: insufficient allocated space

SUBROUTINE ZCOOINIT(ROWIND, COLIND, VAL, NNZ, COO, INFO)
    USE sparse_types_extended
    USE sparse_constants
    IMPLICIT NONE
    
    ! Arguments
    INTEGER, INTENT(IN) :: NNZ
    INTEGER, INTENT(IN) :: ROWIND(NNZ), COLIND(NNZ)
    COMPLEX(real64), INTENT(IN) :: VAL(NNZ)
    TYPE(sparse_coo_z), INTENT(INOUT) :: COO
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check arguments
    IF (NNZ < 0) THEN
        INFO = -4
        RETURN
    END IF
    
    ! Check if COO is allocated
    IF (.NOT. ALLOCATED(COO%row_ind)) THEN
        INFO = -5
        RETURN
    END IF
    
    ! Check allocation size
    IF (NNZ > COO%nnz_alloc) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Validate indices and copy data
    ZO i = 1, NNZ
        ! Check row index
        IF (ROWIND(i) < 1 .OR. ROWIND(i) > COO%nrows) THEN
            INFO = SPARSE_ERR_DIM
            RETURN
        END IF
        
        ! Check column index
        IF (COLIND(i) < 1 .OR. COLIND(i) > COO%ncols) THEN
            INFO = SPARSE_ERR_DIM
            RETURN
        END IF
        
        ! Copy data
        COO%row_ind(i) = ROWIND(i)
        COO%col_ind(i) = COLIND(i)
        COO%values(i) = VAL(i)
    END ZO
    
    ! Set number of non-zeros
    COO%nnz = NNZ
    
    ! Mark as unsorted and unchecked
    COO%sorted = .FALSE.
    COO%checked = .FALSE.
    
END SUBROUTINE ZCOOINIT