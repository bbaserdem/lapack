!> \brief \b DCSCALLOC allocates a CSC sparse matrix
!>
!> \par Purpose:
!> =============
!>
!> DCSCALLOC allocates memory for a CSC format sparse matrix with specified
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
!> \param[out] CSC
!>          CSC is TYPE(sparse_csc_d)
!>          On exit, the allocated CSC sparse matrix structure.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_ALLOC: memory allocation failed

SUBROUTINE DCSCALLOC(NROWS, NCOLS, NNZ, CSC, INFO)
    USE sparse_types
    USE sparse_constants
    IMPLICIT NONE
    
    ! Arguments
    INTEGER, INTENT(IN) :: NROWS, NCOLS, NNZ
    TYPE(sparse_csc_d), INTENT(OUT) :: CSC
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
    CSC%nrows = NROWS
    CSC%ncols = NCOLS
    CSC%nnz = 0  ! No elements yet
    
    ! Determine allocation size for values and row_ind
    IF (NNZ > 0) THEN
        alloc_size = NNZ
    ELSE
        alloc_size = SPARSE_INIT_SIZE
    END IF
    CSC%nnz_alloc = alloc_size
    
    ! Allocate arrays
    ! col_ptr has size ncols+1
    ALLOCATE(CSC%col_ptr(NCOLS+1), STAT=ierr)
    IF (ierr /= 0) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! row_ind and values have size nnz_alloc
    ALLOCATE(CSC%row_ind(alloc_size), &
             CSC%values(alloc_size), STAT=ierr)
    
    IF (ierr /= 0) THEN
        ! Clean up partial allocation
        IF (ALLOCATED(CSC%col_ptr)) DEALLOCATE(CSC%col_ptr)
        INFO = SPARSE_ERR_ALLOC
        CSC%nnz_alloc = 0
        RETURN
    END IF
    
    ! Initialize col_ptr to all 1s (empty columns)
    CSC%col_ptr(:) = 1
    
    ! Initialize flag
    CSC%sorted = .FALSE.
    
END SUBROUTINE DCSCALLOC

!> \brief \b DCSCINIT initializes a CSC sparse matrix from arrays
!>
!> \par Purpose:
!> =============
!>
!> DCSCINIT initializes a CSC format sparse matrix from provided arrays
!> of column pointers, row indices, and values.
!>
!> \param[in] COLPTR
!>          COLPTR is INTEGER array, dimension (NCOLS+1)
!>          The column pointer array. COLPTR(j) points to the start of column j
!>          in the row_ind and values arrays (1-based indexing).
!>          COLPTR(NCOLS+1) = NNZ + 1
!>
!> \param[in] ROWIND
!>          ROWIND is INTEGER array, dimension (NNZ)
!>          The row indices (1-based) of the non-zero elements.
!>
!> \param[in] VAL
!>          VAL is DOUBLE PRECISION array, dimension (NNZ)
!>          The non-zero values.
!>
!> \param[in] NCOLS
!>          NCOLS is INTEGER
!>          The number of columns. NCOLS >= 0.
!>
!> \param[in] NNZ
!>          NNZ is INTEGER
!>          The number of non-zero elements. NNZ >= 0.
!>
!> \param[in,out] CSC
!>          CSC is TYPE(sparse_csc_d)
!>          On entry, an allocated CSC matrix structure.
!>          On exit, the initialized CSC matrix.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_DIM: invalid index found
!>          = SPARSE_ERR_ALLOC: insufficient allocated space
!>          = SPARSE_ERR_INVALID: invalid column pointer structure

SUBROUTINE DCSCINIT(COLPTR, ROWIND, VAL, NCOLS, NNZ, CSC, INFO)
    USE sparse_types
    USE sparse_constants
    IMPLICIT NONE
    
    ! Arguments
    INTEGER, INTENT(IN) :: NCOLS, NNZ
    INTEGER, INTENT(IN) :: COLPTR(NCOLS+1), ROWIND(NNZ)
    REAL(real64), INTENT(IN) :: VAL(NNZ)
    TYPE(sparse_csc_d), INTENT(INOUT) :: CSC
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i, j, col_start, col_end
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check arguments
    IF (NCOLS < 0) THEN
        INFO = -4
        RETURN
    END IF
    
    IF (NNZ < 0) THEN
        INFO = -5
        RETURN
    END IF
    
    ! Check if CSC is allocated
    IF (.NOT. ALLOCATED(CSC%col_ptr)) THEN
        INFO = -6
        RETURN
    END IF
    
    ! Check allocation sizes
    IF (NNZ > CSC%nnz_alloc) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    IF (SIZE(CSC%col_ptr) < NCOLS+1) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Validate column pointer structure
    IF (COLPTR(1) /= 1) THEN
        INFO = SPARSE_ERR_INVALID
        RETURN
    END IF
    
    IF (COLPTR(NCOLS+1) /= NNZ+1) THEN
        INFO = SPARSE_ERR_INVALID
        RETURN
    END IF
    
    ! Check that column pointers are non-decreasing
    DO j = 1, NCOLS
        IF (COLPTR(j+1) < COLPTR(j)) THEN
            INFO = SPARSE_ERR_INVALID
            RETURN
        END IF
    END DO
    
    ! Copy column pointers
    CSC%col_ptr(1:NCOLS+1) = COLPTR(1:NCOLS+1)
    
    ! Validate row indices and copy data
    DO j = 1, NCOLS
        col_start = COLPTR(j)
        col_end = COLPTR(j+1) - 1
        
        DO i = col_start, col_end
            ! Check row index
            IF (ROWIND(i) < 1 .OR. ROWIND(i) > CSC%nrows) THEN
                INFO = SPARSE_ERR_DIM
                RETURN
            END IF
            
            ! Copy data
            CSC%row_ind(i) = ROWIND(i)
            CSC%values(i) = VAL(i)
        END DO
    END DO
    
    ! Set number of columns and non-zeros
    CSC%ncols = NCOLS
    CSC%nnz = NNZ
    
    ! Mark as unsorted (we don't know if rows within columns are sorted)
    CSC%sorted = .FALSE.
    
END SUBROUTINE DCSCINIT