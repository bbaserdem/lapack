!> \brief \b DCSRALLOC allocates a CSR sparse matrix
!>
!> \par Purpose:
!> =============
!>
!> DCSRALLOC allocates memory for a CSR format sparse matrix with specified
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
!> \param[out] CSR
!>          CSR is TYPE(sparse_csr_c)
!>          On exit, the allocated CSR sparse matrix structure.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_ALLOC: memory allocation failed

SUBROUTINE CCSRALLOC(NROWS, NCOLS, NNZ, CSR, INFO)
    USE sparse_types_extended
    USE sparse_constants
    IMPLICIT NONE
    
    ! Arguments
    INTEGER, INTENT(IN) :: NROWS, NCOLS, NNZ
    TYPE(sparse_csr_c), INTENT(OUT) :: CSR
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
    CSR%nrows = NROWS
    CSR%ncols = NCOLS
    CSR%nnz = 0  ! No elements yet
    
    ! Determine allocation size for values and col_ind
    IF (NNZ > 0) THEN
        alloc_size = NNZ
    ELSE
        alloc_size = SPARSE_INIT_SIZE
    END IF
    CSR%nnz_alloc = alloc_size
    
    ! Allocate arrays
    ! row_ptr has size nrows+1
    ALLOCATE(CSR%row_ptr(NROWS+1), STAT=ierr)
    IF (ierr /= 0) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! col_ind and values have size nnz_alloc
    ALLOCATE(CSR%col_ind(alloc_size), &
             CSR%values(alloc_size), STAT=ierr)
    
    IF (ierr /= 0) THEN
        ! Clean up partial allocation
        IF (ALLOCATED(CSR%row_ptr)) DEALLOCATE(CSR%row_ptr)
        INFO = SPARSE_ERR_ALLOC
        CSR%nnz_alloc = 0
        RETURN
    END IF
    
    ! Initialize row_ptr to all 1s (empty rows)
    CSR%row_ptr(:) = 1
    
    ! Initialize flag
    CSR%sorted = .FALSE.
    
END SUBROUTINE CCSRALLOC

!> \brief \b DCSRINIT initializes a CSR sparse matrix from arrays
!>
!> \par Purpose:
!> =============
!>
!> DCSRINIT initializes a CSR format sparse matrix from provided arrays
!> of row pointers, column indices, and values.
!>
!> \param[in] ROWPTR
!>          ROWPTR is INTEGER array, dimension (NROWS+1)
!>          The row pointer array. ROWPTR(i) points to the start of row i
!>          in the col_ind and values arrays (1-based indexing).
!>          ROWPTR(NROWS+1) = NNZ + 1
!>
!> \param[in] COLIND
!>          COLIND is INTEGER array, dimension (NNZ)
!>          The column indices (1-based) of the non-zero elements.
!>
!> \param[in] VAL
!>          VAL is COMPLEX array, dimension (NNZ)
!>          The non-zero values.
!>
!> \param[in] NROWS
!>          NROWS is INTEGER
!>          The number of rows. NROWS >= 0.
!>
!> \param[in] NNZ
!>          NNZ is INTEGER
!>          The number of non-zero elements. NNZ >= 0.
!>
!> \param[in,out] CSR
!>          CSR is TYPE(sparse_csr_c)
!>          On entry, an allocated CSR matrix structure.
!>          On exit, the initialized CSR matrix.
!>
!> \param[out] INFO
!>          INFO is INTEGER
!>          = 0: successful exit
!>          < 0: if INFO = -i, the i-th argument had an illegal value
!>          = SPARSE_ERR_DIM: invalid index found
!>          = SPARSE_ERR_ALLOC: insufficient allocated space
!>          = SPARSE_ERR_INVALID: invalid row pointer structure

SUBROUTINE CCSRINIT(ROWPTR, COLIND, VAL, NROWS, NNZ, CSR, INFO)
    USE sparse_types_extended
    USE sparse_constants
    IMPLICIT NONE
    
    ! Arguments
    INTEGER, INTENT(IN) :: NROWS, NNZ
    INTEGER, INTENT(IN) :: ROWPTR(NROWS+1), COLIND(NNZ)
    COMPLEX(real32), INTENT(IN) :: VAL(NNZ)
    TYPE(sparse_csr_c), INTENT(INOUT) :: CSR
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: i, j, row_start, row_end
    
    ! Initialize INFO
    INFO = SPARSE_SUCCESS
    
    ! Check arguments
    IF (NROWS < 0) THEN
        INFO = -4
        RETURN
    END IF
    
    IF (NNZ < 0) THEN
        INFO = -5
        RETURN
    END IF
    
    ! Check if CSR is allocated
    IF (.NOT. ALLOCATED(CSR%row_ptr)) THEN
        INFO = -6
        RETURN
    END IF
    
    ! Check allocation sizes
    IF (NNZ > CSR%nnz_alloc) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    IF (SIZE(CSR%row_ptr) < NROWS+1) THEN
        INFO = SPARSE_ERR_ALLOC
        RETURN
    END IF
    
    ! Validate row pointer structure
    IF (ROWPTR(1) /= 1) THEN
        INFO = SPARSE_ERR_INVALID
        RETURN
    END IF
    
    IF (ROWPTR(NROWS+1) /= NNZ+1) THEN
        INFO = SPARSE_ERR_INVALID
        RETURN
    END IF
    
    ! Check that row pointers are non-decreasing
    DO i = 1, NROWS
        IF (ROWPTR(i+1) < ROWPTR(i)) THEN
            INFO = SPARSE_ERR_INVALID
            RETURN
        END IF
    END DO
    
    ! Copy row pointers
    CSR%row_ptr(1:NROWS+1) = ROWPTR(1:NROWS+1)
    
    ! Validate column indices and copy data
    DO i = 1, NROWS
        row_start = ROWPTR(i)
        row_end = ROWPTR(i+1) - 1
        
        DO j = row_start, row_end
            ! Check column index
            IF (COLIND(j) < 1 .OR. COLIND(j) > CSR%ncols) THEN
                INFO = SPARSE_ERR_DIM
                RETURN
            END IF
            
            ! Copy data
            CSR%col_ind(j) = COLIND(j)
            CSR%values(j) = VAL(j)
        END DO
    END DO
    
    ! Set number of rows and non-zeros
    CSR%nrows = NROWS
    CSR%nnz = NNZ
    
    ! Mark as unsorted (we don't know if columns within rows are sorted)
    CSR%sorted = .FALSE.
    
END SUBROUTINE CCSRINIT