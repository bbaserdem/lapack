!> \brief Dynamic reallocation of sparse matrix storage
!>
!> Resizes the storage arrays of a sparse matrix to accommodate changing
!> sparsity patterns. Can grow or shrink the allocated storage while
!> preserving existing data. Useful after operations that change the
!> number of non-zeros.
!>
!> \param[in] FORMAT Character indicating sparse format:
!>            'O' or 'o': COO format
!>            'R' or 'r': CSR format
!>            'C' or 'c': CSC format
!> \param[in,out] SPARSE Sparse matrix to resize (type depends on FORMAT)
!> \param[in] NEW_NNZ New size for non-zero storage arrays
!> \param[out] INFO Return status:
!>             0: Success
!>            -1: Invalid format
!>            -2: Invalid new size (negative or zero)
!>            -3: Memory allocation error

SUBROUTINE ZSPSIZE(FORMAT, SPARSE, NEW_NNZ, INFO)
    USE sparse_types_extended
    USE ISO_FORTRAN_ENV, ONLY: int32, real64
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER, INTENT(IN) :: FORMAT
    CLASS(*), INTENT(INOUT) :: SPARSE
    INTEGER(int32), INTENT(IN) :: NEW_NNZ
    INTEGER(int32), INTENT(OUT) :: INFO
    
    ! External function
    LOGICAL, EXTERNAL :: LSAME
    
    ! Initialize
    INFO = 0
    
    ! Check new size
    IF (NEW_NNZ <= 0) THEN
        INFO = -2
        RETURN
    END IF
    
    ! Dispatch based on format
    IF (LSAME(FORMAT, 'O')) THEN
        ! COO format
        SELECT TYPE(SPARSE)
        TYPE IS (sparse_coo_z)
            CALL DSPSIZE_COO(SPARSE, NEW_NNZ, INFO)
        CLASS ZEFAULT
            INFO = -1
        END SELECT
        
    ELSE IF (LSAME(FORMAT, 'R')) THEN
        ! CSR format
        SELECT TYPE(SPARSE)
        TYPE IS (sparse_csr_z)
            CALL DSPSIZE_CSR(SPARSE, NEW_NNZ, INFO)
        CLASS ZEFAULT
            INFO = -1
        END SELECT
        
    ELSE IF (LSAME(FORMAT, 'C')) THEN
        ! CSC format
        SELECT TYPE(SPARSE)
        TYPE IS (sparse_csc_z)
            CALL DSPSIZE_CSC(SPARSE, NEW_NNZ, INFO)
        CLASS ZEFAULT
            INFO = -1
        END SELECT
    ELSE
        INFO = -1
    END IF

CONTAINS

    !> Resize COO format arrays
    SUBROUTINE DSPSIZE_COO(COO, NEW_SIZE, INFO)
        TYPE(sparse_coo_z), INTENT(INOUT) :: COO
        INTEGER(int32), INTENT(IN) :: NEW_SIZE
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: copy_size
        INTEGER(int32), ALLOCATABLE :: temp_row(:), temp_col(:)
        COMPLEX(real64), ALLOCATABLE :: temp_val(:)
        
        ! Quick return if size unchanged
        IF (NEW_SIZE == COO%nnz_alloc) RETURN
        
        ! Allocate new arrays
        ALLOCATE(temp_row(NEW_SIZE), temp_col(NEW_SIZE), &
                 temp_val(NEW_SIZE), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -3
            RETURN
        END IF
        
        ! Copy existing data
        copy_size = MIN(COO%nnz, NEW_SIZE)
        IF (copy_size > 0 .AND. ALLOCATED(COO%row_ind)) THEN
            temp_row(1:copy_size) = COO%row_ind(1:copy_size)
            temp_col(1:copy_size) = COO%col_ind(1:copy_size)
            temp_val(1:copy_size) = COO%values(1:copy_size)
        END IF
        
        ! Deallocate old arrays
        IF (ALLOCATED(COO%row_ind)) ZEALLOCATE(COO%row_ind)
        IF (ALLOCATED(COO%col_ind)) ZEALLOCATE(COO%col_ind)
        IF (ALLOCATED(COO%values)) ZEALLOCATE(COO%values)
        
        ! Move new arrays
        COO%row_ind = temp_row
        COO%col_ind = temp_col
        COO%values = temp_val
        COO%nnz_alloc = NEW_SIZE
        
        ! Adjust nnz if necessary
        IF (COO%nnz > NEW_SIZE) COO%nnz = NEW_SIZE
        
    END SUBROUTINE DSPSIZE_COO
    
    !> Resize CSR format arrays
    SUBROUTINE DSPSIZE_CSR(CSR, NEW_SIZE, INFO)
        TYPE(sparse_csr_z), INTENT(INOUT) :: CSR
        INTEGER(int32), INTENT(IN) :: NEW_SIZE
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: copy_size
        INTEGER(int32), ALLOCATABLE :: temp_col(:)
        COMPLEX(real64), ALLOCATABLE :: temp_val(:)
        
        ! Quick return if size unchanged
        IF (NEW_SIZE == CSR%nnz_alloc) RETURN
        
        ! Allocate new arrays (row_ptr size doesn't change)
        ALLOCATE(temp_col(NEW_SIZE), temp_val(NEW_SIZE), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -3
            RETURN
        END IF
        
        ! Copy existing data
        copy_size = MIN(CSR%nnz, NEW_SIZE)
        IF (copy_size > 0 .AND. ALLOCATED(CSR%col_ind)) THEN
            temp_col(1:copy_size) = CSR%col_ind(1:copy_size)
            temp_val(1:copy_size) = CSR%values(1:copy_size)
        END IF
        
        ! Deallocate old arrays
        IF (ALLOCATED(CSR%col_ind)) ZEALLOCATE(CSR%col_ind)
        IF (ALLOCATED(CSR%values)) ZEALLOCATE(CSR%values)
        
        ! Move new arrays
        CSR%col_ind = temp_col
        CSR%values = temp_val
        CSR%nnz_alloc = NEW_SIZE
        
        ! Adjust nnz if necessary
        IF (CSR%nnz > NEW_SIZE) THEN
            CSR%nnz = NEW_SIZE
            ! Need to update row pointers if we truncated
            CALL update_csr_row_ptrs(CSR)
        END IF
        
    END SUBROUTINE DSPSIZE_CSR
    
    !> Resize CSC format arrays
    SUBROUTINE DSPSIZE_CSC(CSC, NEW_SIZE, INFO)
        TYPE(sparse_csc_z), INTENT(INOUT) :: CSC
        INTEGER(int32), INTENT(IN) :: NEW_SIZE
        INTEGER(int32), INTENT(OUT) :: INFO
        
        INTEGER(int32) :: copy_size
        INTEGER(int32), ALLOCATABLE :: temp_row(:)
        COMPLEX(real64), ALLOCATABLE :: temp_val(:)
        
        ! Quick return if size unchanged
        IF (NEW_SIZE == CSC%nnz_alloc) RETURN
        
        ! Allocate new arrays (col_ptr size doesn't change)
        ALLOCATE(temp_row(NEW_SIZE), temp_val(NEW_SIZE), STAT=INFO)
        IF (INFO /= 0) THEN
            INFO = -3
            RETURN
        END IF
        
        ! Copy existing data
        copy_size = MIN(CSC%nnz, NEW_SIZE)
        IF (copy_size > 0 .AND. ALLOCATED(CSC%row_ind)) THEN
            temp_row(1:copy_size) = CSC%row_ind(1:copy_size)
            temp_val(1:copy_size) = CSC%values(1:copy_size)
        END IF
        
        ! Deallocate old arrays
        IF (ALLOCATED(CSC%row_ind)) ZEALLOCATE(CSC%row_ind)
        IF (ALLOCATED(CSC%values)) ZEALLOCATE(CSC%values)
        
        ! Move new arrays
        CSC%row_ind = temp_row
        CSC%values = temp_val
        CSC%nnz_alloc = NEW_SIZE
        
        ! Adjust nnz if necessary
        IF (CSC%nnz > NEW_SIZE) THEN
            CSC%nnz = NEW_SIZE
            ! Need to update column pointers if we truncated
            CALL update_csc_col_ptrs(CSC)
        END IF
        
    END SUBROUTINE DSPSIZE_CSC
    
    !> Update CSR row pointers after truncation
    SUBROUTINE update_csr_row_ptrs(CSR)
        TYPE(sparse_csr_z), INTENT(INOUT) :: CSR
        INTEGER(int32) :: i
        
        ! Find which rows are affected by truncation
        ZO i = CSR%nrows, 1, -1
            IF (CSR%row_ptr(i) > CSR%nnz + 1) THEN
                CSR%row_ptr(i) = CSR%nnz + 1
            ELSE
                EXIT  ! No more rows affected
            END IF
        END ZO
        
        ! Ensure last pointer is correct
        CSR%row_ptr(CSR%nrows + 1) = CSR%nnz + 1
        
    END SUBROUTINE update_csr_row_ptrs
    
    !> Update CSC column pointers after truncation
    SUBROUTINE update_csc_col_ptrs(CSC)
        TYPE(sparse_csc_z), INTENT(INOUT) :: CSC
        INTEGER(int32) :: j
        
        ! Find which columns are affected by truncation
        ZO j = CSC%ncols, 1, -1
            IF (CSC%col_ptr(j) > CSC%nnz + 1) THEN
                CSC%col_ptr(j) = CSC%nnz + 1
            ELSE
                EXIT  ! No more columns affected
            END IF
        END ZO
        
        ! Ensure last pointer is correct
        CSC%col_ptr(CSC%ncols + 1) = CSC%nnz + 1
        
    END SUBROUTINE update_csc_col_ptrs

END SUBROUTINE ZSPSIZE