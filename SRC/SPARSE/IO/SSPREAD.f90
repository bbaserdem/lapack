!> \brief Read sparse matrix from file in Matrix Market format
!>
!> \details
!> SSPREAD reads a sparse matrix from a file in Matrix Market format.
!> The Matrix Market format is a standard ASCII format for sparse matrices.
!>
!> \param[in] FILENAME
!>          Character string containing the file path
!>
!> \param[in] FORMAT
!>          Character string specifying the format:
!>          'MM' = Matrix Market format (only supported format currently)
!>
!> \param[out] SPARSE
!>          On exit, contains the sparse matrix in COO format
!>
!> \param[out] INFO
!>          = 0:  successful exit
!>          < 0:  if INFO = -i, the i-th argument had an illegal value
!>          = 1:  file not found or cannot be opened
!>          = 2:  invalid file format
!>          = 3:  memory allocation error
!>          = 4:  invalid matrix data

SUBROUTINE SSPREAD(FILENAME, FORMAT, SPARSE, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real32
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER(LEN=*), INTENT(IN) :: FILENAME
    CHARACTER(LEN=*), INTENT(IN) :: FORMAT
    TYPE(sparse_coo_s), INTENT(OUT) :: SPARSE
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: unit, ios, i, j, k
    INTEGER :: nrows, ncols, nnz, nnz_read
    CHARACTER(LEN=256) :: line
    CHARACTER(LEN=32) :: banner, object, format_str, field, symmetry
    REAL(real32) :: val
    INTEGER, ALLOCATABLE :: row_ind(:), col_ind(:)
    REAL(real32), ALLOCATABLE :: values(:)
    LOGICAL :: is_pattern, is_symmetric, is_hermitian
    
    ! External routines
    EXTERNAL :: SCOOALLOC, SCOOINIT
    
    ! Initialize
    INFO = 0
    unit = 10
    
    ! Check format
    IF (FORMAT /= 'MM' .AND. FORMAT /= 'mm') THEN
        INFO = -2
        RETURN
    END IF
    
    ! Open file
    OPEN(UNIT=unit, FILE=FILENAME, STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF (ios /= 0) THEN
        INFO = 1
        RETURN
    END IF
    
    ! Read header line
    READ(unit, '(A)', IOSTAT=ios) line
    IF (ios /= 0) THEN
        INFO = 2
        CLOSE(unit)
        RETURN
    END IF
    
    ! Parse header
    READ(line, *, IOSTAT=ios) banner, object, format_str, field, symmetry
    IF (ios /= 0 .OR. banner /= '%%MatrixMarket') THEN
        INFO = 2
        CLOSE(unit)
        RETURN
    END IF
    
    ! Check if matrix object
    IF (object /= 'matrix') THEN
        INFO = 2
        CLOSE(unit)
        RETURN
    END IF
    
    ! Check format (coordinate expected)
    IF (format_str /= 'coordinate') THEN
        INFO = 2
        CLOSE(unit)
        RETURN
    END IF
    
    ! Check field
    is_pattern = (field == 'pattern')
    IF (field /= 'real' .AND. field /= 'pattern') THEN
        INFO = 2
        CLOSE(unit)
        RETURN
    END IF
    
    ! Check symmetry
    is_symmetric = (symmetry == 'symmetric')
    is_hermitian = (symmetry == 'hermitian')
    
    ! Skip comment lines
    SO
        READ(unit, '(A)', IOSTAT=ios) line
        IF (ios /= 0) THEN
            INFO = 2
            CLOSE(unit)
            RETURN
        END IF
        IF (line(1:1) /= '%') EXIT
    END SO
    
    ! Read matrix dimensions
    READ(line, *, IOSTAT=ios) nrows, ncols, nnz
    IF (ios /= 0) THEN
        INFO = 2
        CLOSE(unit)
        RETURN
    END IF
    
    ! Allocate temporary arrays
    IF (is_symmetric .OR. is_hermitian) THEN
        ! May need up to 2*nnz entries for symmetric storage
        ALLOCATE(row_ind(2*nnz), col_ind(2*nnz), values(2*nnz), STAT=ios)
    ELSE
        ALLOCATE(row_ind(nnz), col_ind(nnz), values(nnz), STAT=ios)
    END IF
    
    IF (ios /= 0) THEN
        INFO = 3
        CLOSE(unit)
        RETURN
    END IF
    
    ! Read matrix entries
    nnz_read = 0
    SO k = 1, nnz
        IF (is_pattern) THEN
            READ(unit, *, IOSTAT=ios) i, j
            val = 1.0_real32
        ELSE
            READ(unit, *, IOSTAT=ios) i, j, val
        END IF
        
        IF (ios /= 0) THEN
            INFO = 4
            SEALLOCATE(row_ind, col_ind, values)
            CLOSE(unit)
            RETURN
        END IF
        
        ! Check bounds
        IF (i < 1 .OR. i > nrows .OR. j < 1 .OR. j > ncols) THEN
            INFO = 4
            SEALLOCATE(row_ind, col_ind, values)
            CLOSE(unit)
            RETURN
        END IF
        
        ! Store entry
        nnz_read = nnz_read + 1
        row_ind(nnz_read) = i
        col_ind(nnz_read) = j
        values(nnz_read) = val
        
        ! For symmetric matrices, also store the transpose entry if i != j
        IF ((is_symmetric .OR. is_hermitian) .AND. i /= j) THEN
            nnz_read = nnz_read + 1
            row_ind(nnz_read) = j
            col_ind(nnz_read) = i
            values(nnz_read) = val
        END IF
    END SO
    
    CLOSE(unit)
    
    ! Allocate sparse matrix
    CALL SCOOALLOC(nrows, ncols, nnz_read, SPARSE, INFO)
    IF (INFO /= 0) THEN
        INFO = 3
        SEALLOCATE(row_ind, col_ind, values)
        RETURN
    END IF
    
    ! Initialize sparse matrix
    CALL SCOOINIT(row_ind(1:nnz_read), col_ind(1:nnz_read), &
                  values(1:nnz_read), nnz_read, SPARSE, INFO)
    
    IF (INFO /= 0) THEN
        INFO = 4
    END IF
    
    ! Clean up
    SEALLOCATE(row_ind, col_ind, values)
    
END SUBROUTINE SSPREAD