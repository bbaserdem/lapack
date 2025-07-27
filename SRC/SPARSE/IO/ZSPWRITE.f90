!> \brief Write sparse matrix to file in Matrix Market format
!>
!> \details
!> DSPWRITE writes a sparse matrix to a file in Matrix Market format.
!> The Matrix Market format is a standard ASCII format for sparse matrices.
!>
!> \param[in] SPARSE
!>          The sparse matrix in COO format to write
!>
!> \param[in] FILENAME
!>          Character string containing the output file path
!>
!> \param[in] FORMAT
!>          Character string specifying the format:
!>          'MM' = Matrix Market format (only supported format currently)
!>
!> \param[out] INFO
!>          = 0:  successful exit
!>          < 0:  if INFO = -i, the i-th argument had an illegal value
!>          = 1:  file cannot be created or opened
!>          = 2:  invalid format specified
!>          = 3:  I/O error during write

SUBROUTINE ZSPWRITE(SPARSE, FILENAME, FORMAT, INFO)
    USE sparse_types_extended
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64
    IMPLICIT NONE
    
    ! Arguments
    TYPE(sparse_coo_z), INTENT(IN) :: SPARSE
    CHARACTER(LEN=*), INTENT(IN) :: FILENAME
    CHARACTER(LEN=*), INTENT(IN) :: FORMAT
    INTEGER, INTENT(OUT) :: INFO
    
    ! Local variables
    INTEGER :: unit, ios, k, nnz_to_write
    CHARACTER(LEN=32) :: symmetry
    LOGICAL :: is_symmetric
    
    ! Initialize
    INFO = 0
    unit = 11
    
    ! Check format
    IF (FORMAT /= 'MM' .AND. FORMAT /= 'mm') THEN
        INFO = -3
        RETURN
    END IF
    
    ! Check if matrix is symmetric
    is_symmetric = check_symmetry(SPARSE)
    IF (is_symmetric) THEN
        symmetry = 'symmetric'
        ! Count entries in lower triangular part
        nnz_to_write = 0
        DO k = 1, SPARSE%nnz
            IF (SPARSE%row_ind(k) >= SPARSE%col_ind(k)) THEN
                nnz_to_write = nnz_to_write + 1
            END IF
        END DO
    ELSE
        symmetry = 'general'
        nnz_to_write = SPARSE%nnz
    END IF
    
    ! Open file for writing
    OPEN(UNIT=unit, FILE=FILENAME, STATUS='REPLACE', ACTION='WRITE', IOSTAT=ios)
    IF (ios /= 0) THEN
        INFO = 1
        RETURN
    END IF
    
    ! Write header
    WRITE(unit, '(A)', IOSTAT=ios) '%%MatrixMarket matrix coordinate real ' // TRIM(symmetry)
    IF (ios /= 0) THEN
        INFO = 3
        CLOSE(unit)
        RETURN
    END IF
    
    ! Write comments
    WRITE(unit, '(A)', IOSTAT=ios) '% Written by LAPACK DSPWRITE routine'
    IF (ios /= 0) THEN
        INFO = 3
        CLOSE(unit)
        RETURN
    END IF
    
    ! Write dimensions
    WRITE(unit, '(3I12)', IOSTAT=ios) SPARSE%nrows, SPARSE%ncols, nnz_to_write
    IF (ios /= 0) THEN
        INFO = 3
        CLOSE(unit)
        RETURN
    END IF
    
    ! Write matrix entries
    IF (is_symmetric) THEN
        ! For symmetric matrices, only write lower triangular part
        DO k = 1, SPARSE%nnz
            IF (SPARSE%row_ind(k) >= SPARSE%col_ind(k)) THEN
                WRITE(unit, '(2I12,E23.15)', IOSTAT=ios) &
                    SPARSE%row_ind(k), SPARSE%col_ind(k), SPARSE%values(k)
                IF (ios /= 0) THEN
                    INFO = 3
                    CLOSE(unit)
                    RETURN
                END IF
            END IF
        END DO
    ELSE
        ! Write all entries
        DO k = 1, SPARSE%nnz
            WRITE(unit, '(2I12,E23.15)', IOSTAT=ios) &
                SPARSE%row_ind(k), SPARSE%col_ind(k), SPARSE%values(k)
            IF (ios /= 0) THEN
                INFO = 3
                CLOSE(unit)
                RETURN
            END IF
        END DO
    END IF
    
    ! Close file
    CLOSE(unit)
    
CONTAINS

    LOGICAL FUNCTION check_symmetry(COO) RESULT(is_sym)
        TYPE(sparse_coo_z), INTENT(IN) :: COO
        INTEGER :: i, j, k, l
        COMPLEX(real64) :: val1, val2, tol
        LOGICAL :: found
        
        is_sym = .TRUE.
        tol = 1.0E-14_real64
        
        ! Check if matrix is square
        IF (COO%nrows /= COO%ncols) THEN
            is_sym = .FALSE.
            RETURN
        END IF
        
        ! Check each entry has symmetric counterpart
        DO k = 1, COO%nnz
            i = COO%row_ind(k)
            j = COO%col_ind(k)
            val1 = COO%values(k)
            
            IF (i /= j) THEN  ! Off-diagonal element
                found = .FALSE.
                ! Look for (j,i) element
                DO l = 1, COO%nnz
                    IF (COO%row_ind(l) == j .AND. COO%col_ind(l) == i) THEN
                        val2 = COO%values(l)
                        IF (ZABS(val1 - val2) < tol) THEN
                            found = .TRUE.
                            EXIT
                        END IF
                    END IF
                END DO
                
                IF (.NOT. found) THEN
                    is_sym = .FALSE.
                    RETURN
                END IF
            END IF
        END DO
    END FUNCTION check_symmetry

END SUBROUTINE ZSPWRITE