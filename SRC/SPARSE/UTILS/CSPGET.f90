!> \brief \b DSPGET retrieves an element from a sparse matrix
!>
!> \par Purpose:
!> =============
!>
!> DSPGET returns the value of element (I,J) from a sparse matrix.
!> The function performs a search to find the element. If the element
!> is not found (i.e., it is zero), the function returns 0.0.
!>
!> The function handles different sparse formats:
!> - COO: Linear search through coordinate list
!> - CSR: Binary search within row I
!> - CSC: Binary search within column J
!>
!> \param[in] SPARSE
!>          SPARSE is a generic sparse matrix type
!>          The sparse matrix from which to retrieve the element.
!>          Can be TYPE(sparse_coo_c), TYPE(sparse_csr_c), or TYPE(sparse_csc_c).
!>
!> \param[in] I
!>          I is INTEGER
!>          The row index of the element (1-based).
!>
!> \param[in] J
!>          J is INTEGER
!>          The column index of the element (1-based).
!>
!> \param[in] FORMAT
!>          FORMAT is CHARACTER*3
!>          The format of the sparse matrix: 'COO', 'CSR', or 'CSC'.
!>
!> \result DSPGET
!>          COMPLEX
!>          The value of element (I,J), or 0.0 if not found.

MODULE CSPGET_MODULE
    USE ISO_FORTRAN_ENV, ONLY: real32
    USE sparse_types_extended
    USE sparse_constants
    IMPLICIT NONE
    
CONTAINS

    COMPLEX FUNCTION CSPGET_COO(COO, I, J)
        TYPE(sparse_coo_c), INTENT(IN) :: COO
        INTEGER, INTENT(IN) :: I, J
        INTEGER :: k
        
        ! Initialize return value
        CSPGET_COO = (0.0_real32, 0.0_real32)
        
        ! Search for the element in COO format
        DO k = 1, COO%nnz
            IF ((COO%row_ind(k) == I) .AND. (COO%col_ind(k) == J)) THEN
                CSPGET_COO = COO%values(k)
                RETURN
            END IF
        END DO
        
    END FUNCTION CSPGET_COO
    
    COMPLEX FUNCTION CSPGET_CSR(CSR, I, J)
        TYPE(sparse_csr_c), INTENT(IN) :: CSR
        INTEGER, INTENT(IN) :: I, J
        INTEGER :: k, left, right, mid
        
        ! Initialize return value
        CSPGET_CSR = (0.0_real32, 0.0_real32)
        
        ! Check bounds
        IF ((I < 1) .OR. (I > CSR%nrows) .OR. (J < 1) .OR. (J > CSR%ncols)) RETURN
        
        ! Get the range for row I
        left = CSR%row_ptr(I)
        right = CSR%row_ptr(I+1) - 1
        
        ! If row is empty
        IF (left > right) RETURN
        
        ! If columns are sorted, use binary search
        IF (CSR%sorted) THEN
            DO WHILE (left <= right)
                mid = (left + right) / 2
                IF (CSR%col_ind(mid) == J) THEN
                    CSPGET_CSR = CSR%values(mid)
                    RETURN
                ELSE IF (CSR%col_ind(mid) < J) THEN
                    left = mid + 1
                ELSE
                    right = mid - 1
                END IF
            END DO
        ELSE
            ! Linear search for unsorted columns
            DO k = left, right
                IF (CSR%col_ind(k) == J) THEN
                    CSPGET_CSR = CSR%values(k)
                    RETURN
                END IF
            END DO
        END IF
        
    END FUNCTION CSPGET_CSR
    
    COMPLEX FUNCTION CSPGET_CSC(CSC, I, J)
        TYPE(sparse_csc_c), INTENT(IN) :: CSC
        INTEGER, INTENT(IN) :: I, J
        INTEGER :: k, left, right, mid
        
        ! Initialize return value
        CSPGET_CSC = (0.0_real32, 0.0_real32)
        
        ! Check bounds
        IF ((I < 1) .OR. (I > CSC%nrows) .OR. (J < 1) .OR. (J > CSC%ncols)) RETURN
        
        ! Get the range for column J
        left = CSC%col_ptr(J)
        right = CSC%col_ptr(J+1) - 1
        
        ! If column is empty
        IF (left > right) RETURN
        
        ! If rows are sorted, use binary search
        IF (CSC%sorted) THEN
            DO WHILE (left <= right)
                mid = (left + right) / 2
                IF (CSC%row_ind(mid) == I) THEN
                    CSPGET_CSC = CSC%values(mid)
                    RETURN
                ELSE IF (CSC%row_ind(mid) < I) THEN
                    left = mid + 1
                ELSE
                    right = mid - 1
                END IF
            END DO
        ELSE
            ! Linear search for unsorted rows
            DO k = left, right
                IF (CSC%row_ind(k) == I) THEN
                    CSPGET_CSC = CSC%values(k)
                    RETURN
                END IF
            END DO
        END IF
        
    END FUNCTION CSPGET_CSC

END MODULE CSPGET_MODULE