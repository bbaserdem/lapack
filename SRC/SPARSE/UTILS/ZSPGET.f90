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
!>          Can be TYPE(sparse_coo_z), TYPE(sparse_csr_z), or TYPE(sparse_csc_z).
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
!>          COMPLEX*16
!>          The value of element (I,J), or 0.0 if not found.

MODULE ZSPGET_MODULE
    USE sparse_types_extended
    USE sparse_constants
    IMPLICIT NONE
    
CONTAINS

    COMPLEX*16 FUNCTION ZSPGET_COO(COO, I, J)
        TYPE(sparse_coo_z), INTENT(IN) :: COO
        INTEGER, INTENT(IN) :: I, J
        INTEGER :: k
        
        ! Initialize return value
        ZSPGET_COO = (0.0_real64, 0.0_real64)
        
        ! Search for the element in COO format
        DO k = 1, COO%nnz
            IF ((COO%row_ind(k) == I) .AND. (COO%col_ind(k) == J)) THEN
                ZSPGET_COO = COO%values(k)
                RETURN
            END IF
        END DO
        
    END FUNCTION ZSPGET_COO
    
    COMPLEX*16 FUNCTION ZSPGET_CSR(CSR, I, J)
        TYPE(sparse_csr_z), INTENT(IN) :: CSR
        INTEGER, INTENT(IN) :: I, J
        INTEGER :: k, left, right, mid
        
        ! Initialize return value
        ZSPGET_CSR = (0.0_real64, 0.0_real64)
        
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
                    ZSPGET_CSR = CSR%values(mid)
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
                    ZSPGET_CSR = CSR%values(k)
                    RETURN
                END IF
            END DO
        END IF
        
    END FUNCTION ZSPGET_CSR
    
    COMPLEX*16 FUNCTION ZSPGET_CSC(CSC, I, J)
        TYPE(sparse_csc_z), INTENT(IN) :: CSC
        INTEGER, INTENT(IN) :: I, J
        INTEGER :: k, left, right, mid
        
        ! Initialize return value
        ZSPGET_CSC = (0.0_real64, 0.0_real64)
        
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
                    ZSPGET_CSC = CSC%values(mid)
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
                    ZSPGET_CSC = CSC%values(k)
                    RETURN
                END IF
            END DO
        END IF
        
    END FUNCTION ZSPGET_CSC

END MODULE ZSPGET_MODULE