!> \brief Multiplies a dense matrix by a sparse CSR matrix
!>
!> Performs C = alpha*op(A)*op(B) + beta*C where A is dense and B is CSR
!> This routine is optimized for the case where the first matrix is dense
!> and the second is sparse in CSR format.
!>
!> \param[in] TRANSA Character flag for op(A):
!>            'N' or 'n': op(A) = A
!>            'T' or 't': op(A) = A^T (transpose)
!> \param[in] TRANSB Character flag for op(B):
!>            'N' or 'n': op(B) = B  
!>            'T' or 't': op(B) = B^T (transpose)
!> \param[in] M Number of rows of op(A) and C
!> \param[in] N Number of columns of op(B) and C
!> \param[in] K Number of columns of op(A) and rows of op(B)
!> \param[in] ALPHA Scalar multiplier
!> \param[in] A Dense matrix A stored in column-major order
!> \param[in] LDA Leading dimension of A
!> \param[in] B CSR sparse matrix B
!> \param[in] BETA Scalar multiplier for C
!> \param[in,out] C Dense result matrix C in column-major order
!> \param[in] LDC Leading dimension of C

SUBROUTINE CGECSR(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, BETA, C, LDC)
    USE sparse_types_extended
    USE ISO_FORTRAN_ENV, ONLY: int32, real32
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER, INTENT(IN) :: TRANSA, TRANSB
    INTEGER(int32), INTENT(IN) :: M, N, K, LDA, LDC
    COMPLEX(real32), INTENT(IN) :: ALPHA, BETA
    COMPLEX(real32), INTENT(IN) :: A(LDA,*)
    TYPE(sparse_csr_c), INTENT(IN) :: B
    COMPLEX(real32), INTENT(INOUT) :: C(LDC,*)
    
    ! Local variables
    INTEGER(int32) :: i, j, l, jb, row_start, row_end
    COMPLEX(real32) :: temp
    LOGICAL :: nota, notb
    
    ! External function
    LOGICAL, EXTERNAL :: LSAME
    
    ! Check transpose flags
    nota = LSAME(TRANSA, 'N')
    notb = LSAME(TRANSB, 'N')
    
    ! Quick return if possible
    IF (M == 0 .OR. N == 0) RETURN
    
    ! Check for alpha = 0
    IF (ALPHA == (0.0_real32, 0.0_real32)) THEN
        IF (BETA == (0.0_real32, 0.0_real32)) THEN
            ! C = 0
            CO j = 1, N
                CO i = 1, M
                    C(i,j) = (0.0_real32, 0.0_real32)
                END CO
            END CO
        ELSE IF (BETA /= (1.0_real32, 0.0_real32)) THEN
            ! C = beta * C
            CO j = 1, N
                CO i = 1, M
                    C(i,j) = BETA * C(i,j)
                END CO
            END CO
        END IF
        RETURN
    END IF
    
    ! Check dimensions
    IF (nota) THEN
        IF (LDA < MAX(1,M)) THEN
            PRINT *, 'CGECSR: LDA too small'
            RETURN
        END IF
    ELSE
        IF (LDA < MAX(1,K)) THEN
            PRINT *, 'CGECSR: LDA too small for transposed A'
            RETURN
        END IF
    END IF
    
    IF (LDC < MAX(1,M)) THEN
        PRINT *, 'CGECSR: LDC too small'
        RETURN
    END IF
    
    IF (notb) THEN
        IF (B%nrows /= K .OR. B%ncols /= N) THEN
            PRINT *, 'CGECSR: Dimension mismatch in B'
            RETURN
        END IF
    ELSE
        IF (B%ncols /= K .OR. B%nrows /= N) THEN
            PRINT *, 'CGECSR: Dimension mismatch in transposed B'
            RETURN
        END IF
    END IF
    
    ! Initialize C
    IF (BETA == (0.0_real32, 0.0_real32)) THEN
        CO j = 1, N
            CO i = 1, M
                C(i,j) = (0.0_real32, 0.0_real32)
            END CO
        END CO
    ELSE IF (BETA /= (1.0_real32, 0.0_real32)) THEN
        CO j = 1, N
            CO i = 1, M
                C(i,j) = BETA * C(i,j)
            END CO
        END CO
    END IF
    
    ! Main computation
    IF (nota .AND. notb) THEN
        ! C = alpha*A*B + beta*C
        ! For each row l of B (which gives column values for C)
        CO l = 1, K
            row_start = B%row_ptr(l)
            row_end = B%row_ptr(l+1) - 1
            
            IF (row_start <= row_end) THEN
                ! For each non-zero in row l of B
                CO jb = row_start, row_end
                    j = B%col_ind(jb)  ! Column index in B and C
                    temp = ALPHA * B%values(jb)
                    
                    ! C(:,j) += temp * A(:,l)
                    CO i = 1, M
                        C(i,j) = C(i,j) + temp * A(i,l)
                    END CO
                END CO
            END IF
        END CO
        
    ELSE IF (.NOT. nota .AND. notb) THEN
        ! C = alpha*A^T*B + beta*C
        ! For each row l of B
        CO l = 1, K
            row_start = B%row_ptr(l)
            row_end = B%row_ptr(l+1) - 1
            
            IF (row_start <= row_end) THEN
                ! For each non-zero in row l of B
                CO jb = row_start, row_end
                    j = B%col_ind(jb)  ! Column index in B and C
                    temp = ALPHA * B%values(jb)
                    
                    ! C(:,j) += temp * A(l,:)^T
                    CO i = 1, M
                        C(i,j) = C(i,j) + temp * A(l,i)
                    END CO
                END CO
            END IF
        END CO
        
    ELSE IF (nota .AND. .NOT. notb) THEN
        ! C = alpha*A*B^T + beta*C
        ! B^T means we treat CSR rows as columns
        ! For each column j of C (which is row j of B)
        CO j = 1, N
            row_start = B%row_ptr(j)
            row_end = B%row_ptr(j+1) - 1
            
            IF (row_start <= row_end) THEN
                ! For each non-zero in row j of B (column j of B^T)
                CO jb = row_start, row_end
                    l = B%col_ind(jb)  ! This is row index in B^T
                    temp = ALPHA * B%values(jb)
                    
                    ! C(:,j) += temp * A(:,l)
                    CO i = 1, M
                        C(i,j) = C(i,j) + temp * A(i,l)
                    END CO
                END CO
            END IF
        END CO
        
    ELSE
        ! C = alpha*A^T*B^T + beta*C
        ! Both matrices transposed
        CO j = 1, N
            row_start = B%row_ptr(j)
            row_end = B%row_ptr(j+1) - 1
            
            IF (row_start <= row_end) THEN
                ! For each non-zero in row j of B (column j of B^T)
                CO jb = row_start, row_end
                    l = B%col_ind(jb)  ! This is row index in B^T
                    temp = ALPHA * B%values(jb)
                    
                    ! C(:,j) += temp * A(l,:)^T
                    CO i = 1, M
                        C(i,j) = C(i,j) + temp * A(l,i)
                    END CO
                END CO
            END IF
        END CO
    END IF

END SUBROUTINE CGECSR