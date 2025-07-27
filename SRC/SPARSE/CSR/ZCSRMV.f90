!> \brief \b DCSRMV performs matrix-vector multiplication using CSR format
!>
!> \par Purpose:
!> =============
!>
!> DCSRMV performs one of the matrix-vector operations
!>
!>    y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
!>
!> where alpha and beta are scalars, x and y are vectors and A is an
!> m by n sparse matrix in CSR (Compressed Sparse Row) format.
!>
!> \param[in] TRANS
!>          TRANS is CHARACTER*1
!>          On entry, TRANS specifies the operation to be performed as
!>          follows:
!>
!>             TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
!>
!>             TRANS = 'T' or 't'   y := alpha*A**T*x + beta*y.
!>
!>             TRANS = 'C' or 'c'   y := alpha*A**T*x + beta*y.
!>
!> \param[in] M
!>          M is INTEGER
!>          On entry, M specifies the number of rows of the matrix A.
!>          M must be at least zero.
!>
!> \param[in] N
!>          N is INTEGER
!>          On entry, N specifies the number of columns of the matrix A.
!>          N must be at least zero.
!>
!> \param[in] ALPHA
!>          ALPHA is COMPLEX*16
!>          On entry, ALPHA specifies the scalar alpha.
!>
!> \param[in] CSR
!>          CSR is TYPE(sparse_csr_z)
!>          The sparse matrix A in CSR format.
!>
!> \param[in] X
!>          X is COMPLEX*16 array, dimension at least
!>          ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
!>          and at least
!>          ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
!>          Before entry, the incremented array X must contain the
!>          vector x.
!>
!> \param[in] INCX
!>          INCX is INTEGER
!>          On entry, INCX specifies the increment for the elements of
!>          X. INCX must not be zero.
!>
!> \param[in] BETA
!>          BETA is COMPLEX*16
!>          On entry, BETA specifies the scalar beta. When BETA is
!>          supplied as zero then Y need not be set on input.
!>
!> \param[in,out] Y
!>          Y is COMPLEX*16 array, dimension at least
!>          ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
!>          and at least
!>          ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
!>          Before entry with BETA non-zero, the incremented array Y
!>          must contain the vector y. On exit, Y is overwritten by the
!>          updated vector y.
!>
!> \param[in] INCY
!>          INCY is INTEGER
!>          On entry, INCY specifies the increment for the elements of
!>          Y. INCY must not be zero.

SUBROUTINE ZCSRMV(TRANS, M, N, ALPHA, CSR, X, INCX, BETA, Y, INCY)
    USE sparse_types_extended
    USE sparse_constants
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER, INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: M, N, INCX, INCY
    COMPLEX(real64), INTENT(IN) :: ALPHA, BETA
    TYPE(sparse_csr_z), INTENT(IN) :: CSR
    COMPLEX(real64), INTENT(IN) :: X(*)
    COMPLEX(real64), INTENT(INOUT) :: Y(*)
    
    ! Local variables
    INTEGER :: i, j, k, ix, iy, jx, jy, kx, ky
    COMPLEX(real64) :: temp
    LOGICAL :: notr
    
    ! External functions
    LOGICAL :: LSAME
    EXTERNAL :: LSAME
    
    ! Test the input parameters
    notr = LSAME(TRANS, 'N')
    
    ! Quick return if possible
    IF ((M == 0) .OR. (N == 0) .OR. &
        ((ALPHA == (0.0_real64, 0.0_real64)) .AND. (BETA == (1.0_real64, 0.0_real64)))) RETURN
    
    ! Set up the start points in X and Y
    IF (notr) THEN
        ! No transpose: x has length N, y has length M
        IF (INCX > 0) THEN
            kx = 1
        ELSE
            kx = 1 - (N - 1) * INCX
        END IF
        
        IF (INCY > 0) THEN
            ky = 1
        ELSE
            ky = 1 - (M - 1) * INCY
        END IF
    ELSE
        ! Transpose: x has length M, y has length N
        IF (INCX > 0) THEN
            kx = 1
        ELSE
            kx = 1 - (M - 1) * INCX
        END IF
        
        IF (INCY > 0) THEN
            ky = 1
        ELSE
            ky = 1 - (N - 1) * INCY
        END IF
    END IF
    
    ! Start the operations. In this version the elements of A are
    ! accessed sequentially with one pass through A.
    
    ! First form y := beta*y
    IF (BETA /= (1.0_real64, 0.0_real64)) THEN
        IF (notr) THEN
            ! No transpose: y has length M
            IF (INCY == 1) THEN
                IF (BETA == (0.0_real64, 0.0_real64)) THEN
                    DO i = 1, M
                        Y(i) = (0.0_real64, 0.0_real64)
                    END DO
                ELSE
                    DO i = 1, M
                        Y(i) = BETA * Y(i)
                    END DO
                END IF
            ELSE
                iy = ky
                IF (BETA == (0.0_real64, 0.0_real64)) THEN
                    DO i = 1, M
                        Y(iy) = (0.0_real64, 0.0_real64)
                        iy = iy + INCY
                    END DO
                ELSE
                    DO i = 1, M
                        Y(iy) = BETA * Y(iy)
                        iy = iy + INCY
                    END DO
                END IF
            END IF
        ELSE
            ! Transpose: y has length N
            IF (INCY == 1) THEN
                IF (BETA == (0.0_real64, 0.0_real64)) THEN
                    DO i = 1, N
                        Y(i) = (0.0_real64, 0.0_real64)
                    END DO
                ELSE
                    DO i = 1, N
                        Y(i) = BETA * Y(i)
                    END DO
                END IF
            ELSE
                iy = ky
                IF (BETA == (0.0_real64, 0.0_real64)) THEN
                    DO i = 1, N
                        Y(iy) = (0.0_real64, 0.0_real64)
                        iy = iy + INCY
                    END DO
                ELSE
                    DO i = 1, N
                        Y(iy) = BETA * Y(iy)
                        iy = iy + INCY
                    END DO
                END IF
            END IF
        END IF
    END IF
    
    IF (ALPHA == (0.0_real64, 0.0_real64)) RETURN
    
    IF (notr) THEN
        ! Form y := alpha*A*x + y
        jy = ky
        IF (INCX == 1) THEN
            DO i = 1, M
                temp = (0.0_real64, 0.0_real64)
                DO k = CSR%row_ptr(i), CSR%row_ptr(i+1) - 1
                    j = CSR%col_ind(k)
                    temp = temp + CSR%values(k) * X(j)
                END DO
                Y(jy) = Y(jy) + ALPHA * temp
                jy = jy + INCY
            END DO
        ELSE
            DO i = 1, M
                temp = (0.0_real64, 0.0_real64)
                jx = kx
                DO k = CSR%row_ptr(i), CSR%row_ptr(i+1) - 1
                    j = CSR%col_ind(k)
                    ix = kx + (j - 1) * INCX
                    temp = temp + CSR%values(k) * X(ix)
                END DO
                Y(jy) = Y(jy) + ALPHA * temp
                jy = jy + INCY
            END DO
        END IF
    ELSE
        ! Form y := alpha*A**T*x + y
        jx = kx
        IF (INCY == 1) THEN
            DO i = 1, M
                IF (X(jx) /= (0.0_real64, 0.0_real64)) THEN
                    temp = ALPHA * X(jx)
                    DO k = CSR%row_ptr(i), CSR%row_ptr(i+1) - 1
                        j = CSR%col_ind(k)
                        Y(j) = Y(j) + temp * CSR%values(k)
                    END DO
                END IF
                jx = jx + INCX
            END DO
        ELSE
            DO i = 1, M
                IF (X(jx) /= (0.0_real64, 0.0_real64)) THEN
                    temp = ALPHA * X(jx)
                    iy = ky
                    DO k = CSR%row_ptr(i), CSR%row_ptr(i+1) - 1
                        j = CSR%col_ind(k)
                        iy = ky + (j - 1) * INCY
                        Y(iy) = Y(iy) + temp * CSR%values(k)
                    END DO
                END IF
                jx = jx + INCX
            END DO
        END IF
    END IF
    
    RETURN
END SUBROUTINE ZCSRMV