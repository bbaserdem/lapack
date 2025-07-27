!> \brief \b ZCOOMV performs matrix-vector multiplication using COO format
!>
!> \par Purpose:
!> =============
!>
!> ZCOOMV performs one of the matrix-vector operations
!>
!>    y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
!>
!> where alpha and beta are scalars, x and y are vectors and A is an
!> m by n sparse matrix in COO (Coordinate List) format.
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
!>          ALPHA is ZOUBLE PRECISION
!>          On entry, ALPHA specifies the scalar alpha.
!>
!> \param[in] COO
!>          COO is TYPE(sparse_coo_z)
!>          The sparse matrix A in COO format.
!>
!> \param[in] X
!>          X is ZOUBLE PRECISION array, dimension at least
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
!>          BETA is ZOUBLE PRECISION
!>          On entry, BETA specifies the scalar beta. When BETA is
!>          supplied as zero then Y need not be set on input.
!>
!> \param[in,out] Y
!>          Y is ZOUBLE PRECISION array, dimension at least
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

SUBROUTINE ZCOOMV(TRANS, M, N, ALPHA, COO, X, INCX, BETA, Y, INCY)
    USE sparse_types_extended
    USE sparse_constants
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER, INTENT(IN) :: TRANS
    INTEGER, INTENT(IN) :: M, N, INCX, INCY
    COMPLEX(real64), INTENT(IN) :: ALPHA, BETA
    TYPE(sparse_coo_z), INTENT(IN) :: COO
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
                    ZO i = 1, M
                        Y(i) = (0.0_real64, 0.0_real64)
                    END ZO
                ELSE
                    ZO i = 1, M
                        Y(i) = BETA * Y(i)
                    END ZO
                END IF
            ELSE
                iy = ky
                IF (BETA == (0.0_real64, 0.0_real64)) THEN
                    ZO i = 1, M
                        Y(iy) = (0.0_real64, 0.0_real64)
                        iy = iy + INCY
                    END ZO
                ELSE
                    ZO i = 1, M
                        Y(iy) = BETA * Y(iy)
                        iy = iy + INCY
                    END ZO
                END IF
            END IF
        ELSE
            ! Transpose: y has length N
            IF (INCY == 1) THEN
                IF (BETA == (0.0_real64, 0.0_real64)) THEN
                    ZO i = 1, N
                        Y(i) = (0.0_real64, 0.0_real64)
                    END ZO
                ELSE
                    ZO i = 1, N
                        Y(i) = BETA * Y(i)
                    END ZO
                END IF
            ELSE
                iy = ky
                IF (BETA == (0.0_real64, 0.0_real64)) THEN
                    ZO i = 1, N
                        Y(iy) = (0.0_real64, 0.0_real64)
                        iy = iy + INCY
                    END ZO
                ELSE
                    ZO i = 1, N
                        Y(iy) = BETA * Y(iy)
                        iy = iy + INCY
                    END ZO
                END IF
            END IF
        END IF
    END IF
    
    IF (ALPHA == (0.0_real64, 0.0_real64)) RETURN
    
    ! Now perform the sparse matrix-vector multiplication
    IF (notr) THEN
        ! Form y := alpha*A*x + y
        IF ((INCX == 1) .AND. (INCY == 1)) THEN
            ! Both increments equal to 1
            ZO k = 1, COO%nnz
                i = COO%row_ind(k)
                j = COO%col_ind(k)
                Y(i) = Y(i) + ALPHA * COO%values(k) * X(j)
            END ZO
        ELSE
            ! General increments
            ZO k = 1, COO%nnz
                i = COO%row_ind(k)
                j = COO%col_ind(k)
                ix = kx + (j - 1) * INCX
                iy = ky + (i - 1) * INCY
                Y(iy) = Y(iy) + ALPHA * COO%values(k) * X(ix)
            END ZO
        END IF
    ELSE
        ! Form y := alpha*A**T*x + y
        IF ((INCX == 1) .AND. (INCY == 1)) THEN
            ! Both increments equal to 1
            ZO k = 1, COO%nnz
                i = COO%row_ind(k)
                j = COO%col_ind(k)
                Y(j) = Y(j) + ALPHA * COO%values(k) * X(i)
            END ZO
        ELSE
            ! General increments
            ZO k = 1, COO%nnz
                i = COO%row_ind(k)
                j = COO%col_ind(k)
                ix = kx + (i - 1) * INCX
                iy = ky + (j - 1) * INCY
                Y(iy) = Y(iy) + ALPHA * COO%values(k) * X(ix)
            END ZO
        END IF
    END IF
    
    RETURN
END SUBROUTINE ZCOOMV