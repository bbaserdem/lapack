!> \brief Sparse matrix test routines
!>
!> This module contains individual test routines for sparse matrix operations

MODULE sparse_tests
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64
    IMPLICIT NONE
    
CONTAINS

    !> Test COO initialization and structure
    SUBROUTINE DSPTEST01(COO, RESULT)
        TYPE(sparse_coo_d), INTENT(IN) :: COO
        DOUBLE PRECISION, INTENT(OUT) :: RESULT
        
        INTEGER :: I, IERR
        DOUBLE PRECISION :: NORM, EPS
        DOUBLE PRECISION, EXTERNAL :: DLAMCH
        
        EPS = DLAMCH('E')
        RESULT = 0.0_real64
        
        ! Check basic properties
        IF (COO%nnz < 0 .OR. COO%nnz > COO%nnz_alloc) THEN
            RESULT = 1.0_real64 / EPS
            RETURN
        END IF
        
        ! Check indices are in bounds
        DO I = 1, COO%nnz
            IF (COO%row_ind(I) < 1 .OR. COO%row_ind(I) > COO%nrows) THEN
                RESULT = 1.0_real64 / EPS
                RETURN
            END IF
            IF (COO%col_ind(I) < 1 .OR. COO%col_ind(I) > COO%ncols) THEN
                RESULT = 1.0_real64 / EPS
                RETURN
            END IF
        END DO
        
        ! Test passed
        RESULT = 0.0_real64
        
    END SUBROUTINE DSPTEST01
    
    !> Test COO SpMV operation
    SUBROUTINE DSPTEST02(COO, RESULT)
        TYPE(sparse_coo_d), INTENT(IN) :: COO
        DOUBLE PRECISION, INTENT(OUT) :: RESULT
        
        INTEGER :: I, N, M
        DOUBLE PRECISION :: ALPHA, BETA, NORM, RESID, EPS
        DOUBLE PRECISION, ALLOCATABLE :: X(:), Y(:), Y_REF(:)
        DOUBLE PRECISION, EXTERNAL :: DLAMCH, DNRM2
        
        EPS = DLAMCH('E')
        N = COO%ncols
        M = COO%nrows
        
        ! Allocate vectors
        ALLOCATE(X(N), Y(M), Y_REF(M))
        
        ! Initialize test vectors
        DO I = 1, N
            X(I) = 1.0_real64 / DBLE(I)
        END DO
        Y = 0.0_real64
        Y_REF = 0.0_real64
        
        ! Compute reference result manually
        DO I = 1, COO%nnz
            Y_REF(COO%row_ind(I)) = Y_REF(COO%row_ind(I)) + &
                                    COO%values(I) * X(COO%col_ind(I))
        END DO
        
        ! Test SpMV
        ALPHA = 1.0_real64
        BETA = 0.0_real64
        CALL DCOOMV('N', M, N, ALPHA, COO, X, 1, BETA, Y, 1)
        
        ! Compute residual
        DO I = 1, M
            Y(I) = Y(I) - Y_REF(I)
        END DO
        RESID = DNRM2(M, Y, 1)
        NORM = DNRM2(M, Y_REF, 1)
        
        IF (NORM > 0.0_real64) THEN
            RESULT = RESID / (NORM * EPS * SQRT(DBLE(COO%nnz)))
        ELSE
            RESULT = RESID / EPS
        END IF
        
        DEALLOCATE(X, Y, Y_REF)
        
    END SUBROUTINE DSPTEST02
    
    !> Test COO to CSR conversion
    SUBROUTINE DSPTEST03(COO, RESULT)
        TYPE(sparse_coo_d), INTENT(IN) :: COO
        DOUBLE PRECISION, INTENT(OUT) :: RESULT
        
        TYPE(sparse_csr_d) :: CSR
        INTEGER :: I, INFO, M, N
        DOUBLE PRECISION :: ALPHA, BETA, NORM, RESID, EPS
        DOUBLE PRECISION, ALLOCATABLE :: X(:), Y1(:), Y2(:)
        DOUBLE PRECISION, EXTERNAL :: DLAMCH, DNRM2
        
        EPS = DLAMCH('E')
        M = COO%nrows
        N = COO%ncols
        
        ! Convert to CSR
        CALL DCOOCONV('CSR', COO, CSR, INFO)
        IF (INFO /= 0) THEN
            RESULT = 1.0_real64 / EPS
            RETURN
        END IF
        
        ! Test by comparing SpMV results
        ALLOCATE(X(N), Y1(M), Y2(M))
        
        ! Random vector
        DO I = 1, N
            X(I) = SIN(DBLE(I))
        END DO
        
        ! COO SpMV
        ALPHA = 1.0_real64
        BETA = 0.0_real64
        CALL DCOOMV('N', M, N, ALPHA, COO, X, 1, BETA, Y1, 1)
        
        ! CSR SpMV
        CALL DCSRMV('N', M, N, ALPHA, CSR, X, 1, BETA, Y2, 1)
        
        ! Compare results
        DO I = 1, M
            Y2(I) = Y2(I) - Y1(I)
        END DO
        RESID = DNRM2(M, Y2, 1)
        NORM = DNRM2(M, Y1, 1)
        
        IF (NORM > 0.0_real64) THEN
            RESULT = RESID / (NORM * EPS)
        ELSE
            RESULT = RESID / EPS
        END IF
        
        ! Clean up
        CALL DCSRFREE(CSR, INFO)
        DEALLOCATE(X, Y1, Y2)
        
    END SUBROUTINE DSPTEST03
    
    !> Test CSR SpMV operation
    SUBROUTINE DSPTEST04(CSR, RESULT)
        TYPE(sparse_csr_d), INTENT(IN) :: CSR
        DOUBLE PRECISION, INTENT(OUT) :: RESULT
        
        INTEGER :: I, J, M, N
        DOUBLE PRECISION :: ALPHA, BETA, NORM, RESID, EPS
        DOUBLE PRECISION, ALLOCATABLE :: X(:), Y(:), Y_REF(:)
        DOUBLE PRECISION, EXTERNAL :: DLAMCH, DNRM2
        
        EPS = DLAMCH('E')
        M = CSR%nrows
        N = CSR%ncols
        
        ! Allocate vectors
        ALLOCATE(X(N), Y(M), Y_REF(M))
        
        ! Test with ones vector
        X = 1.0_real64
        Y = 0.0_real64
        Y_REF = 0.0_real64
        
        ! Compute reference result
        DO I = 1, M
            DO J = CSR%row_ptr(I), CSR%row_ptr(I+1)-1
                Y_REF(I) = Y_REF(I) + CSR%values(J)
            END DO
        END DO
        
        ! Test CSR SpMV
        ALPHA = 1.0_real64
        BETA = 0.0_real64
        CALL DCSRMV('N', M, N, ALPHA, CSR, X, 1, BETA, Y, 1)
        
        ! Compute residual
        DO I = 1, M
            Y(I) = Y(I) - Y_REF(I)
        END DO
        RESID = DNRM2(M, Y, 1)
        NORM = DNRM2(M, Y_REF, 1)
        
        IF (NORM > 0.0_real64) THEN
            RESULT = RESID / (NORM * EPS)
        ELSE
            RESULT = RESID / EPS
        END IF
        
        DEALLOCATE(X, Y, Y_REF)
        
    END SUBROUTINE DSPTEST04
    
    !> Test CSR operations (transpose, etc.)
    SUBROUTINE DSPTEST05(CSR, RESULT) 
        TYPE(sparse_csr_d), INTENT(IN) :: CSR
        DOUBLE PRECISION, INTENT(OUT) :: RESULT
        
        INTEGER :: I, M, N
        DOUBLE PRECISION :: ALPHA, BETA, NORM1, NORM2, EPS
        DOUBLE PRECISION, ALLOCATABLE :: X(:), Y1(:), Y2(:)
        DOUBLE PRECISION, EXTERNAL :: DLAMCH, DNRM2
        
        EPS = DLAMCH('E')
        M = CSR%nrows
        N = CSR%ncols
        
        ! Test transpose SpMV
        ALLOCATE(X(M), Y1(N), Y2(N))
        
        ! Initialize vectors
        DO I = 1, M
            X(I) = COS(DBLE(I))
        END DO
        Y1 = 0.0_real64
        Y2 = 0.0_real64
        
        ! y = A^T * x using normal operation
        ALPHA = 1.0_real64
        BETA = 0.0_real64
        CALL DCSRMV('T', M, N, ALPHA, CSR, X, 1, BETA, Y1, 1)
        
        ! Manual computation for verification
        DO I = 1, M
            DO J = CSR%row_ptr(I), CSR%row_ptr(I+1)-1
                Y2(CSR%col_ind(J)) = Y2(CSR%col_ind(J)) + CSR%values(J) * X(I)
            END DO
        END DO
        
        ! Compare
        DO I = 1, N
            Y1(I) = Y1(I) - Y2(I)
        END DO
        NORM1 = DNRM2(N, Y1, 1)
        NORM2 = DNRM2(N, Y2, 1)
        
        IF (NORM2 > 0.0_real64) THEN
            RESULT = NORM1 / (NORM2 * EPS)
        ELSE
            RESULT = NORM1 / EPS
        END IF
        
        DEALLOCATE(X, Y1, Y2)
        
    END SUBROUTINE DSPTEST05
    
    !> Test CSC SpMV operation
    SUBROUTINE DSPTEST06(CSC, RESULT)
        TYPE(sparse_csc_d), INTENT(IN) :: CSC
        DOUBLE PRECISION, INTENT(OUT) :: RESULT
        
        INTEGER :: I, J, M, N
        DOUBLE PRECISION :: ALPHA, BETA, NORM, RESID, EPS
        DOUBLE PRECISION, ALLOCATABLE :: X(:), Y(:), Y_REF(:)
        DOUBLE PRECISION, EXTERNAL :: DLAMCH, DNRM2
        
        EPS = DLAMCH('E')
        M = CSC%nrows
        N = CSC%ncols
        
        ! Allocate vectors
        ALLOCATE(X(N), Y(M), Y_REF(M))
        
        ! Test vector
        DO I = 1, N
            X(I) = DBLE(I) / DBLE(N)
        END DO
        Y = 0.0_real64
        Y_REF = 0.0_real64
        
        ! Compute reference result
        DO J = 1, N
            DO I = CSC%col_ptr(J), CSC%col_ptr(J+1)-1
                Y_REF(CSC%row_ind(I)) = Y_REF(CSC%row_ind(I)) + &
                                        CSC%values(I) * X(J)
            END DO
        END DO
        
        ! Test CSC SpMV
        ALPHA = 1.0_real64
        BETA = 0.0_real64
        CALL DCSCMV('N', M, N, ALPHA, CSC, X, 1, BETA, Y, 1)
        
        ! Compute residual
        DO I = 1, M
            Y(I) = Y(I) - Y_REF(I)
        END DO
        RESID = DNRM2(M, Y, 1)
        NORM = DNRM2(M, Y_REF, 1)
        
        IF (NORM > 0.0_real64) THEN
            RESULT = RESID / (NORM * EPS)
        ELSE
            RESULT = RESID / EPS
        END IF
        
        DEALLOCATE(X, Y, Y_REF)
        
    END SUBROUTINE DSPTEST06
    
    !> Test CSC operations
    SUBROUTINE DSPTEST07(CSC, RESULT)
        TYPE(sparse_csc_d), INTENT(IN) :: CSC
        DOUBLE PRECISION, INTENT(OUT) :: RESULT
        
        ! For now, just check structure validity
        RESULT = 0.0_real64
        
        ! Check column pointers
        IF (CSC%col_ptr(1) /= 1) THEN
            RESULT = 1.0_real64 / DLAMCH('E')
            RETURN
        END IF
        
        ! Check monotonicity
        DO I = 1, CSC%ncols
            IF (CSC%col_ptr(I+1) < CSC%col_ptr(I)) THEN
                RESULT = 1.0_real64 / DLAMCH('E')
                RETURN
            END IF
        END DO
        
    END SUBROUTINE DSPTEST07
    
    !> Test advanced operations (CSR-CSC multiplication, etc.)
    SUBROUTINE DSPTEST08(COO, CSR, CSC, RESULT)
        TYPE(sparse_coo_d), INTENT(IN) :: COO
        TYPE(sparse_csr_d), INTENT(IN) :: CSR  
        TYPE(sparse_csc_d), INTENT(IN) :: CSC
        DOUBLE PRECISION, INTENT(OUT) :: RESULT
        
        TYPE(sparse_csr_d) :: C
        INTEGER :: I, INFO, M, N, K
        DOUBLE PRECISION :: ALPHA, BETA, NORM, EPS
        DOUBLE PRECISION, ALLOCATABLE :: X(:), Y1(:), Y2(:), TEMP(:)
        DOUBLE PRECISION, EXTERNAL :: DLAMCH, DNRM2
        
        EPS = DLAMCH('E')
        
        ! Test CSR-CSC multiplication if dimensions allow
        IF (CSR%ncols == CSC%nrows) THEN
            M = CSR%nrows
            N = CSC%ncols
            K = CSR%ncols
            
            ALLOCATE(X(N), Y1(M), Y2(M), TEMP(K))
            
            ! Test (A * B) * x = A * (B * x)
            DO I = 1, N
                X(I) = DBLE(I) / DBLE(N+1)
            END DO
            
            ! Method 1: Compute C = A * B, then C * x
            CALL DCSRCSC(CSR%nrows, CSC%ncols, CSR, CSC, C, INFO)
            IF (INFO == 0) THEN
                ALPHA = 1.0_real64
                BETA = 0.0_real64
                CALL DCSRMV('N', M, N, ALPHA, C, X, 1, BETA, Y1, 1)
                
                ! Method 2: Compute y = A * (B * x)
                CALL DCSCMV('N', K, N, ALPHA, CSC, X, 1, BETA, TEMP, 1)
                CALL DCSRMV('N', M, K, ALPHA, CSR, TEMP, 1, BETA, Y2, 1)
                
                ! Compare results
                DO I = 1, M
                    Y1(I) = Y1(I) - Y2(I)
                END DO
                NORM = DNRM2(M, Y2, 1)
                
                IF (NORM > 0.0_real64) THEN
                    RESULT = DNRM2(M, Y1, 1) / (NORM * EPS * SQRT(DBLE(M)))
                ELSE
                    RESULT = DNRM2(M, Y1, 1) / EPS
                END IF
                
                CALL DCSRFREE(C, INFO)
            ELSE
                RESULT = 1.0_real64 / EPS
            END IF
            
            DEALLOCATE(X, Y1, Y2, TEMP)
        ELSE
            ! Dimensions don't match for multiplication test
            RESULT = 0.0_real64
        END IF
        
    END SUBROUTINE DSPTEST08

END MODULE sparse_tests