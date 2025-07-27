!> \brief \b DTIMSP - Timing tests for sparse matrix routines
!>
!> This program measures performance of sparse matrix operations

PROGRAM DTIMSP
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: real64, output_unit
    IMPLICIT NONE
    
    ! Parameters
    INTEGER, PARAMETER :: NMAX = 10000
    INTEGER, PARAMETER :: NNZMAX = 1000000
    INTEGER, PARAMETER :: NSIZE = 5
    INTEGER, PARAMETER :: NNZSIZE = 5
    
    ! Test problem sizes
    INTEGER :: SIZES(NSIZE) = [100, 500, 1000, 5000, 10000]
    REAL(real64) :: DENSITIES(NNZSIZE) = [0.001_real64, 0.005_real64, &
                                           0.01_real64, 0.05_real64, 0.1_real64]
    
    ! Local variables
    INTEGER :: I, J, K, N, NNZ, INFO, NREP
    REAL(real64) :: T1, T2, TIME, MFLOPS, DENSITY
    REAL(real64) :: ALPHA, BETA
    
    ! Arrays
    INTEGER, ALLOCATABLE :: ROW_IND(:), COL_IND(:), ROW_PTR(:), COL_PTR(:)
    REAL(real64), ALLOCATABLE :: VALUES(:), X(:), Y(:), Y_REF(:)
    
    ! Sparse matrices
    TYPE(sparse_coo_d) :: COO
    TYPE(sparse_csr_d) :: CSR
    TYPE(sparse_csc_d) :: CSC
    
    ! External functions
    REAL(real64), EXTERNAL :: DSECND, DLARAN
    
    ! Print header
    WRITE(output_unit, '(A)') 'Sparse Matrix Performance Tests'
    WRITE(output_unit, '(A)') '==============================='
    WRITE(output_unit, *)
    WRITE(output_unit, '(A)') 'Testing SpMV performance for different formats'
    WRITE(output_unit, *)
    WRITE(output_unit, '(A)') 'Matrix Size | Density | NNZ     | Format | Time(s) | MFLOPS'
    WRITE(output_unit, '(A)') '----------- | ------- | ------- | ------ | ------- | -------'
    
    ! Initialize random seed
    CALL RANDOM_SEED()
    
    ! Loop over matrix sizes
    DO I = 1, NSIZE
        N = SIZES(I)
        
        ! Allocate vectors
        ALLOCATE(X(N), Y(N), Y_REF(N))
        
        ! Initialize test vector
        DO J = 1, N
            X(J) = 1.0_real64 / REAL(J, real64)
        END DO
        
        ! Loop over densities
        DO J = 1, NNZSIZE
            DENSITY = DENSITIES(J)
            NNZ = INT(DENSITY * N * N)
            IF (NNZ > NNZMAX) CYCLE
            
            ! Allocate arrays
            ALLOCATE(ROW_IND(NNZ), COL_IND(NNZ), VALUES(NNZ))
            
            ! Generate random sparse matrix
            CALL GENERATE_RANDOM_SPARSE(N, NNZ, ROW_IND, COL_IND, VALUES)
            
            ! Test COO format
            CALL DCOOALLOC(N, N, NNZ, COO, INFO)
            CALL DCOOINIT(ROW_IND, COL_IND, VALUES, NNZ, COO, INFO)
            
            ! Warm up
            ALPHA = 1.0_real64
            BETA = 0.0_real64
            CALL DCOOMV('N', N, N, ALPHA, COO, X, 1, BETA, Y, 1)
            
            ! Time COO SpMV
            NREP = MAX(1, 1000000 / NNZ)  ! Adjust repetitions based on size
            T1 = DSECND()
            DO K = 1, NREP
                CALL DCOOMV('N', N, N, ALPHA, COO, X, 1, BETA, Y, 1)
            END DO
            T2 = DSECND()
            TIME = (T2 - T1) / REAL(NREP, real64)
            MFLOPS = 2.0_real64 * NNZ / (TIME * 1.0E6_real64)
            
            WRITE(output_unit, '(I11, A, F7.3, A, I7, A, A6, A, F7.4, A, F7.1)') &
                N, ' | ', DENSITY, ' | ', NNZ, ' | ', 'COO   ', ' | ', TIME, ' | ', MFLOPS
            
            ! Convert to CSR and test
            CALL DCOOCONV('CSR', COO, CSR, INFO)
            IF (INFO == 0) THEN
                ! Warm up
                CALL DCSRMV('N', N, N, ALPHA, CSR, X, 1, BETA, Y, 1)
                
                ! Time CSR SpMV
                T1 = DSECND()
                DO K = 1, NREP
                    CALL DCSRMV('N', N, N, ALPHA, CSR, X, 1, BETA, Y, 1)
                END DO
                T2 = DSECND()
                TIME = (T2 - T1) / REAL(NREP, real64)
                MFLOPS = 2.0_real64 * NNZ / (TIME * 1.0E6_real64)
                
                WRITE(output_unit, '(I11, A, F7.3, A, I7, A, A6, A, F7.4, A, F7.1)') &
                    N, ' | ', DENSITY, ' | ', NNZ, ' | ', 'CSR   ', ' | ', TIME, ' | ', MFLOPS
            END IF
            
            ! Convert to CSC and test
            CALL DCOOCONV('CSC', COO, CSC, INFO)
            IF (INFO == 0) THEN
                ! Warm up
                CALL DCSCMV('N', N, N, ALPHA, CSC, X, 1, BETA, Y, 1)
                
                ! Time CSC SpMV
                T1 = DSECND()
                DO K = 1, NREP
                    CALL DCSCMV('N', N, N, ALPHA, CSC, X, 1, BETA, Y, 1)
                END DO
                T2 = DSECND()
                TIME = (T2 - T1) / REAL(NREP, real64)
                MFLOPS = 2.0_real64 * NNZ / (TIME * 1.0E6_real64)
                
                WRITE(output_unit, '(I11, A, F7.3, A, I7, A, A6, A, F7.4, A, F7.1)') &
                    N, ' | ', DENSITY, ' | ', NNZ, ' | ', 'CSC   ', ' | ', TIME, ' | ', MFLOPS
            END IF
            
            WRITE(output_unit, *)  ! Blank line between test cases
            
            ! Clean up
            CALL DCOOFREE(COO, INFO)
            CALL DCSRFREE(CSR, INFO)
            CALL DCSCFREE(CSC, INFO)
            DEALLOCATE(ROW_IND, COL_IND, VALUES)
            
        END DO ! Density loop
        
        DEALLOCATE(X, Y, Y_REF)
        
    END DO ! Size loop
    
    ! Test format conversion performance
    WRITE(output_unit, *)
    WRITE(output_unit, '(A)') 'Format Conversion Performance'
    WRITE(output_unit, '(A)') '============================'
    WRITE(output_unit, *)
    WRITE(output_unit, '(A)') 'Size | NNZ     | Conversion    | Time(s)'
    WRITE(output_unit, '(A)') '---- | ------- | ------------- | -------'
    
    ! Test conversion times
    N = 1000
    NNZ = 50000
    ALLOCATE(ROW_IND(NNZ), COL_IND(NNZ), VALUES(NNZ))
    
    ! Generate test matrix
    CALL GENERATE_RANDOM_SPARSE(N, NNZ, ROW_IND, COL_IND, VALUES)
    CALL DCOOALLOC(N, N, NNZ, COO, INFO)
    CALL DCOOINIT(ROW_IND, COL_IND, VALUES, NNZ, COO, INFO)
    
    ! Time COO to CSR
    T1 = DSECND()
    CALL DCOOCONV('CSR', COO, CSR, INFO)
    T2 = DSECND()
    WRITE(output_unit, '(I4, A, I7, A, A13, A, F7.4)') &
        N, ' | ', NNZ, ' | ', 'COO -> CSR   ', ' | ', T2-T1
    
    ! Time COO to CSC
    T1 = DSECND()
    CALL DCOOCONV('CSC', COO, CSC, INFO)
    T2 = DSECND()
    WRITE(output_unit, '(I4, A, I7, A, A13, A, F7.4)') &
        N, ' | ', NNZ, ' | ', 'COO -> CSC   ', ' | ', T2-T1
    
    ! Clean up
    CALL DCOOFREE(COO, INFO)
    CALL DCSRFREE(CSR, INFO)
    CALL DCSCFREE(CSC, INFO)
    DEALLOCATE(ROW_IND, COL_IND, VALUES)
    
CONTAINS

    SUBROUTINE GENERATE_RANDOM_SPARSE(N, NNZ, ROW_IND, COL_IND, VALUES)
        INTEGER, INTENT(IN) :: N, NNZ
        INTEGER, INTENT(OUT) :: ROW_IND(NNZ), COL_IND(NNZ)
        REAL(real64), INTENT(OUT) :: VALUES(NNZ)
        
        INTEGER :: I, J, K
        REAL(real64) :: R
        LOGICAL :: FOUND
        INTEGER :: ISEED(4) = [1234, 5678, 9012, 3456]
        
        ! Simple random generation (may have duplicates)
        DO K = 1, NNZ
            FOUND = .TRUE.
            DO WHILE (FOUND)
                CALL RANDOM_NUMBER(R)
                I = INT(R * N) + 1
                CALL RANDOM_NUMBER(R)
                J = INT(R * N) + 1
                
                ! Simple check for duplicates (not efficient for large NNZ)
                FOUND = .FALSE.
                IF (K > 1) THEN
                    DO L = 1, K-1
                        IF (ROW_IND(L) == I .AND. COL_IND(L) == J) THEN
                            FOUND = .TRUE.
                            EXIT
                        END IF
                    END DO
                END IF
            END DO
            
            ROW_IND(K) = I
            COL_IND(K) = J
            CALL RANDOM_NUMBER(R)
            VALUES(K) = 2.0_real64 * R - 1.0_real64
        END DO
        
    END SUBROUTINE GENERATE_RANDOM_SPARSE

END PROGRAM DTIMSP