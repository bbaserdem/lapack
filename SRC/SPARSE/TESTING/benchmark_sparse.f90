!> \brief Performance benchmark suite for sparse matrix operations
!>
!> This program benchmarks the performance of sparse matrix operations
!> including conversions, SpMV, and I/O operations

PROGRAM benchmark_sparse
    USE sparse_types
    USE sparse_constants
    USE ISO_FORTRAN_ENV, ONLY: int32, real64, int64, output_unit
    IMPLICIT NONE
    
    ! Benchmark parameters
    INTEGER, PARAMETER :: NTRIALS = 100
    INTEGER, PARAMETER :: WARMUP = 10
    
    ! Matrix sizes to test
    INTEGER, PARAMETER :: NSIZES = 5
    INTEGER, DIMENSION(NSIZES) :: sizes = [100, 500, 1000, 5000, 10000]
    REAL(real64), PARAMETER :: DENSITY = 0.01_real64  ! 1% sparse
    
    ! Timing variables
    INTEGER(int64) :: count_start, count_end, count_rate, count_max
    REAL(real64) :: time_elapsed, time_total, time_avg
    REAL(real64) :: gflops, bandwidth_gb
    
    ! Matrices and vectors
    TYPE(sparse_coo_d) :: coo
    TYPE(sparse_csr_d) :: csr
    TYPE(sparse_csc_d) :: csc
    REAL(real64), ALLOCATABLE :: x(:), y(:), dense(:,:)
    
    ! Loop variables
    INTEGER :: i, j, k, n, nnz, info, trial
    REAL(real64) :: alpha, beta
    
    ! External routines
    EXTERNAL :: DCOOMV, DCSRMV, DCSCMV
    EXTERNAL :: DCOOALLOC, DCOOFREE, DCSRALLOC, DCSRFREE, DCSCALLOC, DCSCFREE
    EXTERNAL :: DCOO2CSR, DCOO2CSC, DCSR2COO, DCSC2COO
    EXTERNAL :: DGEMV
    
    WRITE(output_unit, '(A)') '================================================='
    WRITE(output_unit, '(A)') 'Sparse Matrix Operations Performance Benchmark'
    WRITE(output_unit, '(A)') '================================================='
    WRITE(output_unit, '(A,F5.2,A)') 'Matrix density: ', DENSITY*100.0_real64, '%'
    WRITE(output_unit, '(A,I0)') 'Number of trials: ', NTRIALS
    WRITE(output_unit, '(A)') ''
    
    ! Get timer information
    CALL SYSTEM_CLOCK(count_rate=count_rate)
    
    ! Loop over matrix sizes
    DO i = 1, NSIZES
        n = sizes(i)
        nnz = INT(n * n * DENSITY)
        
        WRITE(output_unit, '(A)') '================================================='
        WRITE(output_unit, '(A,I0,A,I0)') 'Matrix size: ', n, ' x ', n
        WRITE(output_unit, '(A,I0)') 'Approximate non-zeros: ', nnz
        WRITE(output_unit, '(A)') ''
        
        ! Allocate vectors
        ALLOCATE(x(n), y(n))
        
        ! Initialize vectors
        x = 1.0_real64
        y = 0.0_real64
        alpha = 1.0_real64
        beta = 0.0_real64
        
        ! Create random sparse matrix
        CALL create_random_sparse_matrix(n, n, DENSITY, coo, info)
        IF (info /= 0) THEN
            WRITE(output_unit, '(A,I0)') 'Error creating sparse matrix, INFO = ', info
            DEALLOCATE(x, y)
            CYCLE
        END IF
        
        nnz = coo%nnz  ! Actual number of non-zeros
        WRITE(output_unit, '(A,I0)') 'Actual non-zeros: ', nnz
        WRITE(output_unit, '(A,F6.3,A)') 'Actual density: ', &
            REAL(nnz, real64) / REAL(n*n, real64) * 100.0_real64, '%'
        WRITE(output_unit, '(A)') ''
        
        ! Benchmark COO to CSR conversion
        WRITE(output_unit, '(A)') 'COO to CSR Conversion:'
        CALL DCSRALLOC(n, n, nnz, csr, info)
        
        ! Warmup
        DO trial = 1, WARMUP
            CALL DCOO2CSR(coo, csr, info)
        END DO
        
        ! Timed runs
        time_total = 0.0_real64
        DO trial = 1, NTRIALS
            CALL SYSTEM_CLOCK(count_start)
            CALL DCOO2CSR(coo, csr, info)
            CALL SYSTEM_CLOCK(count_end)
            time_elapsed = REAL(count_end - count_start, real64) / REAL(count_rate, real64)
            time_total = time_total + time_elapsed
        END DO
        
        time_avg = time_total / REAL(NTRIALS, real64)
        bandwidth_gb = REAL(nnz * (8 + 4 + 4), real64) / (time_avg * 1.0E9_real64)  ! GB/s
        WRITE(output_unit, '(A,F10.6,A)') '  Average time: ', time_avg * 1000.0_real64, ' ms'
        WRITE(output_unit, '(A,F10.3,A)') '  Bandwidth: ', bandwidth_gb, ' GB/s'
        WRITE(output_unit, '(A)') ''
        
        ! Benchmark COO to CSC conversion
        WRITE(output_unit, '(A)') 'COO to CSC Conversion:'
        CALL DCSCALLOC(n, n, nnz, csc, info)
        
        ! Warmup
        DO trial = 1, WARMUP
            CALL DCOO2CSC(coo, csc, info)
        END DO
        
        ! Timed runs
        time_total = 0.0_real64
        DO trial = 1, NTRIALS
            CALL SYSTEM_CLOCK(count_start)
            CALL DCOO2CSC(coo, csc, info)
            CALL SYSTEM_CLOCK(count_end)
            time_elapsed = REAL(count_end - count_start, real64) / REAL(count_rate, real64)
            time_total = time_total + time_elapsed
        END DO
        
        time_avg = time_total / REAL(NTRIALS, real64)
        bandwidth_gb = REAL(nnz * (8 + 4 + 4), real64) / (time_avg * 1.0E9_real64)
        WRITE(output_unit, '(A,F10.6,A)') '  Average time: ', time_avg * 1000.0_real64, ' ms'
        WRITE(output_unit, '(A,F10.3,A)') '  Bandwidth: ', bandwidth_gb, ' GB/s'
        WRITE(output_unit, '(A)') ''
        
        ! Benchmark SpMV operations
        WRITE(output_unit, '(A)') 'Sparse Matrix-Vector Multiplication (y = A*x):'
        
        ! COO SpMV
        WRITE(output_unit, '(A)') '  COO format:'
        
        ! Warmup
        DO trial = 1, WARMUP
            CALL DCOOMV('N', n, n, alpha, coo, x, 1, beta, y, 1)
        END DO
        
        ! Timed runs
        time_total = 0.0_real64
        DO trial = 1, NTRIALS
            CALL SYSTEM_CLOCK(count_start)
            CALL DCOOMV('N', n, n, alpha, coo, x, 1, beta, y, 1)
            CALL SYSTEM_CLOCK(count_end)
            time_elapsed = REAL(count_end - count_start, real64) / REAL(count_rate, real64)
            time_total = time_total + time_elapsed
        END DO
        
        time_avg = time_total / REAL(NTRIALS, real64)
        gflops = (2.0_real64 * REAL(nnz, real64)) / (time_avg * 1.0E9_real64)
        WRITE(output_unit, '(A,F10.6,A)') '    Average time: ', time_avg * 1000.0_real64, ' ms'
        WRITE(output_unit, '(A,F10.3,A)') '    Performance: ', gflops, ' GFLOPS'
        
        ! CSR SpMV
        WRITE(output_unit, '(A)') '  CSR format:'
        
        ! Warmup
        DO trial = 1, WARMUP
            CALL DCSRMV('N', n, n, alpha, csr, x, 1, beta, y, 1)
        END DO
        
        ! Timed runs
        time_total = 0.0_real64
        DO trial = 1, NTRIALS
            CALL SYSTEM_CLOCK(count_start)
            CALL DCSRMV('N', n, n, alpha, csr, x, 1, beta, y, 1)
            CALL SYSTEM_CLOCK(count_end)
            time_elapsed = REAL(count_end - count_start, real64) / REAL(count_rate, real64)
            time_total = time_total + time_elapsed
        END DO
        
        time_avg = time_total / REAL(NTRIALS, real64)
        gflops = (2.0_real64 * REAL(nnz, real64)) / (time_avg * 1.0E9_real64)
        WRITE(output_unit, '(A,F10.6,A)') '    Average time: ', time_avg * 1000.0_real64, ' ms'
        WRITE(output_unit, '(A,F10.3,A)') '    Performance: ', gflops, ' GFLOPS'
        
        ! CSC SpMV
        WRITE(output_unit, '(A)') '  CSC format:'
        
        ! Warmup
        DO trial = 1, WARMUP
            CALL DCSCMV('N', n, n, alpha, csc, x, 1, beta, y, 1)
        END DO
        
        ! Timed runs
        time_total = 0.0_real64
        DO trial = 1, NTRIALS
            CALL SYSTEM_CLOCK(count_start)
            CALL DCSCMV('N', n, n, alpha, csc, x, 1, beta, y, 1)
            CALL SYSTEM_CLOCK(count_end)
            time_elapsed = REAL(count_end - count_start, real64) / REAL(count_rate, real64)
            time_total = time_total + time_elapsed
        END DO
        
        time_avg = time_total / REAL(NTRIALS, real64)
        gflops = (2.0_real64 * REAL(nnz, real64)) / (time_avg * 1.0E9_real64)
        WRITE(output_unit, '(A,F10.6,A)') '    Average time: ', time_avg * 1000.0_real64, ' ms'
        WRITE(output_unit, '(A,F10.3,A)') '    Performance: ', gflops, ' GFLOPS'
        
        ! Compare with dense DGEMV for small matrices
        IF (n <= 1000) THEN
            WRITE(output_unit, '(A)') '  Dense format (DGEMV):'
            ALLOCATE(dense(n,n))
            
            ! Convert to dense
            CALL DCOO2DEN(coo, dense, n, info)
            
            ! Warmup
            DO trial = 1, WARMUP
                CALL DGEMV('N', n, n, alpha, dense, n, x, 1, beta, y, 1)
            END DO
            
            ! Timed runs
            time_total = 0.0_real64
            DO trial = 1, NTRIALS
                CALL SYSTEM_CLOCK(count_start)
                CALL DGEMV('N', n, n, alpha, dense, n, x, 1, beta, y, 1)
                CALL SYSTEM_CLOCK(count_end)
                time_elapsed = REAL(count_end - count_start, real64) / REAL(count_rate, real64)
                time_total = time_total + time_elapsed
            END DO
            
            time_avg = time_total / REAL(NTRIALS, real64)
            gflops = (2.0_real64 * REAL(n*n, real64)) / (time_avg * 1.0E9_real64)
            WRITE(output_unit, '(A,F10.6,A)') '    Average time: ', time_avg * 1000.0_real64, ' ms'
            WRITE(output_unit, '(A,F10.3,A)') '    Performance: ', gflops, ' GFLOPS (full matrix)'
            
            DEALLOCATE(dense)
        END IF
        
        WRITE(output_unit, '(A)') ''
        
        ! Clean up
        CALL DCOOFREE(coo, info)
        CALL DCSRFREE(csr, info)
        CALL DCSCFREE(csc, info)
        DEALLOCATE(x, y)
    END DO
    
    WRITE(output_unit, '(A)') '================================================='
    WRITE(output_unit, '(A)') 'Benchmark completed!'
    
CONTAINS

    SUBROUTINE create_random_sparse_matrix(m, n, density, coo, info)
        INTEGER, INTENT(IN) :: m, n
        REAL(real64), INTENT(IN) :: density
        TYPE(sparse_coo_d), INTENT(OUT) :: coo
        INTEGER, INTENT(OUT) :: info
        
        INTEGER :: i, j, k, nnz_max, nnz_actual
        REAL(real64) :: rand_val
        INTEGER, ALLOCATABLE :: row_ind(:), col_ind(:)
        REAL(real64), ALLOCATABLE :: values(:)
        LOGICAL, ALLOCATABLE :: mask(:,:)
        
        EXTERNAL :: DCOOALLOC, DCOOINIT
        
        ! Estimate maximum non-zeros
        nnz_max = CEILING(REAL(m*n, real64) * density)
        
        ! Allocate temporary arrays
        ALLOCATE(row_ind(nnz_max), col_ind(nnz_max), values(nnz_max))
        ALLOCATE(mask(m,n))
        
        mask = .FALSE.
        nnz_actual = 0
        
        ! Generate random sparse pattern
        DO k = 1, nnz_max
            DO
                CALL RANDOM_NUMBER(rand_val)
                i = 1 + INT(rand_val * m)
                IF (i > m) i = m
                
                CALL RANDOM_NUMBER(rand_val)
                j = 1 + INT(rand_val * n)
                IF (j > n) j = n
                
                IF (.NOT. mask(i,j)) EXIT
            END DO
            
            mask(i,j) = .TRUE.
            nnz_actual = nnz_actual + 1
            row_ind(nnz_actual) = i
            col_ind(nnz_actual) = j
            CALL RANDOM_NUMBER(values(nnz_actual))
        END DO
        
        ! Allocate and initialize COO matrix
        CALL DCOOALLOC(m, n, nnz_actual, coo, info)
        IF (info /= 0) THEN
            DEALLOCATE(row_ind, col_ind, values, mask)
            RETURN
        END IF
        
        CALL DCOOINIT(row_ind(1:nnz_actual), col_ind(1:nnz_actual), &
                      values(1:nnz_actual), nnz_actual, coo, info)
        
        DEALLOCATE(row_ind, col_ind, values, mask)
    END SUBROUTINE create_random_sparse_matrix
    
    SUBROUTINE DCOO2DEN(COO, DENSE, LDA, INFO)
        TYPE(sparse_coo_d), INTENT(IN) :: COO
        INTEGER, INTENT(IN) :: LDA
        REAL(real64), INTENT(OUT) :: DENSE(LDA,*)
        INTEGER, INTENT(OUT) :: INFO
        
        INTEGER :: k
        
        INFO = 0
        
        ! Initialize dense matrix to zero
        DENSE(1:COO%nrows, 1:COO%ncols) = 0.0_real64
        
        ! Fill in non-zero elements
        DO k = 1, COO%nnz
            DENSE(COO%row_ind(k), COO%col_ind(k)) = COO%values(k)
        END DO
    END SUBROUTINE DCOO2DEN

END PROGRAM benchmark_sparse