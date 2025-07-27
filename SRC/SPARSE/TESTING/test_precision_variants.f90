!> \brief Test suite for multi-precision sparse matrix implementations
!>
!> Tests all four precision variants (S, D, C, Z) to ensure consistency
!> across implementations.

PROGRAM test_precision_variants
    USE sparse_types_extended
    USE ISO_FORTRAN_ENV, ONLY: int32, real32, real64, output_unit
    IMPLICIT NONE
    
    ! Test counters
    INTEGER :: tests_passed = 0
    INTEGER :: tests_failed = 0
    
    WRITE(output_unit, '(A)') 'Testing Multi-Precision Sparse Matrix Implementations'
    WRITE(output_unit, '(A)') '===================================================='
    
    ! Test each precision
    CALL test_single_precision()
    CALL test_double_precision()
    CALL test_complex_precision()
    CALL test_double_complex_precision()
    
    ! Summary
    WRITE(output_unit, '(A)') ''
    WRITE(output_unit, '(A,I0,A,I0,A)') 'Total Tests: ', tests_passed, ' passed, ', &
                                         tests_failed, ' failed'
    
    IF (tests_failed > 0) THEN
        STOP 1
    END IF
    
CONTAINS

    !=========================================================================
    ! SINGLE PRECISION TESTS
    !=========================================================================
    
    SUBROUTINE test_single_precision()
        TYPE(sparse_coo_s) :: coo_s
        TYPE(sparse_csr_s) :: csr_s
        REAL(real32) :: val_s(3) = [1.0_real32, 2.0_real32, 3.0_real32]
        INTEGER :: row_ind(3) = [1, 2, 3]
        INTEGER :: col_ind(3) = [1, 2, 3]
        INTEGER :: info
        
        INTERFACE
            SUBROUTINE SCOOALLOC(NROWS, NCOLS, NNZ, COO, INFO)
                USE sparse_types_extended
                INTEGER, INTENT(IN) :: NROWS, NCOLS, NNZ
                TYPE(sparse_coo_s), INTENT(OUT) :: COO
                INTEGER, INTENT(OUT) :: INFO
            END SUBROUTINE SCOOALLOC
            
            SUBROUTINE SCOOINIT(ROWIND, COLIND, VAL, NNZ, COO, INFO)
                USE sparse_types_extended
                USE ISO_FORTRAN_ENV, ONLY: real32
                INTEGER, INTENT(IN) :: NNZ
                INTEGER, INTENT(IN) :: ROWIND(NNZ), COLIND(NNZ)
                REAL(real32), INTENT(IN) :: VAL(NNZ)
                TYPE(sparse_coo_s), INTENT(INOUT) :: COO
                INTEGER, INTENT(OUT) :: INFO
            END SUBROUTINE SCOOINIT
        END INTERFACE
        
        WRITE(output_unit, '(A)') ''
        WRITE(output_unit, '(A)') 'Testing Single Precision (S)...'
        
        ! Test COO allocation and initialization
        CALL SCOOALLOC(3, 3, 3, coo_s, info)
        IF (info /= 0) THEN
            WRITE(output_unit, '(A)') '  FAILED: SCOOALLOC'
            tests_failed = tests_failed + 1
        ELSE
            CALL SCOOINIT(row_ind, col_ind, val_s, 3, coo_s, info)
            IF (info /= 0) THEN
                WRITE(output_unit, '(A)') '  FAILED: SCOOINIT'
                tests_failed = tests_failed + 1
            ELSE
                WRITE(output_unit, '(A)') '  PASSED: Single precision COO'
                tests_passed = tests_passed + 1
            END IF
        END IF
        
        ! Clean up
        IF (ALLOCATED(coo_s%row_ind)) DEALLOCATE(coo_s%row_ind)
        IF (ALLOCATED(coo_s%col_ind)) DEALLOCATE(coo_s%col_ind)
        IF (ALLOCATED(coo_s%values)) DEALLOCATE(coo_s%values)
        
    END SUBROUTINE test_single_precision
    
    !=========================================================================
    ! DOUBLE PRECISION TESTS
    !=========================================================================
    
    SUBROUTINE test_double_precision()
        TYPE(sparse_coo_d) :: coo_d
        REAL(real64) :: val_d(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        INTEGER :: row_ind(3) = [1, 2, 3]
        INTEGER :: col_ind(3) = [1, 2, 3]
        INTEGER :: info
        
        INTERFACE
            SUBROUTINE DCOOALLOC(NROWS, NCOLS, NNZ, COO, INFO)
                USE sparse_types_extended
                INTEGER, INTENT(IN) :: NROWS, NCOLS, NNZ
                TYPE(sparse_coo_d), INTENT(OUT) :: COO
                INTEGER, INTENT(OUT) :: INFO
            END SUBROUTINE DCOOALLOC
            
            SUBROUTINE DCOOINIT(ROWIND, COLIND, VAL, NNZ, COO, INFO)
                USE sparse_types_extended
                USE ISO_FORTRAN_ENV, ONLY: real64
                INTEGER, INTENT(IN) :: NNZ
                INTEGER, INTENT(IN) :: ROWIND(NNZ), COLIND(NNZ)
                REAL(real64), INTENT(IN) :: VAL(NNZ)
                TYPE(sparse_coo_d), INTENT(INOUT) :: COO
                INTEGER, INTENT(OUT) :: INFO
            END SUBROUTINE DCOOINIT
        END INTERFACE
        
        WRITE(output_unit, '(A)') ''
        WRITE(output_unit, '(A)') 'Testing Double Precision (D)...'
        
        ! Test COO allocation and initialization
        CALL DCOOALLOC(3, 3, 3, coo_d, info)
        IF (info /= 0) THEN
            WRITE(output_unit, '(A)') '  FAILED: DCOOALLOC'
            tests_failed = tests_failed + 1
        ELSE
            CALL DCOOINIT(row_ind, col_ind, val_d, 3, coo_d, info)
            IF (info /= 0) THEN
                WRITE(output_unit, '(A)') '  FAILED: DCOOINIT'
                tests_failed = tests_failed + 1
            ELSE
                WRITE(output_unit, '(A)') '  PASSED: Double precision COO'
                tests_passed = tests_passed + 1
            END IF
        END IF
        
        ! Clean up
        IF (ALLOCATED(coo_d%row_ind)) DEALLOCATE(coo_d%row_ind)
        IF (ALLOCATED(coo_d%col_ind)) DEALLOCATE(coo_d%col_ind)
        IF (ALLOCATED(coo_d%values)) DEALLOCATE(coo_d%values)
        
    END SUBROUTINE test_double_precision
    
    !=========================================================================
    ! COMPLEX PRECISION TESTS
    !=========================================================================
    
    SUBROUTINE test_complex_precision()
        TYPE(sparse_coo_c) :: coo_c
        COMPLEX(real32) :: val_c(3) = [(1.0_real32, 0.0_real32), &
                                        (2.0_real32, 1.0_real32), &
                                        (3.0_real32, -1.0_real32)]
        INTEGER :: row_ind(3) = [1, 2, 3]
        INTEGER :: col_ind(3) = [1, 2, 3]
        INTEGER :: info
        
        INTERFACE
            SUBROUTINE CCOOALLOC(NROWS, NCOLS, NNZ, COO, INFO)
                USE sparse_types_extended
                INTEGER, INTENT(IN) :: NROWS, NCOLS, NNZ
                TYPE(sparse_coo_c), INTENT(OUT) :: COO
                INTEGER, INTENT(OUT) :: INFO
            END SUBROUTINE CCOOALLOC
            
            SUBROUTINE CCOOINIT(ROWIND, COLIND, VAL, NNZ, COO, INFO)
                USE sparse_types_extended
                USE ISO_FORTRAN_ENV, ONLY: real32
                INTEGER, INTENT(IN) :: NNZ
                INTEGER, INTENT(IN) :: ROWIND(NNZ), COLIND(NNZ)
                COMPLEX(real32), INTENT(IN) :: VAL(NNZ)
                TYPE(sparse_coo_c), INTENT(INOUT) :: COO
                INTEGER, INTENT(OUT) :: INFO
            END SUBROUTINE CCOOINIT
        END INTERFACE
        
        WRITE(output_unit, '(A)') ''
        WRITE(output_unit, '(A)') 'Testing Complex Precision (C)...'
        
        ! Test COO allocation and initialization
        CALL CCOOALLOC(3, 3, 3, coo_c, info)
        IF (info /= 0) THEN
            WRITE(output_unit, '(A)') '  FAILED: CCOOALLOC'
            tests_failed = tests_failed + 1
        ELSE
            CALL CCOOINIT(row_ind, col_ind, val_c, 3, coo_c, info)
            IF (info /= 0) THEN
                WRITE(output_unit, '(A)') '  FAILED: CCOOINIT'
                tests_failed = tests_failed + 1
            ELSE
                WRITE(output_unit, '(A)') '  PASSED: Complex precision COO'
                tests_passed = tests_passed + 1
            END IF
        END IF
        
        ! Clean up
        IF (ALLOCATED(coo_c%row_ind)) DEALLOCATE(coo_c%row_ind)
        IF (ALLOCATED(coo_c%col_ind)) DEALLOCATE(coo_c%col_ind)
        IF (ALLOCATED(coo_c%values)) DEALLOCATE(coo_c%values)
        
    END SUBROUTINE test_complex_precision
    
    !=========================================================================
    ! DOUBLE COMPLEX PRECISION TESTS
    !=========================================================================
    
    SUBROUTINE test_double_complex_precision()
        TYPE(sparse_coo_z) :: coo_z
        COMPLEX(real64) :: val_z(3) = [(1.0_real64, 0.0_real64), &
                                        (2.0_real64, 1.0_real64), &
                                        (3.0_real64, -1.0_real64)]
        INTEGER :: row_ind(3) = [1, 2, 3]
        INTEGER :: col_ind(3) = [1, 2, 3]
        INTEGER :: info
        
        INTERFACE
            SUBROUTINE ZCOOALLOC(NROWS, NCOLS, NNZ, COO, INFO)
                USE sparse_types_extended
                INTEGER, INTENT(IN) :: NROWS, NCOLS, NNZ
                TYPE(sparse_coo_z), INTENT(OUT) :: COO
                INTEGER, INTENT(OUT) :: INFO
            END SUBROUTINE ZCOOALLOC
            
            SUBROUTINE ZCOOINIT(ROWIND, COLIND, VAL, NNZ, COO, INFO)
                USE sparse_types_extended
                USE ISO_FORTRAN_ENV, ONLY: real64
                INTEGER, INTENT(IN) :: NNZ
                INTEGER, INTENT(IN) :: ROWIND(NNZ), COLIND(NNZ)
                COMPLEX(real64), INTENT(IN) :: VAL(NNZ)
                TYPE(sparse_coo_z), INTENT(INOUT) :: COO
                INTEGER, INTENT(OUT) :: INFO
            END SUBROUTINE ZCOOINIT
        END INTERFACE
        
        WRITE(output_unit, '(A)') ''
        WRITE(output_unit, '(A)') 'Testing Double Complex Precision (Z)...'
        
        ! Test COO allocation and initialization
        CALL ZCOOALLOC(3, 3, 3, coo_z, info)
        IF (info /= 0) THEN
            WRITE(output_unit, '(A)') '  FAILED: ZCOOALLOC'
            tests_failed = tests_failed + 1
        ELSE
            CALL ZCOOINIT(row_ind, col_ind, val_z, 3, coo_z, info)
            IF (info /= 0) THEN
                WRITE(output_unit, '(A)') '  FAILED: ZCOOINIT'
                tests_failed = tests_failed + 1
            ELSE
                WRITE(output_unit, '(A)') '  PASSED: Double complex precision COO'
                tests_passed = tests_passed + 1
            END IF
        END IF
        
        ! Clean up
        IF (ALLOCATED(coo_z%row_ind)) DEALLOCATE(coo_z%row_ind)
        IF (ALLOCATED(coo_z%col_ind)) DEALLOCATE(coo_z%col_ind)
        IF (ALLOCATED(coo_z%values)) DEALLOCATE(coo_z%values)
        
    END SUBROUTINE test_double_complex_precision

END PROGRAM test_precision_variants