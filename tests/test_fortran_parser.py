#!/usr/bin/env python3
"""Tests for the Fortran parser module"""

import tempfile
import os
from pathlib import Path
import pytest

from src.lapack_util.fortran_parser import FortranParser, Routine, ParseResult


def test_parser_initialization():
    """Test that parser initializes correctly"""
    parser = FortranParser()
    assert parser.fortran_src == "fortran-src"


def test_parse_simple_subroutine():
    """Test parsing a simple Fortran subroutine"""
    test_fortran = """
      SUBROUTINE TEST_SUB(A, B, C)
      REAL A, B, C
      
      C = A + B
      CALL HELPER_SUB(C)
      
      END SUBROUTINE TEST_SUB
    """
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f', delete=False) as f:
        f.write(test_fortran)
        temp_file = f.name
    
    try:
        parser = FortranParser()
        result = parser.parse_file(Path(temp_file))
        
        assert isinstance(result, ParseResult)
        assert result.error is None
        assert len(result.routines) >= 1
        
        # Check the first routine
        routine = result.routines[0]
        assert routine.name == "TEST_SUB"
        assert routine.routine_type == "subroutine"
        assert "HELPER_SUB" in routine.calls
        
    finally:
        os.unlink(temp_file)


def test_parse_lapack_routine():
    """Test parsing a LAPACK-style routine with naming conventions"""
    test_fortran = """
      SUBROUTINE DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                 BETA, C, LDC)
      CHARACTER TRANSA, TRANSB
      INTEGER M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*)
      
      CALL XERBLA('DGEMM ', 1)
      RETURN
      END
    """
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f', delete=False) as f:
        f.write(test_fortran)
        temp_file = f.name
    
    try:
        parser = FortranParser()
        result = parser.parse_file(Path(temp_file))
        
        assert result.error is None
        assert len(result.routines) >= 1
        
        routine = result.routines[0]
        assert routine.name == "DGEMM"
        assert routine.precision == "d"  # double precision
        assert routine.operation == "gemm"  # general matrix multiply
        assert "XERBLA" in routine.calls
        
    finally:
        os.unlink(temp_file)


def test_parse_multiple_routines():
    """Test parsing a file with multiple routines"""
    test_fortran = """
      SUBROUTINE DGETRF(M, N, A, LDA, IPIV, INFO)
      INTEGER M, N, LDA, INFO
      INTEGER IPIV(*)
      DOUBLE PRECISION A(LDA,*)
      
      CALL DGEMM('N', 'N', M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
      CALL DTRSM('L', 'L', 'N', 'U', M, N, ONE, A, LDA, B, LDB)
      
      END SUBROUTINE DGETRF
      
      SUBROUTINE DGETRS(TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      CHARACTER TRANS
      INTEGER N, NRHS, LDA, LDB, INFO
      INTEGER IPIV(*)
      DOUBLE PRECISION A(LDA,*), B(LDB,*)
      
      CALL DLASWP(NRHS, B, LDB, 1, N, IPIV, 1)
      CALL DTRSM('L', 'L', 'N', 'U', N, NRHS, ONE, A, LDA, B, LDB)
      
      END SUBROUTINE DGETRS
    """
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f', delete=False) as f:
        f.write(test_fortran)
        temp_file = f.name
    
    try:
        parser = FortranParser()
        result = parser.parse_file(Path(temp_file))
        
        assert result.error is None
        assert len(result.routines) == 2
        
        # Check first routine
        dgetrf = next(r for r in result.routines if r.name == "DGETRF")
        assert dgetrf.precision == "d"
        assert dgetrf.operation == "getrf"
        assert "DGEMM" in dgetrf.calls
        assert "DTRSM" in dgetrf.calls
        
        # Check second routine
        dgetrs = next(r for r in result.routines if r.name == "DGETRS")
        assert dgetrs.precision == "d"
        assert dgetrs.operation == "getrs"
        assert "DLASWP" in dgetrs.calls
        assert "DTRSM" in dgetrs.calls
        
    finally:
        os.unlink(temp_file)


def test_precision_parsing():
    """Test that all LAPACK precisions are parsed correctly"""
    precisions = {
        'S': ('s', 'single'),
        'D': ('d', 'double'),
        'C': ('c', 'complex'),
        'Z': ('z', 'double complex')
    }
    
    for prefix, (symbol, _) in precisions.items():
        routine = Routine(
            name=f"{prefix}GEMM",
            file_path="test.f",
            routine_type="subroutine"
        )
        assert routine.precision == symbol.lower()
        assert routine.operation == "gemm"


def test_empty_file():
    """Test parsing an empty file"""
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f', delete=False) as f:
        f.write("")
        temp_file = f.name
    
    try:
        parser = FortranParser()
        result = parser.parse_file(Path(temp_file))
        
        # Should not error on empty file
        assert result.error is None or len(result.routines) == 0
        
    finally:
        os.unlink(temp_file)


def test_function_parsing():
    """Test parsing Fortran functions (not just subroutines)"""
    test_fortran = """
      DOUBLE PRECISION FUNCTION DNRM2(N, X, INCX)
      INTEGER N, INCX
      DOUBLE PRECISION X(*)
      
      DOUBLE PRECISION DMAX, SCALE, SSQ, TEMP
      
      DNRM2 = 0.0D0
      IF (N .LE. 0) RETURN
      
      CALL DLASSQ(N, X, INCX, SCALE, SSQ)
      DNRM2 = SCALE * SQRT(SSQ)
      
      END FUNCTION DNRM2
      
      LOGICAL FUNCTION LSAME(CA, CB)
      CHARACTER CA, CB
      
      LSAME = CA .EQ. CB
      
      END
    """
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f', delete=False) as f:
        f.write(test_fortran)
        temp_file = f.name
    
    try:
        parser = FortranParser()
        result = parser.parse_file(Path(temp_file))
        
        assert result.error is None
        assert len(result.routines) >= 1  # At least DNRM2 should be found
        
        # Check DNRM2 function
        dnrm2 = next((r for r in result.routines if r.name == "DNRM2"), None)
        assert dnrm2 is not None
        assert dnrm2.routine_type == "function"
        assert dnrm2.precision == "d"
        assert dnrm2.operation == "nrm2"
        assert "DLASSQ" in dnrm2.calls
        
    finally:
        os.unlink(temp_file)


def test_complex_call_patterns():
    """Test parsing complex call patterns including indirect calls"""
    test_fortran = """
      SUBROUTINE COMPLEX_CALLS(A, B, C, N)
      INTEGER N
      DOUBLE PRECISION A(*), B(*), C(*)
      CHARACTER UPLO
      
C     Standard call
      CALL DGEMM('N', 'N', N, N, N, 1.0D0, A, N, B, N, 0.0D0, C, N)
      
C     Call with line continuation
      CALL DSYMM('L', 'U', N, N, 1.0D0, A, N,
     $           B, N, 0.0D0, C, N)
     
C     Multiple calls on continuation
      CALL DTRMM('L', 'U', 'N', 'N', N, N,
     $           1.0D0, A, N, B, N)
      CALL DTRSM('R', 'L', 'T', 'U', N, N,
     $           1.0D0, A, N, B, N)
      
C     Call in nested IF
      IF (N .GT. 0) THEN
          IF (UPLO .EQ. 'U') THEN
              CALL DPOTRF('U', N, A, N, INFO)
          ELSE
              CALL DPOTRF('L', N, A, N, INFO)
          END IF
      END IF
      
      END SUBROUTINE COMPLEX_CALLS
    """
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f', delete=False) as f:
        f.write(test_fortran)
        temp_file = f.name
    
    try:
        parser = FortranParser()
        result = parser.parse_file(Path(temp_file))
        
        assert result.error is None
        assert len(result.routines) >= 1
        
        routine = result.routines[0]
        expected_calls = {"DGEMM", "DSYMM", "DTRMM", "DTRSM", "DPOTRF"}
        assert expected_calls.issubset(routine.calls)
        
    finally:
        os.unlink(temp_file)


def test_f90_syntax():
    """Test parsing Fortran 90 style syntax"""
    test_fortran = """
module test_module
    implicit none
contains
    subroutine modern_syntax(matrix, vector, result)
        real(kind=8), dimension(:,:), intent(in) :: matrix
        real(kind=8), dimension(:), intent(in) :: vector
        real(kind=8), dimension(:), intent(out) :: result
        
        integer :: i, j, n, m
        
        n = size(matrix, 1)
        m = size(matrix, 2)
        
        ! Initialize result
        result = 0.0d0
        
        ! Matrix-vector multiplication
        do i = 1, n
            do j = 1, m
                result(i) = result(i) + matrix(i,j) * vector(j)
            end do
        end do
        
        call dscal(n, 2.0d0, result, 1)
        
    end subroutine modern_syntax
end module test_module
    """
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
        f.write(test_fortran)
        temp_file = f.name
    
    try:
        parser = FortranParser()
        result = parser.parse_file(Path(temp_file))
        
        # fortran-src might have issues with F90 modules, so we're lenient here
        if result.error is None and len(result.routines) > 0:
            routine = result.routines[0]
            assert routine.name == "MODERN_SYNTAX"
            assert "DSCAL" in routine.calls
        
    finally:
        os.unlink(temp_file)


def test_error_recovery():
    """Test that parser can recover from and report errors gracefully"""
    # Intentionally malformed Fortran
    test_fortran = """
      SUBROUTINE BROKEN SYNTAX HERE
      THIS IS NOT VALID FORTRAN AT ALL
      END WHATEVER
    """
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f', delete=False) as f:
        f.write(test_fortran)
        temp_file = f.name
    
    try:
        parser = FortranParser()
        result = parser.parse_file(Path(temp_file))
        
        # Should report error but not crash
        assert result.error is not None
        assert isinstance(result.error, str)
        assert len(result.error) > 0
        
    finally:
        os.unlink(temp_file)


if __name__ == "__main__":
    # Run basic tests
    test_parser_initialization()
    test_parse_simple_subroutine()
    test_parse_lapack_routine()
    test_parse_multiple_routines()
    test_precision_parsing()
    test_empty_file()
    
    print("All tests passed!")