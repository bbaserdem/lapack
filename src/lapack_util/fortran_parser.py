#!/usr/bin/env python3
"""
Fortran source code parser for LAPACK using fortran-src.
Extracts AST and converts to graph format for Neo4j.
"""

import subprocess
import json
import os
import re
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Set
from dataclasses import dataclass, field
import tempfile
import logging

logger = logging.getLogger(__name__)


@dataclass
class Routine:
    """Represents a Fortran routine (subroutine or function)"""
    name: str
    file_path: str
    routine_type: str  # 'subroutine' or 'function'
    precision: Optional[str] = None  # 's', 'd', 'c', 'z'
    operation: Optional[str] = None  # 'gemm', 'getrf', etc.
    calls: Set[str] = field(default_factory=set)
    line_start: Optional[int] = None
    line_end: Optional[int] = None
    
    def __post_init__(self):
        """Parse LAPACK naming convention"""
        if self.name and len(self.name) > 1:
            first_char = self.name[0].lower()
            if first_char in ['s', 'd', 'c', 'z']:
                self.precision = first_char
                self.operation = self.name[1:].lower()


@dataclass
class ParseResult:
    """Result of parsing a Fortran file"""
    file_path: str
    routines: List[Routine]
    error: Optional[str] = None


class FortranParser:
    """Parser for Fortran source files using fortran-src"""
    
    def __init__(self, fortran_src_path: str = "fortran-src"):
        self.fortran_src = fortran_src_path
        self._check_fortran_src()
    
    def _check_fortran_src(self):
        """Check if fortran-src is available"""
        try:
            subprocess.run([self.fortran_src, "--version"], 
                         capture_output=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError):
            raise RuntimeError(f"fortran-src not found at '{self.fortran_src}'")
    
    def parse_file(self, file_path: Path) -> ParseResult:
        """Parse a single Fortran file and extract routines and calls"""
        logger.info(f"Parsing {file_path}")
        
        try:
            # First, get the parsed AST
            ast_result = subprocess.run(
                [self.fortran_src, "-a", "parse", str(file_path)],
                capture_output=True,
                text=True,
                check=True
            )
            
            # Also get the reprinted version for easier parsing
            reprint_result = subprocess.run(
                [self.fortran_src, "-r", str(file_path)],
                capture_output=True,
                text=True,
                check=True
            )
            
            routines = self._extract_routines(ast_result.stdout, reprint_result.stdout, file_path)
            
            return ParseResult(
                file_path=str(file_path),
                routines=routines
            )
            
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to parse {file_path}: {e.stderr}")
            return ParseResult(
                file_path=str(file_path),
                routines=[],
                error=str(e)
            )
    
    def _extract_routines(self, ast_output: str, reprint_output: str, 
                         file_path: Path) -> List[Routine]:
        """Extract routines from AST and reprint output"""
        routines = []
        
        # Parse reprinted output for routine definitions and calls
        lines = reprint_output.split('\n')
        current_routine = None
        
        for i, line in enumerate(lines):
            line_upper = line.upper().strip()
            
            # Check for subroutine definition
            if line_upper.startswith('SUBROUTINE '):
                match = re.match(r'SUBROUTINE\s+(\w+)', line_upper)
                if match:
                    if current_routine:
                        routines.append(current_routine)
                    
                    name = match.group(1)
                    current_routine = Routine(
                        name=name,
                        file_path=str(file_path),
                        routine_type='subroutine',
                        line_start=i + 1
                    )
            
            # Check for function definition
            elif 'FUNCTION' in line_upper and (
                re.match(r'^\s*\w+\s+FUNCTION\s+\w+', line_upper) or 
                re.match(r'^\s*\w+\s+\w+\s+FUNCTION\s+\w+', line_upper) or  # DOUBLE PRECISION FUNCTION
                line_upper.startswith('FUNCTION ')
            ):
                match = re.search(r'FUNCTION\s+(\w+)', line_upper)
                if match:
                    if current_routine:
                        routines.append(current_routine)
                    
                    name = match.group(1)
                    current_routine = Routine(
                        name=name,
                        file_path=str(file_path),
                        routine_type='function',
                        line_start=i + 1
                    )
            
            # Check for CALL statements
            elif current_routine and line_upper.strip().startswith('CALL '):
                match = re.match(r'CALL\s+(\w+)', line_upper.strip())
                if match:
                    called_routine = match.group(1)
                    current_routine.calls.add(called_routine)
            
            # Check for END statements
            elif current_routine and (
                line_upper.startswith('END SUBROUTINE') or 
                line_upper.startswith('END FUNCTION') or
                line_upper == 'END'
            ):
                current_routine.line_end = i + 1
                routines.append(current_routine)
                current_routine = None
        
        # Don't forget the last routine if file doesn't end with END
        if current_routine:
            routines.append(current_routine)
        
        return routines
    
    def parse_directory(self, directory: Path, 
                       extensions: List[str] = ['.f', '.f90', '.f77']) -> List[ParseResult]:
        """Parse all Fortran files in a directory recursively"""
        results = []
        
        for ext in extensions:
            for file_path in directory.rglob(f'*{ext}'):
                result = self.parse_file(file_path)
                results.append(result)
        
        return results


def test_parser():
    """Test the parser with a simple example"""
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
      
      SUBROUTINE DGETRF(M, N, A, LDA, IPIV, INFO)
      INTEGER M, N, LDA, INFO
      INTEGER IPIV(*)
      DOUBLE PRECISION A(LDA,*)
      
      CALL DGEMM('N', 'N', M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
      CALL DTRSM('L', 'L', 'N', 'U', M, N, ONE, A, LDA, B, LDB)
      
      END SUBROUTINE DGETRF
    """
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f', delete=False) as f:
        f.write(test_fortran)
        temp_file = f.name
    
    try:
        parser = FortranParser()
        result = parser.parse_file(Path(temp_file))
        
        print("=== Parser Test Results ===")
        print(f"File: {result.file_path}")
        print(f"Error: {result.error}")
        print(f"Routines found: {len(result.routines)}")
        
        for routine in result.routines:
            print(f"\nRoutine: {routine.name}")
            print(f"  Type: {routine.routine_type}")
            print(f"  Precision: {routine.precision}")
            print(f"  Operation: {routine.operation}")
            print(f"  Lines: {routine.line_start}-{routine.line_end}")
            print(f"  Calls: {routine.calls}")
            
    finally:
        os.unlink(temp_file)


if __name__ == "__main__":
    test_parser()