#!/usr/bin/env python3
"""
Example: Parse a single Fortran file and display results
"""

from pathlib import Path
import sys

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.lapack_util.fortran_parser import FortranParser


def main():
    # Example file to parse
    file_path = Path("BLAS/SRC/dgemm.f")
    
    if not file_path.exists():
        print(f"File {file_path} not found!")
        print("Make sure to run this from the LAPACK root directory")
        return
    
    # Create parser and parse file
    parser = FortranParser()
    result = parser.parse_file(file_path)
    
    # Display results
    print(f"=== Parsing Results for {file_path} ===")
    print(f"Error: {result.error}")
    print(f"Routines found: {len(result.routines)}")
    
    for routine in result.routines:
        print(f"\nRoutine: {routine.name}")
        print(f"  Type: {routine.routine_type}")
        print(f"  Precision: {routine.precision}")
        print(f"  Operation: {routine.operation}")
        print(f"  Lines: {routine.line_start}-{routine.line_end}")
        print(f"  Calls: {', '.join(sorted(routine.calls)) if routine.calls else 'None'}")


if __name__ == "__main__":
    main()