#!/usr/bin/env python3
"""
Generate precision variants of sparse matrix routines - improved version.
Handles S, C, and Z precisions from D precision templates.
"""

import os
import re
import shutil
from pathlib import Path

class PrecisionPorter:
    def __init__(self):
        self.precision_map = {
            'S': {
                'type': 'REAL(real32)',
                'kind': 'real32',
                'module': 'sparse_types_extended',
                'desc': 'single precision',
                'function_type': 'REAL',
                'constants': {
                    '0.0_real64': '0.0_real32',
                    '1.0_real64': '1.0_real32',
                    '-1.0_real64': '-1.0_real32',
                    '2.0_real64': '2.0_real32',
                }
            },
            'C': {
                'type': 'COMPLEX(real32)',
                'kind': 'real32',
                'module': 'sparse_types_extended',
                'desc': 'single precision complex',
                'function_type': 'COMPLEX',
                'constants': {
                    '0.0_real64': '(0.0_real32, 0.0_real32)',
                    '1.0_real64': '(1.0_real32, 0.0_real32)',
                    '-1.0_real64': '(-1.0_real32, 0.0_real32)',
                    '2.0_real64': '(2.0_real32, 0.0_real32)',
                }
            },
            'Z': {
                'type': 'COMPLEX(real64)',
                'kind': 'real64',
                'module': 'sparse_types_extended', 
                'desc': 'double precision complex',
                'function_type': 'COMPLEX*16',
                'constants': {
                    '0.0_real64': '(0.0_real64, 0.0_real64)',
                    '1.0_real64': '(1.0_real64, 0.0_real64)',
                    '-1.0_real64': '(-1.0_real64, 0.0_real64)',
                    '2.0_real64': '(2.0_real64, 0.0_real64)',
                }
            }
        }
        
    def port_file(self, source_path, target_precision):
        """Port a double precision file to target precision."""
        source_path = Path(source_path)
        
        # Create target filename
        filename = source_path.name
        if not filename.startswith('D'):
            print(f"Skipping {filename} - doesn't start with D")
            return None
            
        target_filename = target_precision + filename[1:]
        target_path = source_path.parent / target_filename
        
        if target_path.exists():
            print(f"Skipping {target_filename} - already exists")
            return None
            
        # Read source
        content = source_path.read_text()
        
        # Apply transformations
        new_content = self.transform_content(content, target_precision)
        
        # Write target
        target_path.write_text(new_content)
        print(f"Created {target_filename}")
        
        return target_path
        
    def transform_content(self, content, prec):
        """Transform content for target precision."""
        
        # Replace module usage
        content = content.replace('USE sparse_types', 
                                  f'USE {self.precision_map[prec]["module"]}')
        
        # Add real32 import for S and C precisions
        if prec in ['S', 'C']:
            # Add real32 to ISO_FORTRAN_ENV imports
            content = re.sub(
                r'USE ISO_FORTRAN_ENV, ONLY: int32, real64',
                'USE ISO_FORTRAN_ENV, ONLY: int32, real32',
                content
            )
            content = re.sub(
                r'USE ISO_FORTRAN_ENV, ONLY: real64',
                'USE ISO_FORTRAN_ENV, ONLY: real32',
                content
            )
            # Also add to modules and subroutines that don't have ISO_FORTRAN_ENV
            if 'USE ISO_FORTRAN_ENV' not in content and 'real32' in content:
                # Insert ISO_FORTRAN_ENV import after first USE statement
                content = re.sub(
                    r'(USE [a-zA-Z_]+)',
                    r'USE ISO_FORTRAN_ENV, ONLY: real32\n    \1',
                    content, count=1
                )
        
        # Replace subroutine/function names (D prefix to target prefix)
        # Careful patterns to avoid DO loops
        patterns = [
            (r'\bSUBROUTINE\s+D([A-Z][A-Z0-9]*)', rf'SUBROUTINE {prec}\1'),
            (r'\bFUNCTION\s+D([A-Z][A-Z0-9]*)', rf'FUNCTION {prec}\1'),
            (r'\bCALL\s+D([A-Z][A-Z0-9]*)', rf'CALL {prec}\1'),
            (r'\bEXTERNAL\s+D([A-Z][A-Z0-9]*)', rf'EXTERNAL {prec}\1'),
            (r'\bINTERFACE\s+D([A-Z][A-Z0-9]*)', rf'INTERFACE {prec}\1'),
            # Module names
            (r'\bMODULE\s+D([A-Z][A-Z0-9]*_MODULE)', rf'MODULE {prec}\1'),
        ]
        
        for pattern, replacement in patterns:
            content = re.sub(pattern, replacement, content, flags=re.IGNORECASE)
        
        # Replace type names
        content = content.replace('sparse_coo_d', f'sparse_coo_{prec.lower()}')
        content = content.replace('sparse_csr_d', f'sparse_csr_{prec.lower()}')
        content = content.replace('sparse_csc_d', f'sparse_csc_{prec.lower()}')
        
        # Replace REAL declarations
        content = content.replace('REAL(real64)', self.precision_map[prec]['type'])
        content = content.replace('DOUBLE PRECISION', self.precision_map[prec]['function_type'])
        
        # For complex types, handle special cases
        if prec in ['C', 'Z']:
            # Handle transpose options - add conjugate transpose
            content = re.sub(
                r"(LSAME\(TRANS[A-Z]*, 'T'\))",
                r"\1 .OR. LSAME(TRANSA, 'C')",
                content
            )
            content = re.sub(
                r"(LSAME\(TRANS, 'T'\))",
                r"\1 .OR. LSAME(TRANS, 'C')",
                content
            )
            
            # Handle ABS for complex (use CABS/ZABS)
            if prec == 'C':
                content = re.sub(r'\bABS\(', 'CABS(', content)
            else:
                content = re.sub(r'\bABS\(', 'ZABS(', content)
        
        # Replace constants
        for old_const, new_const in self.precision_map[prec]['constants'].items():
            content = content.replace(old_const, new_const)
        
        # Replace real64 kind parameter
        if prec in ['S', 'C']:
            content = content.replace('real64', 'real32')
        
        # Update documentation - be careful with word boundaries
        content = re.sub(r'\bdouble precision\b', self.precision_map[prec]['desc'], content, flags=re.IGNORECASE)
        content = re.sub(r'\bDOUBLE PRECISION\b', self.precision_map[prec]['desc'].upper(), content)
        
        # Fix any lingering issues
        # Replace incorrectly transformed words
        content = content.replace('SO ', 'DO ')
        content = content.replace('END SO', 'END DO')
        content = content.replace('SOUBLE PRECISION', prec + 'OUBLE PRECISION')
        content = content.replace('COUBLE PRECISION', 'COMPLEX')
        content = content.replace('ZOUBLE PRECISION', 'COMPLEX*16')
        
        return content
        
    def port_directory(self, directory, target_precision):
        """Port all D* files in a directory."""
        directory = Path(directory)
        ported_files = []
        
        for f in directory.glob('D*.f90'):
            result = self.port_file(f, target_precision)
            if result:
                ported_files.append(result)
                
        return ported_files

def main():
    """Main entry point."""
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python generate_precision_variants_v2.py <precision> [directory]")
        print("  precision: S, C, or Z")
        print("  directory: specific directory to process (optional)")
        sys.exit(1)
        
    precision = sys.argv[1].upper()
    if precision not in ['S', 'C', 'Z']:
        print(f"Invalid precision: {precision}")
        sys.exit(1)
        
    porter = PrecisionPorter()
    
    # Process directories
    sparse_dir = Path(__file__).parent
    
    if len(sys.argv) > 2:
        # Process specific directory
        directory = Path(sys.argv[2])
        if not directory.exists():
            print(f"Directory not found: {directory}")
            sys.exit(1)
        ported = porter.port_directory(directory, precision)
        print(f"\nPorted {len(ported)} files in {directory}")
    else:
        # Process all subdirectories
        total_ported = 0
        for subdir in ['COO', 'CSR', 'CSC', 'UTILS', 'IO']:
            dir_path = sparse_dir / subdir
            if dir_path.exists():
                print(f"\nProcessing {subdir}...")
                ported = porter.port_directory(dir_path, precision)
                total_ported += len(ported)
                
        print(f"\nTotal: Ported {total_ported} files to {precision} precision")

if __name__ == '__main__':
    main()