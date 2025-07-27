#!/usr/bin/env python3
"""
Port double precision sparse matrix routines to other precisions.
Follows LAPACK naming conventions:
- S: Single precision real
- D: Double precision real (original)
- C: Single precision complex
- Z: Double precision complex
"""

import os
import re
import sys
from pathlib import Path

# Precision mappings
PRECISION_MAP = {
    'S': {
        'type': 'real32',
        'kind': 'real32',
        'zero': '0.0_real32',
        'one': '1.0_real32',
        'desc': 'single precision',
        'function_type': 'REAL'
    },
    'D': {
        'type': 'real64',
        'kind': 'real64', 
        'zero': '0.0_real64',
        'one': '1.0_real64',
        'desc': 'double precision',
        'function_type': 'DOUBLE PRECISION'
    },
    'C': {
        'type': 'complex(real32)',
        'kind': 'real32',
        'zero': '(0.0_real32, 0.0_real32)',
        'one': '(1.0_real32, 0.0_real32)',
        'desc': 'single precision complex',
        'function_type': 'COMPLEX'
    },
    'Z': {
        'type': 'complex(real64)',
        'kind': 'real64',
        'zero': '(0.0_real64, 0.0_real64)', 
        'one': '(1.0_real64, 0.0_real64)',
        'desc': 'double precision complex',
        'function_type': 'COMPLEX*16'
    }
}

def port_routine(source_file, target_precision):
    """Port a double precision routine to another precision."""
    
    # Read source file
    with open(source_file, 'r') as f:
        content = f.read()
    
    # Get filename and create target filename
    filename = os.path.basename(source_file)
    if not filename.startswith('D'):
        print(f"Warning: {filename} doesn't start with D")
        return None
        
    target_filename = target_precision + filename[1:]
    target_dir = os.path.dirname(source_file)
    target_path = os.path.join(target_dir, target_filename)
    
    # Skip if already exists
    if os.path.exists(target_path):
        print(f"Skipping {target_filename} - already exists")
        return None
    
    # Perform replacements
    new_content = content
    
    # Replace precision in routine/function names
    # Handle main routine name
    new_content = re.sub(r'\bD([A-Z]+[A-Z0-9]*)\b', 
                         lambda m: target_precision + m.group(1), 
                         new_content)
    
    # Replace type declarations
    if target_precision in ['S', 'C']:
        new_content = new_content.replace('real64', PRECISION_MAP[target_precision]['kind'])
        new_content = new_content.replace('REAL(real64)', PRECISION_MAP[target_precision]['type'])
        new_content = new_content.replace('DOUBLE PRECISION', PRECISION_MAP[target_precision]['function_type'])
        
    elif target_precision == 'Z':
        new_content = new_content.replace('REAL(real64)', 'COMPLEX(real64)')
        new_content = new_content.replace('DOUBLE PRECISION', 'COMPLEX*16')
    
    # Replace type suffixes
    new_content = new_content.replace('sparse_coo_d', f'sparse_coo_{target_precision.lower()}')
    new_content = new_content.replace('sparse_csr_d', f'sparse_csr_{target_precision.lower()}')
    new_content = new_content.replace('sparse_csc_d', f'sparse_csc_{target_precision.lower()}')
    
    # Replace constants
    if target_precision != 'D':
        # Replace zero and one constants
        new_content = re.sub(r'0\.0_real64', PRECISION_MAP[target_precision]['zero'], new_content)
        new_content = re.sub(r'1\.0_real64', PRECISION_MAP[target_precision]['one'], new_content)
        
        # For complex types, handle negative one
        if target_precision in ['C', 'Z']:
            neg_one = PRECISION_MAP[target_precision]['one'].replace('1.0', '-1.0')
            new_content = re.sub(r'-1\.0_real64', neg_one, new_content)
    
    # Update documentation
    new_content = new_content.replace('double precision', PRECISION_MAP[target_precision]['desc'])
    new_content = new_content.replace('Double precision', PRECISION_MAP[target_precision]['desc'].capitalize())
    
    # For complex routines, we need to handle conjugate transpose
    if target_precision in ['C', 'Z']:
        # Add handling for conjugate transpose 'C' option
        new_content = new_content.replace(
            "LSAME(TRANS, 'T')",
            "(LSAME(TRANS, 'T') .OR. LSAME(TRANS, 'C'))"
        )
    
    # Write target file
    with open(target_path, 'w') as f:
        f.write(new_content)
    
    print(f"Created {target_filename}")
    return target_path

def main():
    """Main entry point."""
    
    if len(sys.argv) > 1:
        # Port specific files
        precision = sys.argv[1].upper()
        if precision not in ['S', 'C', 'Z']:
            print(f"Invalid precision: {precision}")
            sys.exit(1)
            
        files = sys.argv[2:] if len(sys.argv) > 2 else []
        
        if not files:
            # Find all D* files
            sparse_dir = Path(__file__).parent
            files = list(sparse_dir.rglob('D*.f90'))
        
        created_files = []
        for f in files:
            result = port_routine(str(f), precision)
            if result:
                created_files.append(result)
        
        print(f"\nCreated {len(created_files)} {precision}-precision files")
        
    else:
        print("Usage: python port_precision.py <precision> [files...]")
        print("  precision: S, C, or Z")
        print("  files: specific files to port (optional, defaults to all D* files)")

if __name__ == '__main__':
    main()