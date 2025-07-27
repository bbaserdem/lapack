#!/usr/bin/env python3
"""
Test script to parse fortran-src output and extract procedure/call information
for Neo4j import
"""

import subprocess
import re
import json

def parse_fortran_file(filepath):
    """Parse a Fortran file using fortran-src and extract key information"""
    
    # Run fortran-src
    cmd = ['fortran-src', '-a', 'parse', filepath]
    try:
        result = subprocess.run(cmd, capture_output=True, text=True)
        if result.returncode != 0:
            print(f"Error parsing {filepath}: {result.stderr}")
            return None
        
        output = result.stdout
        
        # Extract procedure name from PUSubroutine or PUFunction
        # Look for patterns like: PUSubroutine ... "DGEMM"
        proc_match = re.search(r'PU(?:Subroutine|Function).*?"(\w+)"', output)
        procedure_name = proc_match.group(1) if proc_match else None
        
        # Extract calls - look for StCall patterns with procedure names
        # This is simplified - real implementation would parse the AST properly
        calls = re.findall(r'StCall.*?"(\w+)"', output)
        
        return {
            'file': filepath,
            'procedure': procedure_name,
            'calls': list(set(calls))  # unique calls
        }
        
    except Exception as e:
        print(f"Exception parsing {filepath}: {e}")
        return None

# Test with a sample file
if __name__ == "__main__":
    # Test with BLAS dgemm
    result = parse_fortran_file('BLAS/SRC/dgemm.f')
    if result:
        print(json.dumps(result, indent=2))
    
    # Test with LAPACK routine that calls BLAS
    result2 = parse_fortran_file('SRC/dgetrf.f')
    if result2:
        print("\n" + json.dumps(result2, indent=2))