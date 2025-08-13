#!/bin/bash
# Simple test workflow for fortran-mapper

echo "=== Testing fortran-mapper basic functionality ==="

# Test 1: Check available hooks
echo -e "\n1. Available hooks:"
fortran-mapper list-hooks

# Test 2: Parse a single BLAS file with LAPACK hooks
echo -e "\n2. Parsing dgemm.f with LAPACK hooks:"
fortran-mapper parse ../BLAS/SRC/dgemm.f \
    --hooks lapack \
    --json output/dgemm_lapack.json \
    --verbose

# Test 3: Check the output
echo -e "\n3. JSON output content:"
if [ -f output/dgemm_lapack.json ]; then
    python3 -c "import json; d=json.load(open('output/dgemm_lapack.json')); print(f'Nodes: {len(d[\"nodes\"])}, Relationships: {len(d[\"relationships\"])}')"
    echo "First few nodes:"
    cat output/dgemm_lapack.json | python3 -m json.tool | head -30
else
    echo "No JSON output generated!"
fi

# Test 4: Try parsing without hooks
echo -e "\n4. Parsing without hooks:"
fortran-mapper parse ../BLAS/SRC/daxpy.f \
    --json output/daxpy_no_hooks.json

# Check if fortran-src is the issue
echo -e "\n5. Checking fortran-src availability:"
which fortran-src || echo "fortran-src not found in PATH"

# Test 6: Parse multiple files
echo -e "\n6. Parsing multiple BLAS files:"
fortran-mapper parse ../BLAS/SRC/dgemm.f ../BLAS/SRC/daxpy.f ../BLAS/SRC/ddot.f \
    --hooks lapack \
    --json output/multiple_files.json

# Test 7: Show stats
echo -e "\n7. Statistics:"
fortran-mapper stats output/multiple_files.json