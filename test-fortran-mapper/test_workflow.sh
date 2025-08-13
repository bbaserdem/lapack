#!/bin/bash
# Test workflow for fortran-mapper with LAPACK code

echo "=== Testing fortran-mapper with LAPACK subset ==="
echo

# Set test directories
LAPACK_DIR="../BLAS/SRC"
OUTPUT_DIR="output"

# Create output directory
mkdir -p $OUTPUT_DIR

# Test 1: Basic parsing with a few BLAS routines
echo "Test 1: Parse core BLAS level 3 routines (dgemm, dtrsm, dsyrk)"
fortran-mapper parse $LAPACK_DIR/dgemm.f $LAPACK_DIR/dtrsm.f $LAPACK_DIR/dsyrk.f \
    --json $OUTPUT_DIR/blas_level3.json \
    --verbose

echo
echo "Test 2: Parse with LAPACK hooks enabled"
fortran-mapper parse $LAPACK_DIR/dgemm.f $LAPACK_DIR/dtrsm.f \
    --hooks lapack \
    --json $OUTPUT_DIR/blas_with_hooks.json \
    --verbose

echo
echo "Test 3: Export to DOT format for visualization"
fortran-mapper parse $LAPACK_DIR/dgemm.f $LAPACK_DIR/daxpy.f $LAPACK_DIR/ddot.f \
    --hooks lapack \
    --dot $OUTPUT_DIR/blas_callgraph.dot \
    --call-graph-only

echo
echo "Test 4: Parse a small subset of files"
# Create temp directory with just a few files
mkdir -p temp_subset
cp $LAPACK_DIR/dgemm.f $LAPACK_DIR/daxpy.f $LAPACK_DIR/ddot.f $LAPACK_DIR/dnrm2.f temp_subset/
fortran-mapper parse temp_subset \
    --hooks lapack \
    --json $OUTPUT_DIR/blas_subset.json
rm -rf temp_subset

echo
echo "Test 5: Show statistics"
if [ -f $OUTPUT_DIR/blas_subset.json ]; then
    fortran-mapper stats $OUTPUT_DIR/blas_subset.json
fi

echo
echo "Test 6: List available hooks"
fortran-mapper list-hooks

echo
echo "=== Testing complete! Check output directory for results ==="
ls -la $OUTPUT_DIR/