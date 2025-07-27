#!/bin/bash
# Basic test runner for sparse matrix routines

echo "========================================="
echo "Running Basic Sparse Matrix Tests"
echo "========================================="

# Check if we're in the build directory
if [ ! -f "liblapack_sparse.a" ]; then
    echo "Error: liblapack_sparse.a not found. Are you in the build directory?"
    exit 1
fi

# Run conversion tests
echo ""
echo "Running conversion tests..."
if [ -x "TESTING/test_conversions" ]; then
    ./TESTING/test_conversions
    if [ $? -eq 0 ]; then
        echo "✓ Conversion tests passed"
    else
        echo "✗ Conversion tests failed"
    fi
else
    echo "✗ test_conversions not found or not executable"
fi

# Run SpMV tests
echo ""
echo "Running SpMV tests..."
if [ -x "TESTING/test_spmv" ]; then
    ./TESTING/test_spmv
    if [ $? -eq 0 ]; then
        echo "✓ SpMV tests passed"
    else
        echo "✗ SpMV tests failed"
    fi
else
    echo "✗ test_spmv not found or not executable"
fi

# Run matrix pattern tests
echo ""
echo "Running matrix pattern tests..."
if [ -x "TESTING/test_matrices" ]; then
    ./TESTING/test_matrices
    if [ $? -eq 0 ]; then
        echo "✓ Matrix pattern tests passed"
    else
        echo "✗ Matrix pattern tests failed"
    fi
else
    echo "✗ test_matrices not found or not executable"
fi

# Run I/O tests
echo ""
echo "Running I/O tests..."
if [ -x "TESTING/test_io" ]; then
    ./TESTING/test_io
    if [ $? -eq 0 ]; then
        echo "✓ I/O tests passed"
    else
        echo "✗ I/O tests failed"
    fi
else
    echo "✗ test_io not found or not executable"
fi

echo ""
echo "========================================="
echo "Basic tests completed"
echo "========================================="