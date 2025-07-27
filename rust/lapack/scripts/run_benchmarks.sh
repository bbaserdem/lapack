#!/bin/bash
# Script to run LAPACK benchmarks and generate comparison report

set -e

echo "Running LAPACK Rust binding benchmarks..."
echo "========================================="

# Create output directory for results
RESULTS_DIR="benchmark_results"
mkdir -p $RESULTS_DIR

# Run the main benchmarks
echo "Running main LAPACK routine benchmarks..."
cargo bench --bench lapack_benchmarks -- --save-baseline lapack_main

# Run matrix type benchmarks  
echo "Running matrix type benchmarks..."
cargo bench --bench matrix_types_bench -- --save-baseline matrix_types

# Generate HTML reports
echo "Benchmark reports generated in target/criterion/"

# Create a summary report
echo "Creating summary report..."
cat > $RESULTS_DIR/benchmark_summary.md << EOF
# LAPACK Rust Bindings Benchmark Report

Generated on: $(date)

## Summary

This report compares the performance of our Rust LAPACK bindings against the existing lapacke crate.

### Benchmarks Performed

1. **Linear System Solvers (dgesv)**
   - Matrix sizes: 10x10, 50x50, 100x100, 500x500, 1000x1000
   - Compared our implementation vs lapacke crate

2. **LU Factorization (dgetrf)**
   - Matrix sizes: 10x10, 50x50, 100x100, 500x500, 1000x1000
   - Compared our implementation vs lapacke crate

3. **Cholesky Factorization (dpotrf)**
   - Matrix sizes: 10x10, 50x50, 100x100, 500x500, 1000x1000
   - Compared our implementation vs lapacke crate

4. **Matrix Layout Conversions**
   - Row-major to column-major and vice versa
   - Matrix sizes: 10x10, 100x100, 1000x1000

5. **Matrix Operations**
   - Creation, sequential access, random access
   - Comparison with ndarray

6. **Matrix Type Specific Operations**
   - Dense, symmetric, triangular, banded matrices
   - Various factorizations on different matrix types

### Results Location

Detailed results can be found in:
- HTML reports: \`target/criterion/report/index.html\`
- Raw data: \`target/criterion/\`

### Performance Analysis

To view detailed performance comparisons:
1. Open \`target/criterion/report/index.html\` in a web browser
2. Navigate through different benchmark groups
3. Compare "rust_binding" vs "lapacke_crate" results

### Next Steps

1. Profile any routines showing performance regressions
2. Optimize memory access patterns
3. Consider SIMD optimizations for critical paths
4. Implement missing LAPACK routines for complete comparison

EOF

echo "Summary report created in $RESULTS_DIR/benchmark_summary.md"
echo ""
echo "To view detailed results:"
echo "  firefox target/criterion/report/index.html"
echo ""
echo "To profile specific routines:"
echo "  perf record cargo bench --bench lapack_benchmarks <specific_benchmark>"
echo "  perf report"