#!/bin/bash
# Script to profile LAPACK benchmarks and identify bottlenecks

set -e

echo "Profiling LAPACK Rust binding benchmarks..."
echo "=========================================="

# Check if perf is available
if ! command -v perf &> /dev/null; then
    echo "Error: 'perf' command not found. Please install perf tools."
    echo "On Ubuntu/Debian: sudo apt-get install linux-tools-common linux-tools-generic"
    echo "On Fedora: sudo dnf install perf"
    exit 1
fi

PROFILE_DIR="profile_results"
mkdir -p $PROFILE_DIR

# Function to run perf on a specific benchmark
profile_benchmark() {
    local bench_name=$1
    local filter=$2
    local output_name=$3
    
    echo "Profiling $bench_name with filter: $filter"
    
    # Record performance data
    perf record -g --call-graph=dwarf -o $PROFILE_DIR/$output_name.data \
        cargo bench --bench $bench_name -- $filter --profile-time 10
    
    # Generate report
    perf report -i $PROFILE_DIR/$output_name.data > $PROFILE_DIR/$output_name.txt
    
    # Generate flamegraph if available
    if command -v perf script &> /dev/null && command -v stackcollapse-perf.pl &> /dev/null; then
        perf script -i $PROFILE_DIR/$output_name.data | \
            stackcollapse-perf.pl | \
            flamegraph.pl > $PROFILE_DIR/$output_name.svg
        echo "Flamegraph generated: $PROFILE_DIR/$output_name.svg"
    fi
}

# Profile specific routines
echo ""
echo "1. Profiling matrix layout conversions..."
profile_benchmark "lapack_benchmarks" "layout_conversions/col_to_row/1000" "layout_conversion"

echo ""
echo "2. Profiling matrix element access patterns..."
profile_benchmark "lapack_benchmarks" "matrix_operations/random_access/1000" "matrix_access"

echo ""
echo "3. Profiling LU factorization..."
profile_benchmark "lapack_benchmarks" "lu_factorization_dgetrf/lapacke_crate/1000" "lu_factorization"

# Generate bottleneck analysis report
echo ""
echo "Generating bottleneck analysis..."
cat > $PROFILE_DIR/bottleneck_analysis.md << EOF
# Performance Bottleneck Analysis

Generated on: $(date)

## Profile Results

### 1. Matrix Layout Conversions
- Profile data: layout_conversion.data
- Key observations from profiling will be added here after manual analysis

### 2. Matrix Element Access
- Profile data: matrix_access.data  
- Key observations from profiling will be added here after manual analysis

### 3. LU Factorization
- Profile data: lu_factorization.data
- Key observations from profiling will be added here after manual analysis

## Common Bottlenecks to Check

1. **Memory Access Patterns**
   - Cache misses due to non-contiguous access
   - Row vs column major layout efficiency

2. **Bounds Checking**
   - Overhead from Result<> returns
   - Unnecessary validation in hot loops

3. **Memory Allocation**
   - Frequent allocations in conversion functions
   - Vector resizing

4. **Function Call Overhead**
   - Inline candidates
   - Generic function monomorphization

## Optimization Recommendations

Based on the profile results, consider:

1. Using unsafe blocks for hot paths with proper validation at boundaries
2. Implementing SIMD operations for vectorizable loops
3. Optimizing memory layout for cache efficiency
4. Pre-allocating buffers for temporary operations

## How to View Results

1. Text reports: \`cat $PROFILE_DIR/*.txt\`
2. Interactive: \`perf report -i $PROFILE_DIR/<name>.data\`
3. Flamegraphs: Open \`$PROFILE_DIR/*.svg\` in a browser

EOF

echo ""
echo "Bottleneck analysis report created in $PROFILE_DIR/bottleneck_analysis.md"
echo ""
echo "To view interactive profile results:"
echo "  perf report -i $PROFILE_DIR/<profile_name>.data"
echo ""
echo "For more detailed analysis:"
echo "  perf annotate -i $PROFILE_DIR/<profile_name>.data <function_name>"