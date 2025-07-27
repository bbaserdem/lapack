# LAPACK Rust Bindings Benchmarks

This directory contains comprehensive performance benchmarks for the LAPACK Rust bindings, comparing them against the existing `lapacke` crate.

## Benchmark Structure

### Main Benchmarks (`lapack_benchmarks.rs`)
- **Linear System Solvers**: Benchmarks for `dgesv` family
- **LU Factorization**: Performance tests for `dgetrf`
- **Cholesky Factorization**: Tests for `dpotrf` on SPD matrices
- **Matrix Layout Conversions**: Row-major ↔ Column-major conversions
- **Matrix Operations**: Creation, access patterns, basic operations
- **ndarray Comparison**: Performance comparison with the ndarray crate

### Matrix Type Benchmarks (`matrix_types_bench.rs`)
- Tests on different matrix structures:
  - Dense matrices
  - Symmetric matrices
  - Triangular matrices (upper/lower)
  - Banded matrices
- Type-specific operations and factorizations

## Running Benchmarks

### Quick Start
```bash
# Run all benchmarks
./scripts/run_benchmarks.sh

# Run specific benchmark suite
cargo bench --bench lapack_benchmarks

# Run specific test within a suite
cargo bench --bench lapack_benchmarks linear_systems_dgesv
```

### Profiling
```bash
# Profile benchmarks to identify bottlenecks
./scripts/profile_benchmarks.sh

# View profiling results
perf report -i profile_results/layout_conversion.data
```

## Matrix Sizes Tested
- Small: 10×10
- Medium: 50×50, 100×100  
- Large: 500×500, 1000×1000

## Viewing Results

### HTML Reports
After running benchmarks, view detailed HTML reports:
```bash
firefox target/criterion/report/index.html
```

### Performance Metrics
The benchmarks measure:
- Execution time (mean, median, std deviation)
- Throughput (operations/second)
- Memory access patterns
- Cache efficiency

## Interpreting Results

### Performance Comparison
Look for "rust_binding" vs "lapacke_crate" in the benchmark names:
- **rust_binding**: Our implementation
- **lapacke_crate**: Reference implementation
- **ndarray**: For basic operation comparisons

### Expected Performance
- Matrix operations should be within 10% of lapacke performance
- Layout conversions may be slower due to safety checks
- Small matrices might show overhead from our abstraction layer

## Adding New Benchmarks

To add a new benchmark:

1. Add the routine to the appropriate benchmark file
2. Follow the naming convention: `bench_<category>_<routine>`
3. Include multiple matrix sizes
4. Compare against lapacke when possible

Example:
```rust
fn bench_new_routine(c: &mut Criterion) {
    let mut group = c.benchmark_group("new_routine");
    
    for size in [10, 100, 1000].iter() {
        // Benchmark our implementation
        group.bench_with_input(BenchmarkId::new("rust_binding", size), ...);
        
        // Benchmark lapacke
        group.bench_with_input(BenchmarkId::new("lapacke_crate", size), ...);
    }
}
```

## Performance Optimization Guide

Based on benchmark results, common optimization targets:

1. **Memory Layout**: Ensure data locality for cache efficiency
2. **Bounds Checking**: Use unsafe blocks judiciously in hot paths
3. **Allocation**: Pre-allocate working memory when possible
4. **SIMD**: Leverage auto-vectorization for simple operations

## CI Integration

These benchmarks can be integrated into CI to catch performance regressions:

```yaml
# Example GitHub Actions step
- name: Run benchmarks
  run: |
    cargo bench --bench lapack_benchmarks -- --save-baseline main
    cargo bench --bench lapack_benchmarks -- --baseline main
```