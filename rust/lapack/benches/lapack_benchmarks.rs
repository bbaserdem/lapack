//! Performance benchmarks for LAPACK Rust bindings
//!
//! This benchmark suite compares the performance of our Rust LAPACK bindings
//! against the existing lapacke crate across various routine categories and
//! matrix sizes.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use lapack::{Matrix, Vector, Layout};
use ndarray::{Array2, Array1};
use std::time::Duration;

/// Benchmark group for linear system solvers (dgesv family)
fn bench_linear_systems(c: &mut Criterion) {
    let mut group = c.benchmark_group("linear_systems_dgesv");
    group.measurement_time(Duration::from_secs(10));
    
    // Test different matrix sizes
    for size in [10, 50, 100, 500, 1000].iter() {
        let n = *size;
        
        // Create a random system Ax = b
        let a_data: Vec<f64> = (0..n*n).map(|i| (i as f64 + 1.0) / (n as f64)).collect();
        let b_data: Vec<f64> = (0..n).map(|i| (i as f64 + 1.0)).collect();
        
        // Benchmark our implementation
        group.bench_with_input(BenchmarkId::new("rust_binding", n), &n, |b, &n| {
            b.iter(|| {
                let mut a = Matrix::from_vec(a_data.clone(), n, n, Layout::ColumnMajor).unwrap();
                let mut b_vec = Vector::from_vec(b_data.clone());
                
                // TODO: Call our dgesv implementation
                // For now, just do the setup to measure overhead
                black_box(&mut a);
                black_box(&mut b_vec);
            });
        });
        
        // Benchmark lapacke crate implementation
        group.bench_with_input(BenchmarkId::new("lapacke_crate", n), &n, |b, &n| {
            b.iter(|| {
                let mut a = a_data.clone();
                let mut b_vec = b_data.clone();
                let mut ipiv = vec![0i32; n];
                
                unsafe {
                    lapacke::dgesv(
                        lapacke::Layout::ColumnMajor,
                        n as i32,
                        1,
                        &mut a,
                        n as i32,
                        &mut ipiv,
                        &mut b_vec,
                        1,
                    );
                }
            });
        });
    }
    
    group.finish();
}

/// Benchmark group for LU factorization (dgetrf)
fn bench_lu_factorization(c: &mut Criterion) {
    let mut group = c.benchmark_group("lu_factorization_dgetrf");
    group.measurement_time(Duration::from_secs(10));
    
    for size in [10, 50, 100, 500, 1000].iter() {
        let n = *size;
        
        // Create a random matrix
        let a_data: Vec<f64> = (0..n*n).map(|i| (i as f64 + 1.0) / (n as f64)).collect();
        
        // Benchmark our implementation
        group.bench_with_input(BenchmarkId::new("rust_binding", n), &n, |b, &n| {
            b.iter(|| {
                let mut a = Matrix::from_vec(a_data.clone(), n, n, Layout::ColumnMajor).unwrap();
                
                // TODO: Call our dgetrf implementation
                black_box(&mut a);
            });
        });
        
        // Benchmark lapacke crate implementation
        group.bench_with_input(BenchmarkId::new("lapacke_crate", n), &n, |b, &n| {
            b.iter(|| {
                let mut a = a_data.clone();
                let mut ipiv = vec![0i32; n];
                
                unsafe {
                    lapacke::dgetrf(
                        lapacke::Layout::ColumnMajor,
                        n as i32,
                        n as i32,
                        &mut a,
                        n as i32,
                        &mut ipiv,
                    );
                }
            });
        });
    }
    
    group.finish();
}

/// Benchmark group for Cholesky factorization (dpotrf)
fn bench_cholesky_factorization(c: &mut Criterion) {
    let mut group = c.benchmark_group("cholesky_factorization_dpotrf");
    group.measurement_time(Duration::from_secs(10));
    
    for size in [10, 50, 100, 500, 1000].iter() {
        let n = *size;
        
        // Create a symmetric positive definite matrix
        let mut a_data = vec![0.0f64; n * n];
        // Simple SPD matrix: A = I + 0.1 * ones
        for i in 0..n {
            for j in 0..n {
                a_data[j * n + i] = if i == j { 1.1 } else { 0.1 };
            }
        }
        
        // Benchmark our implementation
        group.bench_with_input(BenchmarkId::new("rust_binding", n), &n, |b, &n| {
            b.iter(|| {
                let mut a = Matrix::from_vec(a_data.clone(), n, n, Layout::ColumnMajor).unwrap();
                
                // TODO: Call our dpotrf implementation
                black_box(&mut a);
            });
        });
        
        // Benchmark lapacke crate implementation
        group.bench_with_input(BenchmarkId::new("lapacke_crate", n), &n, |b, &n| {
            b.iter(|| {
                let mut a = a_data.clone();
                
                unsafe {
                    lapacke::dpotrf(
                        lapacke::Layout::ColumnMajor,
                        b'U',
                        n as i32,
                        &mut a,
                        n as i32,
                    );
                }
            });
        });
    }
    
    group.finish();
}

/// Benchmark matrix layout conversions
fn bench_layout_conversions(c: &mut Criterion) {
    let mut group = c.benchmark_group("layout_conversions");
    
    for size in [10, 100, 1000].iter() {
        let n = *size;
        let data: Vec<f64> = (0..n*n).map(|i| i as f64).collect();
        
        // Row-major to column-major
        group.bench_with_input(BenchmarkId::new("row_to_col", n), &n, |b, &n| {
            let matrix = Matrix::from_vec(data.clone(), n, n, Layout::RowMajor).unwrap();
            b.iter(|| {
                let col_major = matrix.to_column_major();
                black_box(col_major);
            });
        });
        
        // Column-major to row-major
        group.bench_with_input(BenchmarkId::new("col_to_row", n), &n, |b, &n| {
            let matrix = Matrix::from_vec(data.clone(), n, n, Layout::ColumnMajor).unwrap();
            b.iter(|| {
                let row_major = matrix.to_row_major();
                black_box(row_major);
            });
        });
    }
    
    group.finish();
}

/// Benchmark matrix creation and access patterns
fn bench_matrix_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("matrix_operations");
    
    for size in [10, 100, 1000].iter() {
        let n = *size;
        
        // Matrix creation from vec
        group.bench_with_input(BenchmarkId::new("create_from_vec", n), &n, |b, &n| {
            let data: Vec<f64> = (0..n*n).map(|i| i as f64).collect();
            b.iter(|| {
                let matrix = Matrix::from_vec(data.clone(), n, n, Layout::ColumnMajor).unwrap();
                black_box(matrix);
            });
        });
        
        // Element access (sequential)
        group.bench_with_input(BenchmarkId::new("sequential_access", n), &n, |b, &n| {
            let data: Vec<f64> = (0..n*n).map(|i| i as f64).collect();
            let matrix = Matrix::from_vec(data, n, n, Layout::ColumnMajor).unwrap();
            b.iter(|| {
                let mut sum = 0.0;
                for i in 0..n {
                    for j in 0..n {
                        sum += matrix.get(i, j).unwrap();
                    }
                }
                black_box(sum);
            });
        });
        
        // Element access (random)
        group.bench_with_input(BenchmarkId::new("random_access", n), &n, |b, &n| {
            let data: Vec<f64> = (0..n*n).map(|i| i as f64).collect();
            let matrix = Matrix::from_vec(data, n, n, Layout::ColumnMajor).unwrap();
            b.iter(|| {
                let mut sum = 0.0;
                // Access in a pseudo-random pattern
                for k in 0..n*n {
                    let i = (k * 7) % n;
                    let j = (k * 13) % n;
                    sum += matrix.get(i, j).unwrap();
                }
                black_box(sum);
            });
        });
    }
    
    group.finish();
}

/// Benchmark comparison with ndarray for basic operations
fn bench_vs_ndarray(c: &mut Criterion) {
    let mut group = c.benchmark_group("vs_ndarray");
    
    for size in [10, 100, 1000].iter() {
        let n = *size;
        let data: Vec<f64> = (0..n*n).map(|i| i as f64).collect();
        
        // Our matrix creation
        group.bench_with_input(BenchmarkId::new("our_matrix_create", n), &n, |b, &n| {
            b.iter(|| {
                let matrix = Matrix::from_vec(data.clone(), n, n, Layout::ColumnMajor).unwrap();
                black_box(matrix);
            });
        });
        
        // ndarray creation
        group.bench_with_input(BenchmarkId::new("ndarray_create", n), &n, |b, &n| {
            b.iter(|| {
                let array = Array2::from_shape_vec((n, n), data.clone()).unwrap();
                black_box(array);
            });
        });
    }
    
    group.finish();
}

criterion_group!(
    benches,
    bench_linear_systems,
    bench_lu_factorization,
    bench_cholesky_factorization,
    bench_layout_conversions,
    bench_matrix_operations,
    bench_vs_ndarray
);
criterion_main!(benches);