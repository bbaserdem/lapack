//! Benchmarks for different matrix types (dense, sparse, symmetric, triangular)

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use lapack::{Matrix, Vector, Layout};
use std::time::Duration;

/// Generate a dense matrix with random-like values
fn generate_dense_matrix(n: usize) -> Vec<f64> {
    (0..n*n).map(|i| ((i % 100) as f64 + 1.0) / 100.0).collect()
}

/// Generate a symmetric matrix
fn generate_symmetric_matrix(n: usize) -> Vec<f64> {
    let mut data = vec![0.0; n * n];
    for i in 0..n {
        for j in 0..=i {
            let value = ((i + j) % 100) as f64 / 100.0 + 1.0;
            data[i * n + j] = value;
            data[j * n + i] = value; // Symmetric
        }
    }
    data
}

/// Generate a positive definite symmetric matrix
fn generate_spd_matrix(n: usize) -> Vec<f64> {
    let mut data = vec![0.0; n * n];
    // Create A = I + 0.1 * ones (simple SPD matrix)
    for i in 0..n {
        for j in 0..n {
            data[i * n + j] = if i == j { 2.0 } else { 0.1 };
        }
    }
    data
}

/// Generate an upper triangular matrix
fn generate_upper_triangular(n: usize) -> Vec<f64> {
    let mut data = vec![0.0; n * n];
    for i in 0..n {
        for j in i..n {
            data[i * n + j] = ((i + j) % 100) as f64 / 100.0 + 1.0;
        }
    }
    data
}

/// Generate a lower triangular matrix
fn generate_lower_triangular(n: usize) -> Vec<f64> {
    let mut data = vec![0.0; n * n];
    for i in 0..n {
        for j in 0..=i {
            data[i * n + j] = ((i + j) % 100) as f64 / 100.0 + 1.0;
        }
    }
    data
}

/// Generate a banded matrix with specified bandwidth
fn generate_banded_matrix(n: usize, bandwidth: usize) -> Vec<f64> {
    let mut data = vec![0.0; n * n];
    for i in 0..n {
        for j in 0..n {
            if i.abs_diff(j) <= bandwidth {
                data[i * n + j] = ((i + j) % 100) as f64 / 100.0 + 1.0;
            }
        }
    }
    data
}

/// Benchmark operations on different matrix types
fn bench_matrix_types_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("matrix_types");
    group.measurement_time(Duration::from_secs(10));
    
    for size in [50, 100, 500].iter() {
        let n = *size;
        
        // Dense matrix operations
        let dense_data = generate_dense_matrix(n);
        group.bench_with_input(BenchmarkId::new("dense_access", n), &n, |b, &n| {
            let matrix = Matrix::from_vec(dense_data.clone(), n, n, Layout::ColumnMajor).unwrap();
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
        
        // Symmetric matrix operations
        let symmetric_data = generate_symmetric_matrix(n);
        group.bench_with_input(BenchmarkId::new("symmetric_access", n), &n, |b, &n| {
            let matrix = Matrix::from_vec(symmetric_data.clone(), n, n, Layout::ColumnMajor).unwrap();
            b.iter(|| {
                let mut sum = 0.0;
                // Only access upper triangle
                for i in 0..n {
                    for j in i..n {
                        sum += matrix.get(i, j).unwrap();
                    }
                }
                black_box(sum);
            });
        });
        
        // Upper triangular matrix operations
        let upper_tri_data = generate_upper_triangular(n);
        group.bench_with_input(BenchmarkId::new("upper_triangular_access", n), &n, |b, &n| {
            let matrix = Matrix::from_vec(upper_tri_data.clone(), n, n, Layout::ColumnMajor).unwrap();
            b.iter(|| {
                let mut sum = 0.0;
                for i in 0..n {
                    for j in i..n {
                        sum += matrix.get(i, j).unwrap();
                    }
                }
                black_box(sum);
            });
        });
        
        // Banded matrix operations
        let bandwidth = n / 10; // 10% bandwidth
        let banded_data = generate_banded_matrix(n, bandwidth);
        group.bench_with_input(BenchmarkId::new("banded_access", n), &n, |b, &n| {
            let matrix = Matrix::from_vec(banded_data.clone(), n, n, Layout::ColumnMajor).unwrap();
            b.iter(|| {
                let mut sum = 0.0;
                for i in 0..n {
                    let j_start = i.saturating_sub(bandwidth);
                    let j_end = (i + bandwidth + 1).min(n);
                    for j in j_start..j_end {
                        sum += matrix.get(i, j).unwrap();
                    }
                }
                black_box(sum);
            });
        });
    }
    
    group.finish();
}

/// Benchmark factorizations on different matrix types with lapacke
fn bench_matrix_types_factorizations(c: &mut Criterion) {
    let mut group = c.benchmark_group("matrix_types_factorizations");
    group.measurement_time(Duration::from_secs(10));
    
    for size in [50, 100, 500].iter() {
        let n = *size;
        
        // LU factorization on dense matrix
        let dense_data = generate_dense_matrix(n);
        group.bench_with_input(BenchmarkId::new("dense_lu", n), &n, |b, &n| {
            b.iter(|| {
                let mut a = dense_data.clone();
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
        
        // Cholesky factorization on SPD matrix
        let spd_data = generate_spd_matrix(n);
        group.bench_with_input(BenchmarkId::new("spd_cholesky", n), &n, |b, &n| {
            b.iter(|| {
                let mut a = spd_data.clone();
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
        
        // Triangular solve on upper triangular matrix
        let upper_tri_data = generate_upper_triangular(n);
        let b_data: Vec<f64> = (0..n).map(|i| i as f64 + 1.0).collect();
        group.bench_with_input(BenchmarkId::new("triangular_solve", n), &n, |b, &n| {
            b.iter(|| {
                let a = upper_tri_data.clone();
                let mut x = b_data.clone();
                unsafe {
                    lapacke::dtrsv(
                        lapacke::Layout::ColumnMajor,
                        b'U',
                        b'N',
                        b'N',
                        n as i32,
                        &a,
                        n as i32,
                        &mut x,
                        1,
                    );
                }
            });
        });
    }
    
    group.finish();
}

criterion_group!(
    benches,
    bench_matrix_types_operations,
    bench_matrix_types_factorizations
);
criterion_main!(benches);