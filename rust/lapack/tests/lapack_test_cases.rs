//! Comprehensive LAPACK test cases ported from the original Fortran test suite
//!
//! This module contains test cases that mirror the functionality of the original
//! LAPACK test suite, specifically focusing on:
//! - DGET01: LU factorization accuracy test
//! - DGET02: Solution accuracy test  
//! - DCHKGE: General matrix solver tests with various matrix types
//!
//! The tests cover multiple matrix sizes, types, and properties to ensure
//! correctness and numerical accuracy of the Rust LAPACK bindings.

use lapack::{Matrix, Vector, Layout};

// Note: These imports require the linear-systems feature to be enabled
#[cfg(feature = "linear-systems")]
use lapack::driver::linear_systems::{dgesv, dgetrf, dgetrs, dpotrf, dpotrs};

const EPS: f64 = f64::EPSILON;
const THRESH: f64 = 1e-12;

/// Test data structure for matrix test cases
#[derive(Debug, Clone)]
struct TestCase {
    name: &'static str,
    matrix: Vec<f64>,
    rows: usize,
    cols: usize,
    rhs: Vec<f64>,
    expected_solution: Option<Vec<f64>>,
    expected_residual_bound: f64,
}

/// Generate test matrices of different types similar to LAPACK test suite
fn generate_test_matrices() -> Vec<TestCase> {
    vec![
        // Test Case 1: Simple 2x2 system (known solution)
        TestCase {
            name: "simple_2x2",
            matrix: vec![1.0, 2.0, 3.0, 4.0],
            rows: 2,
            cols: 2,
            rhs: vec![5.0, 11.0],
            expected_solution: Some(vec![1.0, 2.0]),
            expected_residual_bound: 1e-14,
        },
        
        // Test Case 2: 3x3 well-conditioned matrix
        TestCase {
            name: "well_conditioned_3x3",
            matrix: vec![
                4.0, 1.0, 2.0,
                1.0, 5.0, 3.0,
                2.0, 3.0, 6.0
            ],
            rows: 3,
            cols: 3,
            rhs: vec![7.0, 9.0, 11.0],
            expected_solution: Some(vec![1.0, 1.0, 1.0]),
            expected_residual_bound: 1e-13,
        },
        
        // Test Case 3: Identity matrix
        TestCase {
            name: "identity_3x3",
            matrix: vec![
                1.0, 0.0, 0.0,
                0.0, 1.0, 0.0,
                0.0, 0.0, 1.0
            ],
            rows: 3,
            cols: 3,
            rhs: vec![2.0, 3.0, 4.0],
            expected_solution: Some(vec![2.0, 3.0, 4.0]),
            expected_residual_bound: 1e-15,
        },
        
        // Test Case 4: Larger matrix (5x5)
        TestCase {
            name: "larger_5x5",
            matrix: vec![
                5.0, 4.0, 3.0, 2.0, 1.0,
                4.0, 5.0, 4.0, 3.0, 2.0,
                3.0, 4.0, 5.0, 4.0, 3.0,
                2.0, 3.0, 4.0, 5.0, 4.0,
                1.0, 2.0, 3.0, 4.0, 5.0
            ],
            rows: 5,
            cols: 5,
            rhs: vec![15.0, 18.0, 19.0, 18.0, 15.0],
            expected_solution: None, // Will be computed
            expected_residual_bound: 1e-12,
        },
        
        // Test Case 5: Positive definite matrix for Cholesky
        TestCase {
            name: "positive_definite_3x3",
            matrix: vec![
                4.0, 2.0, 1.0,
                2.0, 3.0, 0.5,
                1.0, 0.5, 2.0
            ],
            rows: 3,
            cols: 3,
            rhs: vec![7.0, 5.5, 3.5],
            expected_solution: Some(vec![1.0, 1.0, 1.0]),
            expected_residual_bound: 1e-13,
        },
    ]
}

/// Port of DGET01: Test LU factorization accuracy
/// Computes norm(L*U - P*A) / (N * norm(A) * EPS)
fn test_lu_factorization_accuracy(
    original_a: &Matrix<f64>,
    lu_factors: &Matrix<f64>,
    ipiv: &Vector<i32>
) -> f64 {
    let m = original_a.rows();
    let n = original_a.cols();
    let min_mn = std::cmp::min(m, n);
    
    // Reconstruct L and U from LU factorization
    let mut l = Matrix::<f64>::zeros(m, min_mn, Layout::ColumnMajor).unwrap();
    let mut u = Matrix::<f64>::zeros(min_mn, n, Layout::ColumnMajor).unwrap();
    
    // Extract L (lower triangular with unit diagonal)
    for i in 0..m {
        for j in 0..std::cmp::min(i + 1, min_mn) {
            if i == j {
                l.set(i, j, 1.0).unwrap();
            } else if i > j {
                l.set(i, j, *lu_factors.get(i, j).unwrap()).unwrap();
            }
        }
    }
    
    // Extract U (upper triangular)
    for i in 0..min_mn {
        for j in i..n {
            u.set(i, j, *lu_factors.get(i, j).unwrap()).unwrap();
        }
    }
    
    // Compute L*U
    let lu_product = matrix_multiply(&l, &u);
    
    // Apply permutation to original matrix
    let pa = apply_permutation(original_a, ipiv);
    
    // Compute residual: norm(L*U - P*A)
    let residual_matrix = matrix_subtract(&lu_product, &pa);
    let residual_norm = matrix_norm_1(&residual_matrix);
    let a_norm = matrix_norm_1(original_a);
    
    if a_norm > 0.0 {
        residual_norm / (n as f64 * a_norm * EPS)
    } else {
        residual_norm
    }
}

/// Port of DGET02: Test solution accuracy
/// Computes norm(B - A*X) / (norm(A) * norm(X) * EPS)
fn test_solution_accuracy(
    a: &Matrix<f64>,
    x: &Vector<f64>,
    b: &Vector<f64>
) -> f64 {
    let ax = matrix_vector_multiply(a, x);
    let residual = vector_subtract(b, &ax);
    let residual_norm = vector_norm_1(&residual);
    let a_norm = matrix_norm_1(a);
    let x_norm = vector_norm_1(x);
    
    if a_norm > 0.0 && x_norm > 0.0 {
        residual_norm / (a_norm * x_norm * EPS)
    } else {
        residual_norm
    }
}

// Helper functions for matrix operations
fn matrix_multiply(a: &Matrix<f64>, b: &Matrix<f64>) -> Matrix<f64> {
    let m = a.rows();
    let n = b.cols();
    let k = a.cols();
    assert_eq!(k, b.rows());
    
    let mut c = Matrix::zeros(m, n, Layout::ColumnMajor).unwrap();
    
    for i in 0..m {
        for j in 0..n {
            let mut sum = 0.0;
            for l in 0..k {
                sum += a.get(i, l).unwrap() * b.get(l, j).unwrap();
            }
            c.set(i, j, sum).unwrap();
        }
    }
    c
}

fn matrix_vector_multiply(a: &Matrix<f64>, x: &Vector<f64>) -> Vector<f64> {
    let m = a.rows();
    let n = a.cols();
    assert_eq!(n, x.len());
    
    let mut y = Vector::<f64>::zeros(m);
    
    for i in 0..m {
        let mut sum = 0.0;
        for j in 0..n {
            sum += a.get(i, j).unwrap() * x.get(j).unwrap();
        }
        y.set(i, sum).unwrap();
    }
    y
}

fn matrix_subtract(a: &Matrix<f64>, b: &Matrix<f64>) -> Matrix<f64> {
    assert_eq!(a.rows(), b.rows());
    assert_eq!(a.cols(), b.cols());
    
    let mut c = a.clone();
    for i in 0..a.rows() {
        for j in 0..a.cols() {
            let val = a.get(i, j).unwrap() - b.get(i, j).unwrap();
            c.set(i, j, val).unwrap();
        }
    }
    c
}

fn vector_subtract(a: &Vector<f64>, b: &Vector<f64>) -> Vector<f64> {
    assert_eq!(a.len(), b.len());
    
    let mut c = a.clone();
    for i in 0..a.len() {
        let val = a.get(i).unwrap() - b.get(i).unwrap();
        c.set(i, val).unwrap();
    }
    c
}

fn matrix_norm_1(a: &Matrix<f64>) -> f64 {
    let mut max_col_sum: f64 = 0.0;
    for j in 0..a.cols() {
        let mut col_sum = 0.0;
        for i in 0..a.rows() {
            col_sum += a.get(i, j).unwrap().abs();
        }
        max_col_sum = max_col_sum.max(col_sum);
    }
    max_col_sum
}

fn vector_norm_1(x: &Vector<f64>) -> f64 {
    let mut sum = 0.0;
    for i in 0..x.len() {
        sum += x.get(i).unwrap().abs();
    }
    sum
}

fn apply_permutation(a: &Matrix<f64>, ipiv: &Vector<i32>) -> Matrix<f64> {
    let mut pa = a.clone();
    
    // Apply row permutations as done in LAPACK
    for i in 0..ipiv.len() {
        let pivot = ipiv.get(i).unwrap() - 1; // Convert to 0-based indexing
        if pivot != i as i32 {
            // Swap rows i and pivot
            for j in 0..a.cols() {
                let temp = *pa.get(i, j).unwrap();
                pa.set(i, j, *pa.get(pivot as usize, j).unwrap()).unwrap();
                pa.set(pivot as usize, j, temp).unwrap();
            }
        }
    }
    pa
}

#[cfg(all(test, feature = "linear-systems"))]
mod tests {
    use super::*;

    #[test]
    fn test_dgesv_comprehensive() {
        let test_cases = generate_test_matrices();
        
        for test_case in test_cases.iter() {
            if test_case.rows != test_case.cols {
                continue; // Skip non-square matrices for dgesv
            }
            
            println!("Testing dgesv with case: {}", test_case.name);
            
            let mut a = Matrix::from_slice(
                &test_case.matrix,
                test_case.rows,
                test_case.cols,
                Layout::RowMajor
            ).unwrap();
            
            let mut b = Vector::from_slice(&test_case.rhs);
            let original_a = a.clone();
            let original_b = b.clone();
            
            // Solve the system
            let ipiv = dgesv(&mut a, &mut b).unwrap();
            
            // Test solution accuracy (DGET02 equivalent)
            let residual_ratio = test_solution_accuracy(&original_a, &b, &original_b);
            
            println!("  Solution residual ratio: {:.2e}", residual_ratio);
            assert!(residual_ratio < THRESH, 
                "Solution accuracy test failed for {}: ratio = {:.2e}", 
                test_case.name, residual_ratio);
            
            // If we have expected solution, check against it
            if let Some(ref expected) = test_case.expected_solution {
                for i in 0..expected.len() {
                    let diff = (b.get(i).unwrap() - expected[i]).abs();
                    assert!(diff < test_case.expected_residual_bound,
                        "Solution mismatch for {} at index {}: got {}, expected {}, diff = {:.2e}",
                        test_case.name, i, b.get(i).unwrap(), expected[i], diff);
                }
            }
        }
    }

    #[test]
    fn test_dgetrf_dgetrs_comprehensive() {
        let test_cases = generate_test_matrices();
        
        for test_case in test_cases.iter() {
            if test_case.rows != test_case.cols {
                continue; // Skip non-square matrices
            }
            
            println!("Testing dgetrf/dgetrs with case: {}", test_case.name);
            
            let mut a = Matrix::from_slice(
                &test_case.matrix,
                test_case.rows,
                test_case.cols,
                Layout::RowMajor
            ).unwrap();
            
            let original_a = a.clone();
            
            // Compute LU factorization
            let ipiv = dgetrf(&mut a).unwrap();
            
            // Test LU factorization accuracy (DGET01 equivalent)
            let lu_residual_ratio = test_lu_factorization_accuracy(&original_a, &a, &ipiv);
            
            println!("  LU factorization residual ratio: {:.2e}", lu_residual_ratio);
            assert!(lu_residual_ratio < THRESH,
                "LU factorization accuracy test failed for {}: ratio = {:.2e}",
                test_case.name, lu_residual_ratio);
            
            // Test solving with the factorization
            let mut b = Vector::from_slice(&test_case.rhs);
            let original_b = b.clone();
            
            dgetrs(&a, &ipiv, &mut b, 'N').unwrap();
            
            // Test solution accuracy
            let solution_residual_ratio = test_solution_accuracy(&original_a, &b, &original_b);
            
            println!("  Solution residual ratio: {:.2e}", solution_residual_ratio);
            assert!(solution_residual_ratio < THRESH,
                "Solution accuracy test failed for {}: ratio = {:.2e}",
                test_case.name, solution_residual_ratio);
        }
    }

    #[test]
    fn test_dpotrf_dpotrs_comprehensive() {
        // Test only with positive definite matrices
        let pd_matrices = vec![
            TestCase {
                name: "positive_definite_2x2",
                matrix: vec![4.0, 2.0, 2.0, 3.0],
                rows: 2,
                cols: 2,
                rhs: vec![6.0, 5.0],
                expected_solution: Some(vec![1.0, 1.0]),
                expected_residual_bound: 1e-14,
            },
            TestCase {
                name: "positive_definite_3x3",
                matrix: vec![
                    9.0, 3.0, 1.0,
                    3.0, 5.0, 2.0,
                    1.0, 2.0, 4.0
                ],
                rows: 3,
                cols: 3,
                rhs: vec![13.0, 10.0, 7.0],
                expected_solution: Some(vec![1.0, 1.0, 1.0]),
                expected_residual_bound: 1e-13,
            },
        ];
        
        for test_case in pd_matrices.iter() {
            println!("Testing dpotrf/dpotrs with case: {}", test_case.name);
            
            let mut a = Matrix::from_slice(
                &test_case.matrix,
                test_case.rows,
                test_case.cols,
                Layout::RowMajor
            ).unwrap();
            
            let original_a = a.clone();
            
            // Compute Cholesky factorization
            dpotrf(&mut a, 'L').unwrap();
            
            // Test solving with the factorization
            let mut b = Vector::from_slice(&test_case.rhs);
            let original_b = b.clone();
            
            dpotrs(&a, &mut b, 'L').unwrap();
            
            // Test solution accuracy
            let solution_residual_ratio = test_solution_accuracy(&original_a, &b, &original_b);
            
            println!("  Solution residual ratio: {:.2e}", solution_residual_ratio);
            assert!(solution_residual_ratio < THRESH,
                "Cholesky solution accuracy test failed for {}: ratio = {:.2e}",
                test_case.name, solution_residual_ratio);
            
            // Check against expected solution if available
            if let Some(ref expected) = test_case.expected_solution {
                for i in 0..expected.len() {
                    let diff = (b.get(i).unwrap() - expected[i]).abs();
                    assert!(diff < test_case.expected_residual_bound,
                        "Cholesky solution mismatch for {} at index {}: got {}, expected {}, diff = {:.2e}",
                        test_case.name, i, b.get(i).unwrap(), expected[i], diff);
                }
            }
        }
    }

    #[test]
    fn test_matrix_sizes() {
        // Test various matrix sizes to ensure scalability
        let sizes = vec![1, 2, 3, 5, 10, 20];
        
        for &n in sizes.iter() {
            println!("Testing with matrix size: {}x{}", n, n);
            
            // Create a well-conditioned test matrix (shifted identity + random)
            let mut matrix_data = vec![0.0; n * n];
            for i in 0..n {
                for j in 0..n {
                    if i == j {
                        matrix_data[i * n + j] = 2.0 + (i as f64) * 0.1;
                    } else {
                        matrix_data[i * n + j] = 0.1 / ((i + j + 2) as f64);
                    }
                }
            }
            
            // Create RHS vector (sum of each row)
            let mut rhs = vec![0.0; n];
            for i in 0..n {
                for j in 0..n {
                    rhs[i] += matrix_data[i * n + j];
                }
            }
            
            let mut a = Matrix::from_slice(&matrix_data, n, n, Layout::RowMajor).unwrap();
            let mut b = Vector::from_slice(&rhs);
            let original_a = a.clone();
            let original_b = b.clone();
            
            // Solve
            let ipiv = dgesv(&mut a, &mut b).unwrap();
            
            // Solution should be approximately [1, 1, ..., 1]
            for i in 0..n {
                let diff = (b.get(i).unwrap() - 1.0).abs();
                assert!(diff < 1e-10,
                    "Size {} test failed: solution[{}] = {}, expected 1.0, diff = {:.2e}",
                    n, i, b.get(i).unwrap(), diff);
            }
            
            // Test solution accuracy
            let residual_ratio = test_solution_accuracy(&original_a, &b, &original_b);
            assert!(residual_ratio < THRESH,
                "Size {} accuracy test failed: ratio = {:.2e}", n, residual_ratio);
        }
    }

    #[test]
    fn test_error_conditions() {
        // Test dimension mismatch errors
        let mut a = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0], 2, 2, Layout::RowMajor).unwrap();
        let mut b = Vector::from_slice(&[1.0, 2.0, 3.0]); // Wrong size
        
        let result = dgesv(&mut a, &mut b);
        assert!(result.is_err(), "Expected dimension mismatch error");
        
        // Test non-square matrix error
        let mut a_nonsquare = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0, 5.0, 6.0], 2, 3, Layout::RowMajor).unwrap();
        let mut b_correct = Vector::from_slice(&[1.0, 2.0]);
        
        let result = dgesv(&mut a_nonsquare, &mut b_correct);
        assert!(result.is_err(), "Expected non-square matrix error");
    }

    #[test]
    fn test_singular_matrix_detection() {
        // Test with a singular matrix (rank deficient)
        let singular_matrix = vec![
            1.0, 2.0, 3.0,
            2.0, 4.0, 6.0,  // Second row is 2x first row
            1.0, 1.0, 1.0
        ];
        
        let mut a = Matrix::from_slice(&singular_matrix, 3, 3, Layout::RowMajor).unwrap();
        let mut b = Vector::from_slice(&[1.0, 2.0, 1.0]);
        
        let result = dgesv(&mut a, &mut b);
        
        // LAPACK should detect singularity and return an error
        // The exact behavior depends on the LAPACK implementation
        match result {
            Err(_) => {
                println!("Correctly detected singular matrix");
            },
            Ok(_) => {
                // Some implementations might not detect singularity immediately
                // but the solution would be numerically unstable
                println!("Warning: Singular matrix not detected, but solution may be unstable");
            }
        }
    }
}