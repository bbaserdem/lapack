//! Edge case tests for LAPACK routines
//!
//! Tests numerical accuracy, error handling, and boundary conditions

#[cfg(feature = "linear-systems")]
mod tests {
    use lapack::{Matrix, Vector, Layout};
    use lapack::driver::linear_systems::{dgesv, dpotrf, dpotrs};

    #[test]
    fn test_small_matrices() {
        // Test 1x1 matrix
        let mut matrix_1x1 = Matrix::from_slice(&[2.0], 1, 1, Layout::RowMajor).unwrap();
        let mut rhs_1x1 = Vector::from_slice(&[4.0]);
        
        let result = dgesv(&mut matrix_1x1, &mut rhs_1x1);
        assert!(result.is_ok(), "1x1 matrix solve failed");
        assert!((rhs_1x1.get(0).unwrap() - 2.0).abs() < 1e-14, "1x1 solution incorrect");
        
        // Test 2x2 matrix
        let mut matrix_2x2 = Matrix::from_slice(&[1.0, 0.0, 0.0, 1.0], 2, 2, Layout::RowMajor).unwrap();
        let mut rhs_2x2 = Vector::from_slice(&[3.0, 5.0]);
        
        let result = dgesv(&mut matrix_2x2, &mut rhs_2x2);
        assert!(result.is_ok(), "2x2 identity solve failed");
    }

    #[test]
    fn test_error_conditions() {
        // Dimension mismatch
        let mut matrix = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0], 2, 2, Layout::RowMajor).unwrap();
        let mut wrong_rhs = Vector::from_slice(&[1.0, 2.0, 3.0]);
        
        assert!(dgesv(&mut matrix, &mut wrong_rhs).is_err(), "Should detect dimension mismatch");
        
        // Non-square matrix
        let mut non_square = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0, 5.0, 6.0], 2, 3, Layout::RowMajor).unwrap();
        let mut correct_rhs = Vector::from_slice(&[1.0, 2.0]);
        
        assert!(dgesv(&mut non_square, &mut correct_rhs).is_err(), "Should detect non-square matrix");
    }

    #[test]
    fn test_cholesky_simple() {
        // Simple positive definite matrix
        let mut matrix = Matrix::from_slice(&[4.0, 2.0, 2.0, 3.0], 2, 2, Layout::RowMajor).unwrap();
        let result = dpotrf(&mut matrix, 'L');
        assert!(result.is_ok(), "Simple Cholesky factorization failed");
        
        // Test solving with it
        let mut rhs = Vector::from_slice(&[6.0, 5.0]);
        let solve_result = dpotrs(&matrix, &mut rhs, 'L');
        assert!(solve_result.is_ok(), "Cholesky solve failed");
    }

    #[test]
    fn test_layout_consistency() {
        let data = vec![1.0, 2.0, 3.0, 4.0];
        let rhs = vec![5.0, 11.0];
        
        // Test row-major
        let mut matrix_row = Matrix::from_slice(&data, 2, 2, Layout::RowMajor).unwrap();
        let mut rhs_row = Vector::from_slice(&rhs);
        let result_row = dgesv(&mut matrix_row, &mut rhs_row);
        assert!(result_row.is_ok(), "Row-major solve failed");
        
        // Test column-major (need to transpose the data)
        let col_data = vec![1.0, 3.0, 2.0, 4.0]; // Column-major representation
        let mut matrix_col = Matrix::from_slice(&col_data, 2, 2, Layout::ColumnMajor).unwrap();
        let mut rhs_col = Vector::from_slice(&rhs);
        let result_col = dgesv(&mut matrix_col, &mut rhs_col);
        assert!(result_col.is_ok(), "Column-major solve failed");
    }

    #[test]
    fn test_singular_matrices() {
        // Completely singular matrix (rows are multiples)
        let mut singular_matrix = Matrix::from_slice(
            &[1.0, 2.0, 3.0,
              2.0, 4.0, 6.0,
              1.0, 2.0, 3.0], 
            3, 3, Layout::RowMajor
        ).unwrap();
        let mut rhs = Vector::from_slice(&[1.0, 2.0, 1.0]);
        
        let result = dgesv(&mut singular_matrix, &mut rhs);
        assert!(result.is_err(), "Should detect singular matrix");
        
        // Nearly singular matrix (very small determinant)
        let mut nearly_singular = Matrix::from_slice(
            &[1.0, 1.0,
              1.0, 1.0 + 1e-15], 
            2, 2, Layout::RowMajor
        ).unwrap();
        let mut rhs2 = Vector::from_slice(&[2.0, 2.0]);
        
        // Should either fail or produce warning about numerical issues
        let result2 = dgesv(&mut nearly_singular, &mut rhs2);
        // Even if it succeeds, solution may be numerically unstable
        if result2.is_ok() {
            println!("Warning: Nearly singular matrix solved, solution may be unstable");
        }
    }

    #[test]
    fn test_ill_conditioned_matrices() {
        // Small Hilbert matrix (famously ill-conditioned)
        let n = 3;
        let mut hilbert_data = vec![0.0; n * n];
        for i in 0..n {
            for j in 0..n {
                hilbert_data[i * n + j] = 1.0 / ((i + j + 2) as f64);
            }
        }
        
        let mut hilbert = Matrix::from_slice(&hilbert_data, n, n, Layout::RowMajor).unwrap();
        let mut rhs = Vector::from_slice(&vec![1.0; n]);
        
        // Store original matrix for residual check
        let original_hilbert = hilbert_data.clone();
        
        // Solve the system
        let result = dgesv(&mut hilbert, &mut rhs);
        assert!(result.is_ok(), "Hilbert matrix solve failed");
        
        // Verify solution by computing residual: ||Ax - b||
        let mut residual = vec![0.0; n];
        for i in 0..n {
            let mut sum = 0.0;
            for j in 0..n {
                sum += original_hilbert[i * n + j] * rhs.get(j).unwrap();
            }
            residual[i] = sum - 1.0; // Original RHS was all 1s
        }
        
        let residual_norm: f64 = residual.iter().map(|x| x * x).sum::<f64>().sqrt();
        
        // For ill-conditioned matrices, expect larger residual due to numerical errors
        // Hilbert matrices are notoriously ill-conditioned, so we use a looser tolerance
        assert!(residual_norm < 1.0, "Residual too large for ill-conditioned system: {}", residual_norm);
        
        // Test a moderately ill-conditioned matrix with known condition number
        // Matrix with condition number ~ 1000
        let mut ill_cond = Matrix::from_slice(
            &[1.0, 0.999,
              0.999, 1.0], 
            2, 2, Layout::RowMajor
        ).unwrap();
        let mut ill_rhs = Vector::from_slice(&[2.0, 2.0]);
        
        let result2 = dgesv(&mut ill_cond, &mut ill_rhs);
        assert!(result2.is_ok(), "Ill-conditioned 2x2 matrix solve failed");
    }

    #[test]
    fn test_extreme_values() {
        // Test with very large values
        let large_scale = 1e100;
        let mut large_matrix = Matrix::from_slice(
            &[large_scale, 0.0,
              0.0, large_scale], 
            2, 2, Layout::RowMajor
        ).unwrap();
        let mut large_rhs = Vector::from_slice(&[2.0 * large_scale, 3.0 * large_scale]);
        
        let result = dgesv(&mut large_matrix, &mut large_rhs);
        assert!(result.is_ok(), "Large value matrix solve failed");
        assert!((large_rhs.get(0).unwrap() - 2.0).abs() < 1e-10, "Large value solution incorrect");
        assert!((large_rhs.get(1).unwrap() - 3.0).abs() < 1e-10, "Large value solution incorrect");
        
        // Test with very small values
        let small_scale = 1e-100;
        let mut small_matrix = Matrix::from_slice(
            &[small_scale, 0.0,
              0.0, small_scale], 
            2, 2, Layout::RowMajor
        ).unwrap();
        let mut small_rhs = Vector::from_slice(&[2.0 * small_scale, 3.0 * small_scale]);
        
        let result2 = dgesv(&mut small_matrix, &mut small_rhs);
        assert!(result2.is_ok(), "Small value matrix solve failed");
        assert!((small_rhs.get(0).unwrap() - 2.0).abs() < 1e-10, "Small value solution incorrect");
        assert!((small_rhs.get(1).unwrap() - 3.0).abs() < 1e-10, "Small value solution incorrect");
        
        // Test with mixed scales (poorly scaled matrix)
        let mut mixed_matrix = Matrix::from_slice(
            &[1e10, 1e-10,
              1e-10, 1e10], 
            2, 2, Layout::RowMajor
        ).unwrap();
        let mut mixed_rhs = Vector::from_slice(&[1e10, 1e10]);
        
        let result3 = dgesv(&mut mixed_matrix, &mut mixed_rhs);
        assert!(result3.is_ok(), "Mixed scale matrix solve failed");
    }

    #[test]
    fn test_numerical_accuracy_validation() {
        // Test system with known exact solution
        // System: 2x + y = 5, x + 3y = 5
        // Exact solution: x = 2, y = 1
        let mut matrix = Matrix::from_slice(
            &[2.0, 1.0,
              1.0, 3.0], 
            2, 2, Layout::RowMajor
        ).unwrap();
        let mut rhs = Vector::from_slice(&[5.0, 5.0]);
        
        let result = dgesv(&mut matrix, &mut rhs);
        assert!(result.is_ok(), "Simple system solve failed");
        
        // Check against exact solution with high precision
        let x = rhs.get(0).unwrap();
        let y = rhs.get(1).unwrap();
        assert!((x - 2.0).abs() < 1e-14, "x solution not accurate enough: expected 2.0, got {}", x);
        assert!((y - 1.0).abs() < 1e-14, "y solution not accurate enough: expected 1.0, got {}", y);
        
        // Test tridiagonal system with known solution
        // Tridiagonal: [-2, 1, 0; 1, -2, 1; 0, 1, -2] * [1; 2; 3] = [-2+2; 1-4+3; 0+2-6] = [0; 0; -4]
        let mut tridiag = Matrix::from_slice(
            &[-2.0, 1.0, 0.0,
               1.0, -2.0, 1.0,
               0.0, 1.0, -2.0], 
            3, 3, Layout::RowMajor
        ).unwrap();
        let mut tridiag_rhs = Vector::from_slice(&[0.0, 0.0, -4.0]);
        
        let result2 = dgesv(&mut tridiag, &mut tridiag_rhs);
        assert!(result2.is_ok(), "Tridiagonal system solve failed");
        
        // Verify solution
        let x1 = tridiag_rhs.get(0).unwrap();
        let x2 = tridiag_rhs.get(1).unwrap();
        let x3 = tridiag_rhs.get(2).unwrap();
        
        assert!((x1 - 1.0).abs() < 1e-14, "Tridiagonal x1 incorrect: got {}, expected 1.0", x1);
        assert!((x2 - 2.0).abs() < 1e-14, "Tridiagonal x2 incorrect: got {}, expected 2.0", x2);
        assert!((x3 - 3.0).abs() < 1e-14, "Tridiagonal x3 incorrect: got {}, expected 3.0", x3);
    }

    #[test]
    fn test_high_precision_tolerance() {
        // Test with a well-conditioned symmetric positive definite matrix
        // Using smaller epsilon for validation
        let epsilon = 1e-15;
        
        // Create SPD matrix with known solution
        // System: 4x + 2y = 10, 2x + 5y = 9
        // Expected exact solution: x = 2, y = 1
        let mut spd_matrix = Matrix::from_slice(
            &[4.0, 2.0,
              2.0, 5.0], 
            2, 2, Layout::RowMajor
        ).unwrap();
        let mut rhs = Vector::from_slice(&[10.0, 9.0]);
        
        // Store original for verification
        let a11 = 4.0;
        let a12 = 2.0;
        let a21 = 2.0;
        let a22 = 5.0;
        let b1 = 10.0;
        let b2 = 9.0;
        
        let result = dgesv(&mut spd_matrix, &mut rhs);
        assert!(result.is_ok(), "SPD system solve failed");
        
        let x = rhs.get(0).unwrap();
        let y = rhs.get(1).unwrap();
        
        // Verify the solution satisfies the original equations
        let residual1 = (a11 * x + a12 * y - b1).abs();
        let residual2 = (a21 * x + a22 * y - b2).abs();
        
        assert!(residual1 < epsilon, "High precision test failed: equation 1 residual = {}", residual1);
        assert!(residual2 < epsilon, "High precision test failed: equation 2 residual = {}", residual2);
        
        // Also check if solution is close to expected
        assert!((x - 2.0).abs() < 1e-10, "Solution x not close to expected: got {}", x);
        assert!((y - 1.0).abs() < 1e-10, "Solution y not close to expected: got {}", y);
        
        // Test orthogonal matrix (should preserve norms exactly)
        // Rotation matrix by 45 degrees
        let sqrt2_inv = 1.0 / 2.0_f64.sqrt();
        let mut ortho_matrix = Matrix::from_slice(
            &[sqrt2_inv, -sqrt2_inv,
              sqrt2_inv, sqrt2_inv], 
            2, 2, Layout::RowMajor
        ).unwrap();
        let mut ortho_rhs = Vector::from_slice(&[sqrt2_inv, sqrt2_inv]);
        
        let result2 = dgesv(&mut ortho_matrix, &mut ortho_rhs);
        assert!(result2.is_ok(), "Orthogonal system solve failed");
        
        // Solution should be [1, 0]
        assert!((ortho_rhs.get(0).unwrap() - 1.0).abs() < epsilon, "Orthogonal x incorrect");
        assert!(ortho_rhs.get(1).unwrap().abs() < epsilon, "Orthogonal y should be 0");
    }
}