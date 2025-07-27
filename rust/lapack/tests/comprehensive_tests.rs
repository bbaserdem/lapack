//! Comprehensive LAPACK test cases following DGET01/DGET02 patterns
//!
//! This module implements test patterns similar to the original LAPACK test suite:
//! - Factorization accuracy testing (DGET01 pattern)
//! - Solution accuracy testing (DGET02 pattern)
//! - Multiple matrix sizes and conditions
//! - Numerical accuracy validation

#[cfg(feature = "linear-systems")]
mod tests {
    use lapack::{Matrix, Vector, Layout};
    use lapack::driver::linear_systems::{dgesv, dgetrf, dgetrs, dpotrf, dpotrs};
    
    const EPS: f64 = f64::EPSILON;
    const ACCURACY_THRESHOLD: f64 = 1e-10;

    /// Test helper: compute matrix 1-norm
    fn matrix_1_norm(matrix: &Matrix<f64>) -> f64 {
        let mut max_col_sum = 0.0_f64;
        for j in 0..matrix.cols() {
            let mut col_sum = 0.0;
            for i in 0..matrix.rows() {
                col_sum += matrix.get(i, j).unwrap().abs();
            }
            max_col_sum = max_col_sum.max(col_sum);
        }
        max_col_sum
    }

    /// Test helper: compute vector 1-norm
    fn vector_1_norm(vector: &Vector<f64>) -> f64 {
        let mut sum = 0.0;
        for i in 0..vector.len() {
            sum += vector.get(i).unwrap().abs();
        }
        sum
    }

    /// Test helper: matrix-vector multiplication
    fn matvec_multiply(matrix: &Matrix<f64>, vector: &Vector<f64>) -> Vector<f64> {
        assert_eq!(matrix.cols(), vector.len());
        let mut result = Vector::<f64>::zeros(matrix.rows());
        
        for i in 0..matrix.rows() {
            let mut sum = 0.0;
            for j in 0..matrix.cols() {
                sum += matrix.get(i, j).unwrap() * vector.get(j).unwrap();
            }
            result.set(i, sum).unwrap();
        }
        result
    }

    /// Test helper: vector subtraction
    fn vector_subtract(a: &Vector<f64>, b: &Vector<f64>) -> Vector<f64> {
        assert_eq!(a.len(), b.len());
        let mut result = a.clone();
        for i in 0..a.len() {
            let diff = a.get(i).unwrap() - b.get(i).unwrap();
            result.set(i, diff).unwrap();
        }
        result
    }

    /// DGET02-style solution accuracy test
    /// Computes ||b - A*x|| / (||A|| * ||x|| * eps) where A*x should equal b
    fn solution_accuracy_test(
        original_matrix: &Matrix<f64>,
        solution: &Vector<f64>,
        original_rhs: &Vector<f64>
    ) -> f64 {
        let computed_rhs = matvec_multiply(original_matrix, solution);
        let residual = vector_subtract(original_rhs, &computed_rhs);
        
        let residual_norm = vector_1_norm(&residual);
        let matrix_norm = matrix_1_norm(original_matrix);
        let solution_norm = vector_1_norm(solution);
        
        if matrix_norm > 0.0 && solution_norm > 0.0 {
            residual_norm / (matrix_norm * solution_norm * EPS)
        } else {
            residual_norm
        }
    }

    /// Generate test matrix with controlled properties
    fn generate_test_matrix(size: usize, condition_type: &str) -> (Matrix<f64>, Vector<f64>) {
        match condition_type {
            "identity" => {
                let mut data = vec![0.0; size * size];
                for i in 0..size {
                    data[i * size + i] = 1.0;
                }
                let matrix = Matrix::from_slice(&data, size, size, Layout::RowMajor).unwrap();
                let rhs = (1..=size).map(|i| i as f64).collect::<Vec<_>>();
                let rhs_vec = Vector::from_slice(&rhs);
                (matrix, rhs_vec)
            },
            "diagonal" => {
                let mut data = vec![0.0; size * size];
                for i in 0..size {
                    data[i * size + i] = (i + 1) as f64;
                }
                let matrix = Matrix::from_slice(&data, size, size, Layout::RowMajor).unwrap();
                let rhs = vec![1.0; size];
                let rhs_vec = Vector::from_slice(&rhs);
                (matrix, rhs_vec)
            },
            "well_conditioned" => {
                // Create a simple well-conditioned matrix
                let mut data = vec![0.0; size * size];
                for i in 0..size {
                    for j in 0..size {
                        if i == j {
                            data[i * size + j] = 2.0 + (i as f64) * 0.1;
                        } else {
                            data[i * size + j] = 0.1;
                        }
                    }
                }
                let matrix = Matrix::from_slice(&data, size, size, Layout::RowMajor).unwrap();
                // Create RHS such that solution is approximately [1, 1, ..., 1]
                let mut rhs = vec![0.0; size];
                for i in 0..size {
                    for j in 0..size {
                        rhs[i] += data[i * size + j];
                    }
                }
                let rhs_vec = Vector::from_slice(&rhs);
                (matrix, rhs_vec)
            },
            _ => panic!("Unknown condition type: {}", condition_type)
        }
    }

    #[test]
    fn test_dgesv_accuracy_multiple_sizes() {
        let sizes = vec![1, 2, 3, 5, 10];
        let conditions = vec!["identity", "diagonal", "well_conditioned"];
        
        for &size in &sizes {
            for condition in &conditions {
                let (mut matrix, mut rhs) = generate_test_matrix(size, condition);
                let original_matrix = matrix.clone();
                let original_rhs = rhs.clone();
                
                // Solve the system
                let result = dgesv(&mut matrix, &mut rhs);
                assert!(result.is_ok(), "dgesv failed for size {} condition {}", size, condition);
                
                // Test solution accuracy
                let accuracy_ratio = solution_accuracy_test(&original_matrix, &rhs, &original_rhs);
                
                assert!(accuracy_ratio < ACCURACY_THRESHOLD,
                    "Solution accuracy failed for size {} condition {}: ratio = {:.2e}",
                    size, condition, accuracy_ratio);
            }
        }
    }

    #[test]
    fn test_dgetrf_dgetrs_accuracy() {
        let test_cases = vec![
            (2, "identity"),
            (3, "diagonal"), 
            (5, "well_conditioned"),
            (10, "well_conditioned"),
        ];
        
        for (size, condition) in test_cases {
            let (mut matrix, mut rhs) = generate_test_matrix(size, condition);
            let original_matrix = matrix.clone();
            let original_rhs = rhs.clone();
            
            // Compute LU factorization
            let ipiv_result = dgetrf(&mut matrix);
            assert!(ipiv_result.is_ok(), "dgetrf failed for size {} condition {}", size, condition);
            let ipiv = ipiv_result.unwrap();
            
            // Solve using factorization
            let solve_result = dgetrs(&matrix, &ipiv, &mut rhs, 'N');
            assert!(solve_result.is_ok(), "dgetrs failed for size {} condition {}", size, condition);
            
            // Test solution accuracy
            let accuracy_ratio = solution_accuracy_test(&original_matrix, &rhs, &original_rhs);
            
            assert!(accuracy_ratio < ACCURACY_THRESHOLD,
                "LU solution accuracy failed for size {} condition {}: ratio = {:.2e}",
                size, condition, accuracy_ratio);
        }
    }

    #[test]
    fn test_dpotrf_dpotrs_positive_definite() {
        // Generate positive definite test matrices
        let sizes = vec![2, 3, 5];
        
        for size in sizes {
            // Create a simple positive definite matrix: A = I + ones/size
            let mut data = vec![1.0 / (size as f64); size * size];
            for i in 0..size {
                data[i * size + i] += 1.0;
            }
            
            let mut matrix = Matrix::from_slice(&data, size, size, Layout::RowMajor).unwrap();
            let original_matrix = matrix.clone();
            
            // Create RHS vector
            let rhs_data: Vec<f64> = (1..=size).map(|i| i as f64).collect();
            let mut rhs = Vector::from_slice(&rhs_data);
            let original_rhs = rhs.clone();
            
            // Compute Cholesky factorization
            let chol_result = dpotrf(&mut matrix, 'L');
            assert!(chol_result.is_ok(), "dpotrf failed for size {}", size);
            
            // Solve using Cholesky factorization
            let solve_result = dpotrs(&matrix, &mut rhs, 'L');
            assert!(solve_result.is_ok(), "dpotrs failed for size {}", size);
            
            // Test solution accuracy
            let accuracy_ratio = solution_accuracy_test(&original_matrix, &rhs, &original_rhs);
            
            assert!(accuracy_ratio < ACCURACY_THRESHOLD,
                "Cholesky solution accuracy failed for size {}: ratio = {:.2e}",
                size, accuracy_ratio);
        }
    }

    #[test]
    fn test_error_handling() {
        // Test dimension mismatch
        let mut matrix = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0], 2, 2, Layout::RowMajor).unwrap();
        let mut wrong_rhs = Vector::from_slice(&[1.0, 2.0, 3.0]); // Size 3 instead of 2
        
        let result = dgesv(&mut matrix, &mut wrong_rhs);
        assert!(result.is_err(), "Expected dimension mismatch error");
        
        // Test non-square matrix
        let mut non_square = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0, 5.0, 6.0], 2, 3, Layout::RowMajor).unwrap();
        let mut correct_rhs = Vector::from_slice(&[1.0, 2.0]);
        
        let result = dgesv(&mut non_square, &mut correct_rhs);
        assert!(result.is_err(), "Expected non-square matrix error");
    }

    #[test]
    fn test_layout_handling() {
        // Test that both row-major and column-major layouts work correctly
        let data = vec![1.0, 2.0, 3.0, 4.0]; // 2x2 matrix
        let rhs_data = vec![5.0, 11.0];
        
        // Test with row-major layout
        let mut matrix_row = Matrix::from_slice(&data, 2, 2, Layout::RowMajor).unwrap();
        let mut rhs_row = Vector::from_slice(&rhs_data);
        let _original_matrix_row = matrix_row.clone();
        let _original_rhs_row = rhs_row.clone();
        
        let result_row = dgesv(&mut matrix_row, &mut rhs_row);
        assert!(result_row.is_ok(), "dgesv failed with row-major layout");
        
        // Test with column-major layout (same logical matrix)
        let col_major_data = vec![1.0, 3.0, 2.0, 4.0]; // Transposed storage
        let mut matrix_col = Matrix::from_slice(&col_major_data, 2, 2, Layout::ColumnMajor).unwrap();
        let mut rhs_col = Vector::from_slice(&rhs_data);
        
        let result_col = dgesv(&mut matrix_col, &mut rhs_col);
        assert!(result_col.is_ok(), "dgesv failed with column-major layout");
        
        // Solutions should be the same (within numerical precision)
        for i in 0..rhs_row.len() {
            let diff = (rhs_row.get(i).unwrap() - rhs_col.get(i).unwrap()).abs();
            assert!(diff < 1e-14, "Layout handling produced different solutions");
        }
    }

    #[test]
    fn test_numerical_stability() {
        // Test with a range of matrix conditions
        let base_size = 4;
        
        // Test 1: Well-conditioned matrix
        let mut well_cond_data = vec![0.0; base_size * base_size];
        for i in 0..base_size {
            for j in 0..base_size {
                well_cond_data[i * base_size + j] = if i == j { 2.0 } else { 0.1 };
            }
        }
        
        let mut matrix = Matrix::from_slice(&well_cond_data, base_size, base_size, Layout::RowMajor).unwrap();
        let mut rhs = Vector::from_slice(&vec![1.0; base_size]);
        let original_matrix = matrix.clone();
        let original_rhs = rhs.clone();
        
        let result = dgesv(&mut matrix, &mut rhs);
        assert!(result.is_ok(), "Well-conditioned matrix solution failed");
        
        let accuracy = solution_accuracy_test(&original_matrix, &rhs, &original_rhs);
        assert!(accuracy < ACCURACY_THRESHOLD, 
            "Well-conditioned matrix accuracy failed: {:.2e}", accuracy);
    }
}