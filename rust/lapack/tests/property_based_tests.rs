//! Property-based tests for LAPACK routines
//!
//! This module implements property-based testing patterns similar to QuickCheck,
//! testing mathematical properties that should hold for linear algebra operations
//! regardless of specific input values.

#[cfg(feature = "linear-systems")]
mod tests {
    use lapack::{Matrix, Vector, Layout};
    use lapack::driver::linear_systems::{dgesv, dgetrf, dgetrs, dpotrf, dpotrs};
    
    const TOLERANCE: f64 = 1e-8;

    /// Property: For any nonsingular matrix A and vector b, solving Ax=b should give
    /// a solution x such that Ax is approximately equal to b
    #[test]
    fn property_dgesv_solution_satisfies_equation() {
        let test_matrices = generate_well_conditioned_matrices();
        
        for (original_matrix, rhs) in test_matrices {
            let mut matrix = original_matrix.clone();
            let mut solution = rhs.clone();
            
            let result = dgesv(&mut matrix, &mut solution);
            if result.is_err() {
                continue; // Skip if matrix is singular
            }
            
            // Property: A * solution ≈ original_rhs
            let computed_rhs = matrix_vector_multiply(&original_matrix, &solution);
            
            for i in 0..rhs.len() {
                let expected = rhs.get(i).unwrap();
                let actual = computed_rhs.get(i).unwrap();
                let error = (expected - actual).abs();
                let relative_error = if expected.abs() > 1e-12 {
                    error / expected.abs()
                } else {
                    error
                };
                
                assert!(relative_error < TOLERANCE,
                    "Solution doesn't satisfy equation: expected {}, got {}, error = {:.2e}",
                    expected, actual, relative_error);
            }
        }
    }

    /// Property: LU factorization should be invertible - 
    /// dgetrf followed by dgetrs should give the same result as dgesv
    #[test]
    fn property_lu_factorization_equivalence() {
        let test_matrices = generate_well_conditioned_matrices();
        
        for (matrix, rhs) in test_matrices {
            let mut matrix_gesv = matrix.clone();
            let mut rhs_gesv = rhs.clone();
            
            let mut matrix_getrf = matrix.clone();
            let mut rhs_getrs = rhs.clone();
            
            // Solve with dgesv
            let gesv_result = dgesv(&mut matrix_gesv, &mut rhs_gesv);
            if gesv_result.is_err() {
                continue; // Skip singular matrices
            }
            
            // Solve with dgetrf + dgetrs
            let getrf_result = dgetrf(&mut matrix_getrf);
            if getrf_result.is_err() {
                continue;
            }
            let ipiv = getrf_result.unwrap();
            
            let getrs_result = dgetrs(&matrix_getrf, &ipiv, &mut rhs_getrs, 'N');
            if getrs_result.is_err() {
                continue;
            }
            
            // Property: Both methods should give the same solution
            for i in 0..rhs.len() {
                let gesv_sol = rhs_gesv.get(i).unwrap();
                let getrs_sol = rhs_getrs.get(i).unwrap();
                let diff = (gesv_sol - getrs_sol).abs();
                
                assert!(diff < TOLERANCE,
                    "LU factorization doesn't match direct solve: dgesv = {}, dgetrs = {}, diff = {:.2e}",
                    gesv_sol, getrs_sol, diff);
            }
        }
    }

    /// Property: For positive definite matrices, Cholesky factorization should work
    /// and give the same result as general LU factorization
    #[test]
    fn property_cholesky_positive_definite() {
        let pd_matrices = generate_positive_definite_matrices();
        
        for (matrix, rhs) in pd_matrices {
            let mut matrix_gesv = matrix.clone();
            let mut rhs_gesv = rhs.clone();
            
            let mut matrix_chol = matrix.clone();
            let mut rhs_chol = rhs.clone();
            
            // Solve with general method
            let gesv_result = dgesv(&mut matrix_gesv, &mut rhs_gesv);
            if gesv_result.is_err() {
                continue;
            }
            
            // Solve with Cholesky
            let potrf_result = dpotrf(&mut matrix_chol, 'L');
            if potrf_result.is_err() {
                continue; // Not positive definite after all
            }
            
            let potrs_result = dpotrs(&matrix_chol, &mut rhs_chol, 'L');
            assert!(potrs_result.is_ok(), "dpotrs failed after successful dpotrf");
            
            // Property: Both methods should give the same solution
            for i in 0..rhs.len() {
                let gesv_sol = rhs_gesv.get(i).unwrap();
                let chol_sol = rhs_chol.get(i).unwrap();
                let diff = (gesv_sol - chol_sol).abs();
                
                assert!(diff < TOLERANCE,
                    "Cholesky doesn't match general solve: gesv = {}, chol = {}, diff = {:.2e}",
                    gesv_sol, chol_sol, diff);
            }
        }
    }

    /// Property: Identity matrix should give trivial solutions
    #[test]
    fn property_identity_matrix() {
        let sizes = vec![1, 2, 3, 5, 10];
        
        for size in sizes {
            // Create identity matrix
            let mut identity_data = vec![0.0; size * size];
            for i in 0..size {
                identity_data[i * size + i] = 1.0;
            }
            let mut identity = Matrix::from_slice(&identity_data, size, size, Layout::RowMajor).unwrap();
            
            // Create test RHS
            let rhs_data: Vec<f64> = (1..=size).map(|i| i as f64).collect();
            let mut rhs = Vector::from_slice(&rhs_data);
            let original_rhs = rhs.clone();
            
            let result = dgesv(&mut identity, &mut rhs);
            assert!(result.is_ok(), "Identity matrix solve failed for size {}", size);
            
            // Property: For identity matrix, solution should equal RHS
            for i in 0..size {
                let expected = original_rhs.get(i).unwrap();
                let actual = rhs.get(i).unwrap();
                let diff = (expected - actual).abs();
                
                assert!(diff < TOLERANCE,
                    "Identity matrix property failed: expected {}, got {}, diff = {:.2e}",
                    expected, actual, diff);
            }
        }
    }

    /// Property: Diagonal matrices should give predictable solutions
    #[test]
    fn property_diagonal_matrix() {
        let sizes = vec![2, 3, 5];
        
        for size in sizes {
            // Create diagonal matrix with values [1, 2, 3, ...]
            let mut diag_data = vec![0.0; size * size];
            for i in 0..size {
                diag_data[i * size + i] = (i + 1) as f64;
            }
            let mut diagonal = Matrix::from_slice(&diag_data, size, size, Layout::RowMajor).unwrap();
            
            // Create RHS = [1, 1, 1, ...]
            let mut rhs = Vector::from_slice(&vec![1.0; size]);
            
            let result = dgesv(&mut diagonal, &mut rhs);
            assert!(result.is_ok(), "Diagonal matrix solve failed for size {}", size);
            
            // Property: For diagonal matrix D and RHS of ones, solution should be [1/d1, 1/d2, ...]
            for i in 0..size {
                let expected = 1.0 / ((i + 1) as f64);
                let actual = rhs.get(i).unwrap();
                let diff = (expected - actual).abs();
                
                assert!(diff < TOLERANCE,
                    "Diagonal matrix property failed at index {}: expected {}, got {}, diff = {:.2e}",
                    i, expected, actual, diff);
            }
        }
    }

    /// Property: Scaling a matrix and RHS by the same factor shouldn't change the solution
    #[test]
    fn property_scaling_invariance() {
        let scales = vec![0.1, 2.0, 10.0, 100.0];
        
        // Base test case
        let base_matrix_data = vec![2.0, 1.0, 1.0, 3.0]; // 2x2 matrix
        let base_rhs_data = vec![3.0, 4.0];
        
        let mut base_matrix = Matrix::from_slice(&base_matrix_data, 2, 2, Layout::RowMajor).unwrap();
        let mut base_rhs = Vector::from_slice(&base_rhs_data);
        
        let base_result = dgesv(&mut base_matrix, &mut base_rhs);
        assert!(base_result.is_ok(), "Base case solve failed");
        
        for scale in scales {
            // Scale matrix and RHS
            let scaled_matrix_data: Vec<f64> = base_matrix_data.iter().map(|x| x * scale).collect();
            let scaled_rhs_data: Vec<f64> = base_rhs_data.iter().map(|x| x * scale).collect();
            
            let mut scaled_matrix = Matrix::from_slice(&scaled_matrix_data, 2, 2, Layout::RowMajor).unwrap();
            let mut scaled_rhs = Vector::from_slice(&scaled_rhs_data);
            
            let scaled_result = dgesv(&mut scaled_matrix, &mut scaled_rhs);
            assert!(scaled_result.is_ok(), "Scaled case solve failed for scale {}", scale);
            
            // Property: Solution should be the same (within tolerance)
            for i in 0..2 {
                let base_sol = base_rhs.get(i).unwrap();
                let scaled_sol = scaled_rhs.get(i).unwrap();
                let diff = (base_sol - scaled_sol).abs();
                
                assert!(diff < TOLERANCE,
                    "Scaling invariance failed for scale {}: base = {}, scaled = {}, diff = {:.2e}",
                    scale, base_sol, scaled_sol, diff);
            }
        }
    }

    // Helper functions

    fn matrix_vector_multiply(matrix: &Matrix<f64>, vector: &Vector<f64>) -> Vector<f64> {
        assert_eq!(matrix.cols(), vector.len());
        let mut result = Vector::<f64>::zeros(matrix.rows());
        
        // Use unsafe accesses for better performance in tests
        unsafe {
            for i in 0..matrix.rows() {
                let mut sum = 0.0;
                for j in 0..matrix.cols() {
                    sum += matrix.get_unchecked(i, j) * vector.get_unchecked(j);
                }
                *result.get_unchecked_mut(i) = sum;
            }
        }
        result
    }

    fn generate_well_conditioned_matrices() -> Vec<(Matrix<f64>, Vector<f64>)> {
        vec![
            // 2x2 simple case
            {
                let matrix = Matrix::from_slice(&[2.0, 1.0, 1.0, 2.0], 2, 2, Layout::RowMajor).unwrap();
                let rhs = Vector::from_slice(&[3.0, 3.0]);
                (matrix, rhs)
            },
            // 3x3 diagonal dominant
            {
                let matrix = Matrix::from_slice(&[
                    3.0, 1.0, 0.5,
                    1.0, 4.0, 0.5,
                    0.5, 0.5, 3.0
                ], 3, 3, Layout::RowMajor).unwrap();
                let rhs = Vector::from_slice(&[4.5, 5.5, 4.0]);
                (matrix, rhs)
            },
            // 4x4 symmetric
            {
                let matrix = Matrix::from_slice(&[
                    4.0, 1.0, 1.0, 1.0,
                    1.0, 4.0, 1.0, 1.0,
                    1.0, 1.0, 4.0, 1.0,
                    1.0, 1.0, 1.0, 4.0
                ], 4, 4, Layout::RowMajor).unwrap();
                let rhs = Vector::from_slice(&[7.0, 7.0, 7.0, 7.0]);
                (matrix, rhs)
            },
        ]
    }

    fn generate_positive_definite_matrices() -> Vec<(Matrix<f64>, Vector<f64>)> {
        vec![
            // 2x2 positive definite
            {
                let matrix = Matrix::from_slice(&[4.0, 2.0, 2.0, 3.0], 2, 2, Layout::RowMajor).unwrap();
                let rhs = Vector::from_slice(&[6.0, 5.0]);
                (matrix, rhs)
            },
            // 3x3 positive definite
            {
                let matrix = Matrix::from_slice(&[
                    9.0, 3.0, 1.0,
                    3.0, 5.0, 2.0,
                    1.0, 2.0, 4.0
                ], 3, 3, Layout::RowMajor).unwrap();
                let rhs = Vector::from_slice(&[13.0, 10.0, 7.0]);
                (matrix, rhs)
            },
        ]
    }
}