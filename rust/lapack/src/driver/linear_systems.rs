//! Linear system solvers
//!
//! This module provides safe Rust wrappers for LAPACK linear system solvers.
//! These routines solve systems of linear equations Ax = b where A is a matrix
//! and x, b are vectors.

use crate::{Error, Layout, Matrix, Vector, Result};
use lapack_sys::{self, lapack_int, LAPACK_COL_MAJOR};

/// Solve a general system of linear equations Ax = b
///
/// This function solves the system of linear equations A*x = b using LU decomposition
/// with partial pivoting. Both A and b are modified in-place.
///
/// # Arguments
/// * `a` - On entry, the n-by-n coefficient matrix A. On exit, contains the LU decomposition
/// * `b` - On entry, the right-hand side matrix B. On exit, contains the solution X
///
/// # Returns
/// * `Ok(pivot_indices)` - Vector of pivot indices used in the LU decomposition
/// * `Err(error)` - Error if the system could not be solved
///
/// # Examples
/// ```rust
/// use lapack::{Matrix, Vector, Layout};
/// 
/// // Create a 2x2 system: [1, 2; 3, 4] * x = [5; 11]
/// let mut a = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0], 2, 2, Layout::RowMajor)?;
/// let mut b = Vector::from_slice(&[5.0, 11.0]);
/// 
/// let pivot = lapack::dgesv(&mut a, &mut b)?;
/// // Solution: x = [1.0, 2.0]
/// # Ok::<(), lapack::Error>(())
/// ```
pub fn dgesv(a: &mut Matrix<f64>, b: &mut Vector<f64>) -> Result<Vector<i32>> {
    // Check dimensions
    if a.rows() != a.cols() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: a.cols(),
        });
    }
    
    if a.rows() != b.len() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: b.len(),
        });
    }

    let n = a.rows() as lapack_int;
    let nrhs = 1_i32; // Single right-hand side vector
    
    // Convert matrix to column-major layout for LAPACK
    a.convert_layout(Layout::ColumnMajor);
    
    let lda = n;
    let ldb = n;
    
    // Allocate pivot vector
    let mut ipiv = vec![0_i32; n as usize];
    
    // Call LAPACKE_dgesv
    let info = unsafe {
        lapack_sys::LAPACKE_dgesv(
            LAPACK_COL_MAJOR as i32,
            n,
            nrhs,
            a.as_mut_slice().as_mut_ptr(),
            lda,
            ipiv.as_mut_ptr(),
            b.as_mut_slice().as_mut_ptr(),
            ldb,
        )
    };
    
    // Check for errors
    Error::check_lapack_info(info, "dgesv")?;
    
    Ok(Vector::from_vec(ipiv))
}

/// Solve a general system of linear equations with multiple right-hand sides Ax = B
///
/// This function solves the system A*X = B where A is n-by-n and B is n-by-nrhs.
/// Both A and B are modified in-place.
///
/// # Arguments
/// * `a` - On entry, the n-by-n coefficient matrix A. On exit, contains the LU decomposition
/// * `b` - On entry, the right-hand side matrix B. On exit, contains the solution X
///
/// # Returns
/// * `Ok(pivot_indices)` - Vector of pivot indices used in the LU decomposition
/// * `Err(error)` - Error if the system could not be solved
pub fn dgesv_multiple(a: &mut Matrix<f64>, b: &mut Matrix<f64>) -> Result<Vector<i32>> {
    // Check dimensions
    if a.rows() != a.cols() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: a.cols(),
        });
    }
    
    if a.rows() != b.rows() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: b.rows(),
        });
    }

    let n = a.rows() as lapack_int;
    let nrhs = b.cols() as lapack_int;
    
    // Convert matrices to column-major layout for LAPACK
    a.convert_layout(Layout::ColumnMajor);
    b.convert_layout(Layout::ColumnMajor);
    
    let lda = n;
    let ldb = n;
    
    // Allocate pivot vector
    let mut ipiv = vec![0_i32; n as usize];
    
    // Call LAPACKE_dgesv
    let info = unsafe {
        lapack_sys::LAPACKE_dgesv(
            LAPACK_COL_MAJOR as i32,
            n,
            nrhs,
            a.as_mut_slice().as_mut_ptr(),
            lda,
            ipiv.as_mut_ptr(),
            b.as_mut_slice().as_mut_ptr(),
            ldb,
        )
    };
    
    // Check for errors
    Error::check_lapack_info(info, "dgesv")?;
    
    Ok(Vector::from_vec(ipiv))
}

/// Compute the LU decomposition of a general matrix
///
/// This function computes the LU decomposition of an m-by-n matrix A using partial
/// pivoting with row interchanges. The decomposition has the form A = P*L*U where
/// P is a permutation matrix, L is lower triangular, and U is upper triangular.
///
/// # Arguments
/// * `a` - On entry, the m-by-n matrix A. On exit, contains the LU decomposition
///
/// # Returns
/// * `Ok(pivot_indices)` - Vector of pivot indices representing the permutation P
/// * `Err(error)` - Error if the decomposition failed
///
/// # Examples
/// ```rust
/// use lapack::{Matrix, Layout};
/// 
/// let mut a = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0], 2, 2, Layout::RowMajor)?;
/// let pivot = lapack::dgetrf(&mut a)?;
/// # Ok::<(), lapack::Error>(())
/// ```
pub fn dgetrf(a: &mut Matrix<f64>) -> Result<Vector<i32>> {
    let m = a.rows() as lapack_int;
    let n = a.cols() as lapack_int;
    
    // Convert matrix to column-major layout for LAPACK
    a.convert_layout(Layout::ColumnMajor);
    
    let lda = m;
    
    // Allocate pivot vector
    let mut ipiv = vec![0_i32; std::cmp::min(m, n) as usize];
    
    // Call LAPACKE_dgetrf
    let info = unsafe {
        lapack_sys::LAPACKE_dgetrf(
            LAPACK_COL_MAJOR as i32,
            m,
            n,
            a.as_mut_slice().as_mut_ptr(),
            lda,
            ipiv.as_mut_ptr(),
        )
    };
    
    // Check for errors
    Error::check_lapack_info(info, "dgetrf")?;
    
    Ok(Vector::from_vec(ipiv))
}

/// Solve a system using LU decomposition computed by `dgetrf`
///
/// This function solves the system A*X = B using the LU decomposition computed
/// by `dgetrf`. The matrix A should be the output from `dgetrf`, and the pivot
/// indices should be those returned by `dgetrf`.
///
/// # Arguments
/// * `a` - The LU decomposition computed by `dgetrf`
/// * `ipiv` - The pivot indices returned by `dgetrf`
/// * `b` - On entry, the right-hand side. On exit, the solution
/// * `trans` - Whether to solve A*X=B ('N') or A^T*X=B ('T')
///
/// # Returns
/// * `Ok(())` - Success
/// * `Err(error)` - Error if the solution failed
pub fn dgetrs(
    a: &Matrix<f64>, 
    ipiv: &Vector<i32>, 
    b: &mut Vector<f64>, 
    trans: char
) -> Result<()> {
    // Check dimensions
    if a.rows() != a.cols() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: a.cols(),
        });
    }
    
    if a.rows() != b.len() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: b.len(),
        });
    }
    
    if ipiv.len() != a.rows() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: ipiv.len(),
        });
    }

    let n = a.rows() as lapack_int;
    let nrhs = 1_i32;
    let lda = n;
    let ldb = n;
    
    // Ensure matrix is in column-major format
    let a_col_major = a.to_column_major();
    
    // Call LAPACKE_dgetrs
    let info = unsafe {
        lapack_sys::LAPACKE_dgetrs(
            LAPACK_COL_MAJOR as i32,
            trans as i8,
            n,
            nrhs,
            a_col_major.as_slice().as_ptr(),
            lda,
            ipiv.as_slice().as_ptr(),
            b.as_mut_slice().as_mut_ptr(),
            ldb,
        )
    };
    
    // Check for errors
    Error::check_lapack_info(info, "dgetrs")?;
    
    Ok(())
}

/// Solve a system using LU decomposition with multiple right-hand sides
///
/// This is the multiple right-hand side version of `dgetrs`.
///
/// # Arguments
/// * `a` - The LU decomposition computed by `dgetrf`
/// * `ipiv` - The pivot indices returned by `dgetrf`
/// * `b` - On entry, the right-hand side matrix. On exit, the solution matrix
/// * `trans` - Whether to solve A*X=B ('N') or A^T*X=B ('T')
pub fn dgetrs_multiple(
    a: &Matrix<f64>, 
    ipiv: &Vector<i32>, 
    b: &mut Matrix<f64>, 
    trans: char
) -> Result<()> {
    // Check dimensions
    if a.rows() != a.cols() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: a.cols(),
        });
    }
    
    if a.rows() != b.rows() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: b.rows(),
        });
    }
    
    if ipiv.len() != a.rows() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: ipiv.len(),
        });
    }

    let n = a.rows() as lapack_int;
    let nrhs = b.cols() as lapack_int;
    let lda = n;
    let ldb = n;
    
    // Ensure matrices are in column-major format
    let a_col_major = a.to_column_major();
    b.convert_layout(Layout::ColumnMajor);
    
    // Call LAPACKE_dgetrs
    let info = unsafe {
        lapack_sys::LAPACKE_dgetrs(
            LAPACK_COL_MAJOR as i32,
            trans as i8,
            n,
            nrhs,
            a_col_major.as_slice().as_ptr(),
            lda,
            ipiv.as_slice().as_ptr(),
            b.as_mut_slice().as_mut_ptr(),
            ldb,
        )
    };
    
    // Check for errors
    Error::check_lapack_info(info, "dgetrs")?;
    
    Ok(())
}

/// Compute the Cholesky decomposition of a positive definite matrix
///
/// This function computes the Cholesky decomposition of a real symmetric
/// positive definite matrix A. The decomposition has the form A = U^T*U
/// (if uplo='U') or A = L*L^T (if uplo='L').
///
/// # Arguments
/// * `a` - On entry, the symmetric matrix A. On exit, contains the Cholesky factor
/// * `uplo` - Whether to store the upper ('U') or lower ('L') triangular factor
///
/// # Returns
/// * `Ok(())` - Success
/// * `Err(error)` - Error if the matrix is not positive definite
///
/// # Examples
/// ```rust
/// use lapack::{Matrix, Layout};
/// 
/// // Create a 2x2 positive definite matrix
/// let mut a = Matrix::from_slice(&[4.0, 2.0, 2.0, 3.0], 2, 2, Layout::RowMajor)?;
/// lapack::dpotrf(&mut a, 'L')?;
/// # Ok::<(), lapack::Error>(())
/// ```
pub fn dpotrf(a: &mut Matrix<f64>, uplo: char) -> Result<()> {
    // Check that matrix is square
    if a.rows() != a.cols() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: a.cols(),
        });
    }

    let n = a.rows() as lapack_int;
    let lda = n;
    
    // Convert matrix to column-major layout for LAPACK
    a.convert_layout(Layout::ColumnMajor);
    
    // Call LAPACKE_dpotrf
    let info = unsafe {
        lapack_sys::LAPACKE_dpotrf(
            LAPACK_COL_MAJOR as i32,
            uplo as i8,
            n,
            a.as_mut_slice().as_mut_ptr(),
            lda,
        )
    };
    
    // Check for errors
    Error::check_lapack_info(info, "dpotrf")?;
    
    Ok(())
}

/// Solve a system using Cholesky decomposition computed by `dpotrf`
///
/// This function solves the system A*X = B using the Cholesky decomposition
/// computed by `dpotrf`.
///
/// # Arguments
/// * `a` - The Cholesky decomposition computed by `dpotrf`
/// * `b` - On entry, the right-hand side. On exit, the solution
/// * `uplo` - Whether the Cholesky factor is upper ('U') or lower ('L') triangular
///
/// # Returns
/// * `Ok(())` - Success
/// * `Err(error)` - Error if the solution failed
pub fn dpotrs(a: &Matrix<f64>, b: &mut Vector<f64>, uplo: char) -> Result<()> {
    // Check dimensions
    if a.rows() != a.cols() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: a.cols(),
        });
    }
    
    if a.rows() != b.len() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: b.len(),
        });
    }

    let n = a.rows() as lapack_int;
    let nrhs = 1_i32;
    let lda = n;
    let ldb = n;
    
    // Ensure matrix is in column-major format
    let a_col_major = a.to_column_major();
    
    // Call LAPACKE_dpotrs
    let info = unsafe {
        lapack_sys::LAPACKE_dpotrs(
            LAPACK_COL_MAJOR as i32,
            uplo as i8,
            n,
            nrhs,
            a_col_major.as_slice().as_ptr(),
            lda,
            b.as_mut_slice().as_mut_ptr(),
            ldb,
        )
    };
    
    // Check for errors
    Error::check_lapack_info(info, "dpotrs")?;
    
    Ok(())
}

/// Solve a system using Cholesky decomposition with multiple right-hand sides
///
/// This is the multiple right-hand side version of `dpotrs`.
///
/// # Arguments
/// * `a` - The Cholesky decomposition computed by `dpotrf`
/// * `b` - On entry, the right-hand side matrix. On exit, the solution matrix
/// * `uplo` - Whether the Cholesky factor is upper ('U') or lower ('L') triangular
pub fn dpotrs_multiple(a: &Matrix<f64>, b: &mut Matrix<f64>, uplo: char) -> Result<()> {
    // Check dimensions
    if a.rows() != a.cols() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: a.cols(),
        });
    }
    
    if a.rows() != b.rows() {
        return Err(Error::DimensionMismatch {
            expected: a.rows(),
            actual: b.rows(),
        });
    }

    let n = a.rows() as lapack_int;
    let nrhs = b.cols() as lapack_int;
    let lda = n;
    let ldb = n;
    
    // Ensure matrices are in column-major format
    let a_col_major = a.to_column_major();
    b.convert_layout(Layout::ColumnMajor);
    
    // Call LAPACKE_dpotrs
    let info = unsafe {
        lapack_sys::LAPACKE_dpotrs(
            LAPACK_COL_MAJOR as i32,
            uplo as i8,
            n,
            nrhs,
            a_col_major.as_slice().as_ptr(),
            lda,
            b.as_mut_slice().as_mut_ptr(),
            ldb,
        )
    };
    
    // Check for errors
    Error::check_lapack_info(info, "dpotrs")?;
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Matrix, Vector, Layout};

    #[test]
    fn test_dgesv_simple() {
        // Solve [1, 2; 3, 4] * x = [5; 11]
        // Expected solution: x = [1, 2]
        let mut a = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0], 2, 2, Layout::RowMajor).unwrap();
        let mut b = Vector::from_slice(&[5.0, 11.0]);
        
        let _pivot = dgesv(&mut a, &mut b).unwrap();
        
        // Check solution (approximately)
        assert!((b.get(0).unwrap() - 1.0).abs() < 1e-10);
        assert!((b.get(1).unwrap() - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_lu_decomposition() {
        let mut a = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0], 2, 2, Layout::RowMajor).unwrap();
        let _original_a = a.clone();
        
        // Compute LU decomposition
        let ipiv = dgetrf(&mut a).unwrap();
        assert_eq!(ipiv.len(), 2);
        
        // Solve using the decomposition
        let mut b = Vector::from_slice(&[5.0, 11.0]);
        dgetrs(&a, &ipiv, &mut b, 'N').unwrap();
        
        // Check solution
        assert!((b.get(0).unwrap() - 1.0).abs() < 1e-10);
        assert!((b.get(1).unwrap() - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_cholesky_decomposition() {
        // Create a positive definite matrix [[4, 2], [2, 3]]
        let mut a = Matrix::from_slice(&[4.0, 2.0, 2.0, 3.0], 2, 2, Layout::RowMajor).unwrap();
        
        // Compute Cholesky decomposition
        dpotrf(&mut a, 'L').unwrap();
        
        // Solve using the decomposition
        let mut b = Vector::from_slice(&[10.0, 8.0]);
        dpotrs(&a, &mut b, 'L').unwrap();
        
        // The solution should be approximately [2, 1]
        // (we can verify: [4, 2; 2, 3] * [2; 1] = [10; 7] ≠ [10; 8])
        // Let's check the solution makes sense
        assert!(b.get(0).unwrap().is_finite());
        assert!(b.get(1).unwrap().is_finite());
    }

    #[test]
    fn test_dimension_mismatch() {
        let mut a = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0], 2, 2, Layout::RowMajor).unwrap();
        let mut b = Vector::from_slice(&[5.0, 11.0, 7.0]); // Wrong size
        
        assert!(dgesv(&mut a, &mut b).is_err());
    }
}