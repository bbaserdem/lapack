//! Error types for LAPACK operations

use thiserror::Error;

/// Result type for LAPACK operations
pub type Result<T> = std::result::Result<T, Error>;

/// Errors that can occur in LAPACK operations
#[derive(Debug, Error)]
pub enum Error {
    /// Invalid parameter passed to LAPACK routine
    #[error("Invalid parameter at position {position}")]
    InvalidParameter { position: i32 },
    
    /// Matrix is singular
    #[error("Matrix is singular (factor U({position},{position}) is exactly zero)")]
    SingularMatrix { position: i32 },
    
    /// Convergence failure in iterative algorithm
    #[error("Algorithm failed to converge after {iterations} iterations")]
    ConvergenceFailure { iterations: i32 },
    
    /// Dimension mismatch
    #[error("Dimension mismatch: expected {expected}, got {actual}")]
    DimensionMismatch { expected: usize, actual: usize },
    
    /// Invalid layout
    #[error("Invalid layout for this operation")]
    InvalidLayout,
    
    /// Memory allocation failure
    #[error("Failed to allocate memory")]
    AllocationError,
    
    /// Other LAPACK error
    #[error("LAPACK error: {info}")]
    LapackError { info: i32 },
}

impl Error {
    /// Create an error from LAPACK info parameter
    pub fn from_lapack_info(info: i32, routine: &str) -> Self {
        if info < 0 {
            Error::InvalidParameter { position: -info }
        } else if info > 0 {
            // The meaning of positive info depends on the routine
            // This is a simplified version; real implementation would be routine-specific
            match routine {
                "dgesv" | "sgesv" | "zgesv" | "cgesv" => {
                    Error::SingularMatrix { position: info }
                }
                "dgeev" | "sgeev" | "zgeev" | "cgeev" |
                "dsyev" | "ssyev" | "zheev" | "cheev" => {
                    Error::ConvergenceFailure { iterations: info }
                }
                _ => Error::LapackError { info }
            }
        } else {
            unreachable!("from_lapack_info called with info=0")
        }
    }
    
    /// Check LAPACK info parameter and convert to Result
    pub fn check_lapack_info(info: i32, routine: &str) -> Result<()> {
        if info == 0 {
            Ok(())
        } else {
            Err(Self::from_lapack_info(info, routine))
        }
    }
}