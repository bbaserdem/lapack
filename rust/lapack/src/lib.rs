//! Safe Rust wrappers for LAPACK
//!
//! This crate provides safe, idiomatic Rust bindings to LAPACK routines.
//! It handles memory safety, error checking, and provides a more ergonomic
//! API compared to the raw FFI bindings.
//!
//! # Features
//!
//! This crate uses feature flags to control which routines are available:
//!
//! - `std` (default): Use the standard library
//! - `full`: Enable all LAPACK routines
//! - `driver`: Enable all driver routines
//! - `computational`: Enable all computational routines
//! - `auxiliary`: Enable all auxiliary routines
//!
//! See the [crate documentation](https://docs.rs/lapack) for a complete list
//! of available features.
//!
//! # Example
//!
//! ```rust
//! use lapack::{Matrix, Vector, Layout};
//! 
//! // Create a 2x2 matrix in row-major order
//! let data = vec![1.0, 2.0, 3.0, 4.0];
//! let matrix = Matrix::from_vec(data, 2, 2, Layout::RowMajor)?;
//! 
//! // Create a vector
//! let vector = Vector::from_vec(vec![5.0, 11.0]);
//! 
//! // Convert matrix to column-major for LAPACK compatibility
//! let col_major_matrix = matrix.to_column_major();
//! 
//! println!("Matrix: {}", matrix);
//! println!("Vector: {}", vector);
//! # Ok::<(), lapack::Error>(())
//! ```

pub mod error;
pub mod matrix;

pub use error::{Error, Result};
pub use matrix::{Matrix, Vector};

/// Matrix layout
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Layout {
    /// Column-major order (Fortran style)
    ColumnMajor,
    /// Row-major order (C style)
    RowMajor,
}

/// Matrix transpose operation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Transpose {
    /// No transpose
    No,
    /// Transpose
    Yes,
    /// Conjugate transpose
    ConjugateTranspose,
}

// Module organization
pub mod driver;      // Driver routines (solve, eigenvalues, etc.)

#[cfg(feature = "computational")]
pub mod computational; // Computational routines

#[cfg(feature = "auxiliary")]
pub mod auxiliary;    // Auxiliary routines

// Re-export commonly used items at crate root for convenience
// pub use driver::linear_systems::*;