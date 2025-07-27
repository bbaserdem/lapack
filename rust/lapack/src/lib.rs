//! Safe Rust wrappers for LAPACK
//!
//! This crate provides safe, idiomatic Rust bindings to LAPACK routines.
//! It handles memory safety, error checking, and provides a more ergonomic
//! API compared to the raw FFI bindings.
//!
//! # Example
//!
//! ```no_run
//! use lapack::{Layout, Transpose};
//! 
//! // Solve a system of linear equations Ax = b
//! let a = vec![1.0, 2.0, 3.0, 4.0]; // 2x2 matrix in column-major order
//! let mut b = vec![5.0, 11.0];      // Right-hand side
//! 
//! // Solve the system (result stored in b)
//! lapack::dgesv(Layout::ColumnMajor, 2, 1, &mut a, 2, &mut b, 2)?;
//! # Ok::<(), lapack::Error>(())
//! ```

pub mod error;

pub use error::{Error, Result};

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

// Module organization (to be implemented)
// pub mod driver;      // Driver routines (solve, eigenvalues, etc.)
// pub mod computational; // Computational routines
// pub mod auxiliary;    // Auxiliary routines