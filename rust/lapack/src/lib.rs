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
#[cfg(feature = "driver")]
pub mod driver;      // Driver routines (solve, eigenvalues, etc.)

#[cfg(feature = "computational")]
pub mod computational; // Computational routines

#[cfg(feature = "auxiliary")]
pub mod auxiliary;    // Auxiliary routines

// Re-export commonly used items at crate root for convenience
#[cfg(feature = "linear-systems")]
pub use driver::linear_systems::*;

#[cfg(feature = "eigenvalues")]
pub use driver::eigenvalues::*;