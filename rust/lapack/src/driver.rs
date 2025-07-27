//! Driver routines - high-level LAPACK operations
//!
//! This module contains driver routines that solve complete problems,
//! such as systems of linear equations, eigenvalue problems, and
//! singular value decompositions.

#[cfg(feature = "linear-systems")]
pub mod linear_systems;

#[cfg(feature = "least-squares")]
pub mod least_squares;

#[cfg(feature = "eigenvalues")]
pub mod eigenvalues;

#[cfg(feature = "svd")]
pub mod svd;

// Re-export commonly used items
#[cfg(feature = "linear-systems")]
pub use linear_systems::*;