//! LAPACK source provider
//!
//! This crate does not contain any functionality of its own. It simply
//! provides a way to select which LAPACK implementation to link against
//! through Cargo features.
//!
//! # Features
//!
//! - `netlib`: Link against the reference Netlib implementation
//! - `openblas`: Link against OpenBLAS
//! - `intel-mkl`: Link against Intel MKL
//!
//! # Example
//!
//! In your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! lapack = "0.1"
//! lapack-src = { version = "0.1", features = ["openblas"] }
//! ```

#![no_std]

// Re-export the selected backend
#[cfg(feature = "netlib")]
extern crate netlib_src as raw;

#[cfg(feature = "openblas")]
extern crate openblas_src as raw;

#[cfg(feature = "intel-mkl")]
extern crate intel_mkl_src as raw;

// Ensure exactly one backend is selected
#[cfg(not(any(feature = "netlib", feature = "openblas", feature = "intel-mkl")))]
compile_error!("Please select a LAPACK provider by enabling one of the features: netlib, openblas, intel-mkl");

#[cfg(all(feature = "netlib", feature = "openblas"))]
compile_error!("Only one LAPACK provider can be enabled at a time");

#[cfg(all(feature = "netlib", feature = "intel-mkl"))]
compile_error!("Only one LAPACK provider can be enabled at a time");

#[cfg(all(feature = "openblas", feature = "intel-mkl"))]
compile_error!("Only one LAPACK provider can be enabled at a time");