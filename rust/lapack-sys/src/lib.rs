//! Low-level FFI bindings to LAPACK
//!
//! This crate provides raw FFI bindings to LAPACK functions. These bindings
//! are unsafe and require manual memory management. Most users should use
//! the higher-level `lapack` crate instead.
//!
//! # Safety
//!
//! All functions in this crate are unsafe as they:
//! - Accept raw pointers
//! - Require correct array dimensions
//! - Assume column-major (Fortran) layout
//! - Do not validate input parameters

#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use libc::{c_char, c_int, c_float, c_double};

// Placeholder for generated bindings
// The actual bindings will be generated from LAPACK source

// Example binding (to be replaced by generated code):
extern "C" {
    /// DGESV computes the solution to a real system of linear equations
    /// A * X = B, where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
    pub fn dgesv_(
        n: *const c_int,
        nrhs: *const c_int,
        a: *mut c_double,
        lda: *const c_int,
        ipiv: *mut c_int,
        b: *mut c_double,
        ldb: *const c_int,
        info: *mut c_int,
    );
}