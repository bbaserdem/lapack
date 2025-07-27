//! Basic integration test to verify bindgen-generated bindings work
//!
//! This test doesn't actually call LAPACK functions (which would require
//! a LAPACK installation), but verifies that the bindings compile and
//! that we can reference the generated types and constants.

// Disable the test by default since it requires LAPACK to be installed
#![cfg(all(test, feature = "test-with-lapack"))]

use lapack_sys::*;

#[test]
fn test_binding_types_exist() {
    // Test that basic types are available
    let _n: lapack_int = 3;
    
    // Test that layout constants are available
    let _layout = LAPACK_ROW_MAJOR;
    
    // Test that we can create function pointers to LAPACKE functions
    // This verifies the function signatures were generated correctly
    let _dgesv_ptr: unsafe extern "C" fn(
        matrix_layout: ::core::ffi::c_int,
        n: lapack_int,
        nrhs: lapack_int,
        a: *mut ::core::ffi::c_double,
        lda: lapack_int,
        ipiv: *mut lapack_int,
        b: *mut ::core::ffi::c_double,
        ldb: lapack_int,
    ) -> lapack_int = LAPACKE_dgesv;
}

#[test]
fn test_complex_types() {
    // Verify complex number types are generated
    // The exact type names depend on the LAPACK configuration
    
    // For now, we just verify the test compiles
    // Actual usage would look like:
    // let _c: lapack_complex_float = ...;
    // let _z: lapack_complex_double = ...;
}