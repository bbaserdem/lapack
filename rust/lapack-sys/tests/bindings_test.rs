//! Test that bindgen-generated bindings compile and basic types exist

use lapack_sys::*;

#[test]
fn test_lapack_int_type() {
    // Verify lapack_int is the correct size
    #[cfg(not(feature = "ilp64"))]
    assert_eq!(std::mem::size_of::<lapack_int>(), 4);
    
    #[cfg(feature = "ilp64")]
    assert_eq!(std::mem::size_of::<lapack_int>(), 8);
    
    // Test that we can create lapack_int values
    let _n: lapack_int = 10;
    let _m: lapack_int = 5;
}

#[test]
fn test_constants_exist() {
    // Verify key constants are available
    let _row_major = LAPACK_ROW_MAJOR;
    let _col_major = LAPACK_COL_MAJOR;
    
    assert_eq!(LAPACK_ROW_MAJOR, 101);
    assert_eq!(LAPACK_COL_MAJOR, 102);
}

#[test] 
fn test_function_signatures() {
    // Test that we can create function pointers to verify signatures
    // This doesn't call the functions, just verifies they have the expected type
    
    // LAPACKE_dgesv: Solves a system of linear equations A*X = B
    let _dgesv: unsafe extern "C" fn(
        matrix_layout: ::core::ffi::c_int,
        n: lapack_int,
        nrhs: lapack_int,
        a: *mut ::core::ffi::c_double,
        lda: lapack_int,
        ipiv: *mut lapack_int,
        b: *mut ::core::ffi::c_double,
        ldb: lapack_int,
    ) -> lapack_int = LAPACKE_dgesv;
    
    // LAPACKE_dgetrf: Computes LU factorization
    let _dgetrf: unsafe extern "C" fn(
        matrix_layout: ::core::ffi::c_int,
        m: lapack_int,
        n: lapack_int,
        a: *mut ::core::ffi::c_double,
        lda: lapack_int,
        ipiv: *mut lapack_int,
    ) -> lapack_int = LAPACKE_dgetrf;
}

#[test]
fn test_complex_types() {
    // Test that complex types are generated correctly
    // The __BindgenComplex type should be available
    type ComplexFloat = __BindgenComplex<f32>;
    type ComplexDouble = __BindgenComplex<f64>;
    
    let c_float = ComplexFloat { re: 1.0, im: 2.0 };
    let c_double = ComplexDouble { re: 3.0, im: 4.0 };
    
    assert_eq!(c_float.re, 1.0);
    assert_eq!(c_float.im, 2.0);
    assert_eq!(c_double.re, 3.0);
    assert_eq!(c_double.im, 4.0);
}