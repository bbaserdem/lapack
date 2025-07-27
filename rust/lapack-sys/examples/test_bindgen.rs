//! Test if bindgen generates bindings correctly
//!
//! Run with: cargo run --example test_bindgen --features std

fn main() {
    println!("This example tests if bindgen successfully generates bindings.");
    println!("If this compiles, bindgen is working!");
    
    // Try to use a generated type
    #[cfg(feature = "std")]
    {
        use lapack_sys::*;
        println!("lapack_int size: {} bytes", std::mem::size_of::<lapack_int>());
    }
}