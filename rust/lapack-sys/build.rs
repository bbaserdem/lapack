//! Build script for lapack-sys
//!
//! This script:
//! 1. Generates Rust FFI bindings from LAPACKE headers using bindgen
//! 2. Handles linking to the LAPACK library selected by lapack-src

use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    
    // Get the path to the LAPACKE headers
    // In the actual LAPACK source tree, headers are in LAPACKE/include/
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let lapack_root = manifest_dir
        .parent().unwrap()  // rust/
        .parent().unwrap(); // repository root
    
    let lapacke_include = lapack_root.join("LAPACKE").join("include");
    
    // Verify the header path exists
    if !lapacke_include.exists() {
        panic!(
            "LAPACKE headers not found at {:?}. \
             Make sure you're building from within the LAPACK source tree.",
            lapacke_include
        );
    }
    
    // Configure bindgen
    let bindings = bindgen::Builder::default()
        // Primary headers - lapacke.h includes lapacke_config.h
        .header(lapacke_include.join("lapacke_config.h").to_str().unwrap())
        .header(lapacke_include.join("lapacke.h").to_str().unwrap())
        
        // Include path for LAPACKE headers
        .clang_arg(format!("-I{}", lapacke_include.display()))
        
        // Only generate bindings for LAPACKE functions and types
        .allowlist_function("LAPACKE_.*")
        .allowlist_type("lapack_.*")
        .allowlist_type("LAPACK_.*")
        .allowlist_var("LAPACK_.*")
        
        // Use core::ffi types for C compatibility
        .ctypes_prefix("::core::ffi")
        
        // Use core when no_std
        .use_core()
        
        // Generate bindings for specific feature sets if enabled
        .allowlist_function(get_function_filter())
        
        // Don't generate bindings for system headers
        .derive_default(true)
        .derive_debug(true)
        
        // Layout tests can be large; only generate them in test builds
        .layout_tests(cfg!(test))
        
        // Use Rust naming conventions
        .prepend_enum_name(false)
        
        // Tell bindgen to use our lapack_int type instead of generating one
        .blocklist_type("lapack_int")
        
        // Generate the bindings
        .generate()
        .expect("Unable to generate bindings");
    
    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
    
    // Platform-specific linking configuration
    handle_platform_specific_linking();
    
    // The lapack-src crate and its backend dependencies handle the actual linking.
    // This build script ensures proper propagation of link directives.
}

/// Returns a regex pattern for functions to include based on enabled features
fn get_function_filter() -> &'static str {
    // For now, include all LAPACKE functions
    // In the future, we can filter based on feature flags
    "LAPACKE_.*"
}

/// Handle platform-specific linking requirements
fn handle_platform_specific_linking() {
    // Get the built LAPACK libraries from the repository
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let lapack_root = manifest_dir
        .parent().unwrap()  // rust/
        .parent().unwrap(); // repository root
    
    let lib_path = lapack_root.join("result").join("lib");
    
    if lib_path.exists() {
        // Link against the built LAPACK libraries
        println!("cargo:rustc-link-search=native={}", lib_path.display());
        println!("cargo:rustc-link-lib=lapacke");
        println!("cargo:rustc-link-lib=lapack");
        println!("cargo:rustc-link-lib=blas");
        
        // For dynamic linking, we also need to set the library path
        println!("cargo:rustc-link-arg=-Wl,-rpath,{}", lib_path.display());
    } else {
        // Fallback to system libraries or environment variables
        if let Ok(lib_dir) = env::var("LAPACK_LIB_DIR") {
            println!("cargo:rustc-link-search=native={}", lib_dir);
            println!("cargo:rustc-link-lib=lapacke");
            println!("cargo:rustc-link-lib=lapack");
            println!("cargo:rustc-link-lib=blas");
        } else {
            // Try pkg-config as last resort
            #[cfg(target_os = "linux")]
            {
                println!("cargo:rustc-link-lib=lapacke");
                println!("cargo:rustc-link-lib=lapack");
                println!("cargo:rustc-link-lib=blas");
            }
            
            #[cfg(target_os = "macos")]
            {
                if env::var("CARGO_FEATURE_ACCELERATE").is_ok() {
                    println!("cargo:rustc-link-lib=framework=Accelerate");
                } else {
                    println!("cargo:rustc-link-lib=lapacke");
                    println!("cargo:rustc-link-lib=lapack");
                    println!("cargo:rustc-link-lib=blas");
                }
            }
            
            #[cfg(target_os = "windows")]
            {
                println!("cargo:rustc-link-lib=lapacke");
                println!("cargo:rustc-link-lib=lapack");
                println!("cargo:rustc-link-lib=blas");
            }
        }
    }
}