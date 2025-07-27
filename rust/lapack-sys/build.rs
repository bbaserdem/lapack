//! Build script for lapack-sys
//!
//! This script handles linking to the LAPACK library selected by lapack-src.
//! The actual linking is done by the backend provider (netlib-src, openblas-src, etc.).

fn main() {
    // The lapack-src crate and its backend dependencies handle the actual linking.
    // This build script ensures proper propagation of link directives.
    
    // Print cargo directives for documentation
    println!("cargo:rerun-if-changed=build.rs");
    
    // Note: The actual linking is handled by the selected backend in lapack-src.
    // For example:
    // - netlib-src will link liblapack
    // - openblas-src will link libopenblas (which includes LAPACK)
    // - intel-mkl-src will link MKL libraries
    
    // Platform-specific considerations
    #[cfg(target_os = "windows")]
    {
        // On Windows, we may need to handle different library naming conventions
        // This is typically handled by the backend crates
    }
    
    #[cfg(target_os = "macos")]
    {
        // On macOS, we could optionally use Accelerate framework
        // This would be handled by an accelerate-src backend
    }
    
    #[cfg(target_os = "linux")]
    {
        // Linux typically uses standard library paths
        // Backend crates handle pkg-config or manual specification
    }
}