# lapack-src

LAPACK source provider for Rust.

This crate allows you to select which LAPACK implementation to link against through Cargo features.

## Available Backends

- `netlib`: Reference implementation from Netlib
- `openblas`: OpenBLAS (includes optimized BLAS)
- `intel-mkl`: Intel Math Kernel Library

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
lapack = "0.1"
lapack-src = { version = "0.1", features = ["openblas"] }
```

You must enable exactly one feature to select the backend.

## Backend Comparison

| Backend | Performance | Size | License | Platforms |
|---------|------------|------|---------|-----------|
| netlib | Baseline | Small | BSD | All |
| openblas | Optimized | Medium | BSD | Most |
| intel-mkl | Highly Optimized | Large | Proprietary | x86/x64 |

## License

BSD-3-Clause (crate itself)

Note: The selected backend may have different licensing terms.