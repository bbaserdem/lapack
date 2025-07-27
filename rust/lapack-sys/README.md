# lapack-sys

Low-level FFI bindings to LAPACK (Linear Algebra PACKage).

This crate provides raw, unsafe bindings to LAPACK functions through the LAPACKE C interface. These bindings are automatically generated from LAPACKE headers using bindgen.

## Build Requirements

This crate uses bindgen to generate FFI bindings at build time. This requires:
- Clang/LLVM (for bindgen to parse C headers)
- LAPACKE headers (included in the LAPACK source tree)

## Usage

Most users should use the higher-level `lapack` crate instead, which provides safe wrappers around these FFI bindings.

```toml
[dependencies]
lapack-sys = "0.1"
```

## Safety

All functions in this crate are `unsafe` because they:
- Accept raw pointers without validity checks
- Require correct array dimensions
- Assume column-major (Fortran) memory layout
- Do not validate input parameters

## Features

This crate provides bindings for:
- Driver routines (GESV, GELS, etc.)
- Computational routines
- Auxiliary routines

## License

Same as LAPACK - BSD-3-Clause