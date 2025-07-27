# Rust LAPACK Bindings

This project provides Rust bindings to LAPACK (Linear Algebra PACKage), offering both low-level FFI access and high-level safe abstractions.

## Project Structure

We have adopted a **modular crate structure** following established Rust conventions:

### Crates

- **`lapack-sys`**: Low-level FFI bindings to LAPACK functions
  - Raw, unsafe bindings
  - Minimal dependencies
  - Direct mapping to LAPACK C/Fortran API

- **`lapack`**: High-level safe Rust wrappers
  - Idiomatic Rust API
  - Comprehensive error handling
  - Type-safe matrix operations

- **`lapack-src`**: Backend provider selection
  - Allows choosing between different LAPACK implementations
  - Supports netlib, OpenBLAS, Intel MKL, etc.

## Design Decision: Modular Structure

After careful analysis (see `crate_structure_analysis.md`), we chose a modular structure for the following reasons:

1. **Separation of Concerns**: Clear boundary between unsafe FFI and safe abstractions
2. **Flexibility**: Users can choose to use raw FFI or safe wrappers
3. **Maintainability**: Changes to FFI don't affect high-level API
4. **Industry Standard**: Follows the established `-sys` crate pattern
5. **Backend Flexibility**: Easy to swap LAPACK implementations

## Getting Started

### For Application Developers

Add this to your `Cargo.toml`:

```toml
[dependencies]
lapack = "0.1"
lapack-src = { version = "0.1", features = ["openblas"] }
```

### For Library Authors

If you only need raw FFI:

```toml
[dependencies]
lapack-sys = "0.1"
```

## Project Status

This project is currently in early development. See the task list for progress on individual components.

## Contributing

Contributions are welcome! Please see our contributing guidelines.

## License

This project is licensed under the same terms as LAPACK itself.