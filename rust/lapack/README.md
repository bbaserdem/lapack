# lapack

Safe Rust wrappers for LAPACK (Linear Algebra PACKage).

This crate provides idiomatic Rust bindings to LAPACK routines, handling memory safety and error checking.

## Features

- Safe wrappers for common LAPACK operations
- Automatic error checking and conversion to Rust `Result` types
- Support for both row-major and column-major layouts
- Integration with the Rust numeric ecosystem

## Example

```rust
use lapack::{Layout, dgesv};

// Solve a system of linear equations Ax = b
let mut a = vec![
    1.0, 2.0,  // First column
    3.0, 4.0   // Second column
];
let mut b = vec![5.0, 11.0];
let mut ipiv = vec![0; 2];

// Solve the system
dgesv(Layout::ColumnMajor, 2, 1, &mut a, 2, &mut ipiv, &mut b, 2)?;

// b now contains the solution x
assert_eq!(b, vec![1.0, 2.0]);
```

## Backends

This crate requires a LAPACK implementation. Use `lapack-src` to select one:

```toml
[dependencies]
lapack = "0.1"
lapack-src = { version = "0.1", features = ["openblas"] }
```

## License

BSD-3-Clause