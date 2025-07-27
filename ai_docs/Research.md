# Research: Existing Rust LAPACK Crates Best Practices

## Overview

This document contains findings from researching existing Rust LAPACK crates to identify best practices for FFI bindings, memory management, and error handling. The research covers several key crates in the Rust LAPACK ecosystem.

## Key Crates Analyzed

### 1. lapack-sys
- **Purpose**: Low-level FFI bindings to LAPACK (Fortran)
- **Repository**: https://github.com/blas-lapack-rs/lapack-sys

#### Key Implementation Details:
- **Code Generation**: Bindings are generated via shell script from LAPACK submodule
- **Architecture**: Follows standard `-sys` crate conventions - raw, unsafe bindings only
- **Backend Integration**: Works with `lapack-src` for linking to actual implementations
- **No High-Level Abstractions**: Pure FFI bindings without safe wrappers

### 2. ndarray-linalg
- **Purpose**: High-level linear algebra package for ndarray
- **Repository**: https://github.com/rust-ndarray/ndarray-linalg

#### Design Patterns:
- **Trait-Based API**: Each routine exposed as traits with ownership variants
  - `QRSquare` (immutable reference)
  - `QRSquareInplace` (mutable reference)
  - `QRSquareInto` (by value)
- **Feature-Based Backend Selection**: Uses Cargo features for backend choice
- **Modular Design**: Separates core array operations from linear algebra
- **Zero-Copy Views**: Supports slicing and views without copying data
- **Type System Safety**: Compile-time dimension checking where possible

### 3. openblas-sys/openblas-src
- **Purpose**: Build configuration and linking for OpenBLAS
- **Repository**: https://github.com/blas-lapack-rs/openblas-src

#### Build Configuration:
- **Environment Variables**: 
  - `OPENBLAS_CC`, `OPENBLAS_FC`, `OPENBLAS_HOSTCC`, `OPENBLAS_TARGET`
  - Prefixed to avoid conflicts
- **Cross-Compilation Support**: Auto-detection of compilers
- **Cache Feature**: Reuses build products across crates
- **Platform Considerations**: Windows requires system feature (no source builds)

### 4. lapacke-sys/lapacke
- **Purpose**: C API bindings and safe wrappers
- **Pattern**: Two-tier architecture
  - `lapacke-sys`: Low-level unsafe C bindings
  - `lapacke`: Higher-level safe Rust wrapper

## Best Practices Identified

### FFI Binding Patterns

1. **Two-Tier Architecture**:
   ```
   Application
       ↓
   Safe Wrapper Crate (e.g., lapack, ndarray-linalg)
       ↓
   -sys Crate (e.g., lapack-sys, lapacke-sys)
       ↓
   Native Library (LAPACK/OpenBLAS)
   ```

2. **Code Generation**:
   - Use scripts (shell/Python) to generate bindings from source
   - Consider `bindgen` for C header processing
   - Maintain reproducible generation process

3. **Backend Flexibility**:
   - Use feature flags for backend selection
   - Support multiple implementations (OpenBLAS, Intel MKL, netlib)
   - Libraries should not link backends directly

### Memory Management Patterns

1. **Column-Major vs Row-Major**:
   - Fortran uses column-major layout
   - Provide layout conversion utilities or document requirements

2. **Ownership Semantics**:
   - Clear ownership variants in API (borrowed, mutable, owned)
   - Use Rust's type system to enforce memory safety

3. **Zero-Copy Operations**:
   - Support views and slicing without data duplication
   - Leverage Rust's borrowing for efficient operations

4. **Array Safety**:
   - Validate dimensions and sizes at API boundaries
   - Use slice patterns instead of raw pointers where possible

### Error Handling Approaches

1. **Result-Based APIs**:
   - Convert LAPACK error codes to `Result<T, E>`
   - Define domain-specific error types

2. **Error Types**:
   ```rust
   pub enum LapackError {
       InvalidParameter(i32),
       SingularMatrix,
       ConvergenceFailure,
       // ... other LAPACK-specific errors
   }
   ```

3. **Panic vs Error**:
   - Panic for programmer errors (invalid API usage)
   - Return `Result` for runtime failures

### Build and Linking Strategies

1. **Environment Configuration**:
   - Support standard environment variables
   - Provide clear documentation for build requirements

2. **Feature Flags**:
   ```toml
   [features]
   default = []
   system = []        # Use system libraries
   static = []        # Static linking
   cache = []         # Cache build artifacts
   ```

3. **Cross-Compilation**:
   - Auto-detect target architecture
   - Support compiler override via environment

4. **Dependency Management**:
   - Separate source providers from bindings
   - Allow users to choose implementation

## Recommendations for Rust LAPACK Bindings

### Architecture Recommendation

```
lapack-rust/
├── lapack-sys/          # Raw FFI bindings
│   ├── src/
│   │   └── lib.rs       # extern "C" declarations
│   └── build.rs         # Link configuration
├── lapack-src/          # Backend provider
│   └── src/
│       └── lib.rs       # Feature-based backend selection
└── lapack/              # Safe wrapper
    ├── src/
    │   ├── lib.rs       # Public API
    │   ├── error.rs     # Error types
    │   └── traits.rs    # Operation traits
    └── examples/
```

### API Design Guidelines

1. **Trait-Based Operations**:
   ```rust
   pub trait Solve<RHS> {
       type Output;
       fn solve(&self, rhs: &RHS) -> Result<Self::Output, LapackError>;
   }
   
   pub trait SolveInplace<RHS> {
       fn solve_inplace(&mut self, rhs: &mut RHS) -> Result<(), LapackError>;
   }
   ```

2. **Builder Pattern for Complex Operations**:
   ```rust
   let result = SvdBuilder::new()
       .compute_u(true)
       .compute_vt(true)
       .build()
       .decompose(&matrix)?;
   ```

3. **Type Safety**:
   - Use phantom types for matrix properties
   - Encode constraints in type system where possible

### Testing Strategy

1. **Known Test Cases**: Port LAPACK test cases
2. **Property Testing**: Use `proptest` or `quickcheck`
3. **Numerical Accuracy**: Compare with reference implementations
4. **Memory Safety**: Run under valgrind/sanitizers

## Conclusion

The Rust LAPACK ecosystem demonstrates mature patterns for FFI integration. Key takeaways:
- Separate low-level bindings from safe abstractions
- Use code generation for maintainability
- Provide flexible backend selection
- Prioritize type safety and zero-cost abstractions
- Follow Rust idioms for error handling and API design

These patterns provide a solid foundation for creating robust Rust bindings to LAPACK.