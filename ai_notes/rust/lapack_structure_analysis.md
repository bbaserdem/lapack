# LAPACK Directory Structure Analysis

## Overview
This document provides a comprehensive mapping of the LAPACK directory structure, identifying key directories and their purposes for Rust FFI bindings development.

## Top-Level Structure

### Core Source Directories

#### `/BLAS/`
- **Purpose**: Basic Linear Algebra Subprograms (BLAS) implementation
- **Key subdirectories**:
  - `SRC/`: BLAS source code (Fortran)
  - `TESTING/`: BLAS test suite
  - `SRC/DEPRECATED/`: Deprecated BLAS routines

#### `/CBLAS/`
- **Purpose**: C interface to BLAS routines
- **Key subdirectories**:
  - `include/`: CBLAS header files (cblas.h)
  - `src/`: C wrapper implementations
  - `testing/`: CBLAS test programs
  - `examples/`: CBLAS usage examples
  - `cmake/`: CBLAS-specific CMake configuration

#### `/LAPACKE/` ⭐ (Primary focus for Rust FFI)
- **Purpose**: C interface to LAPACK routines
- **Key subdirectories**:
  - `include/`: LAPACKE header files (lapacke.h, lapacke_config.h, lapacke_utils.h)
  - `src/`: C wrapper implementations for LAPACK routines
  - `utils/`: Utility functions for LAPACKE
  - `example/`: LAPACKE usage examples
  - `mangling/`: Name mangling utilities
  - `cmake/`: LAPACKE-specific CMake configuration

#### `/SRC/`
- **Purpose**: Core LAPACK Fortran source code
- **Key subdirectories**:
  - Main directory: Contains all LAPACK Fortran routines (.f files)
  - `DEPRECATED/`: Deprecated LAPACK routines
  - `VARIANTS/`: Algorithm variants for specific routines
    - `cholesky/`: Cholesky factorization variants
    - `lu/`: LU decomposition variants
    - `qr/`: QR factorization variants
    - `larft/`: Triangular factor computation variants

### Testing and Quality Assurance

#### `/TESTING/`
- **Purpose**: Comprehensive LAPACK test suite
- **Key subdirectories**:
  - `EIG/`: Eigenvalue problem tests
  - `LIN/`: Linear equation solver tests
  - `MATGEN/`: Matrix generation utilities for testing

### Build System

#### `/CMAKE/`
- **Purpose**: CMake modules and configuration files
- **Contains**: FindBLAS.cmake, FindLAPACK.cmake, and other CMake utilities

#### Build Configuration Files
- `CMakeLists.txt`: Main CMake configuration
- `lapack_build.cmake`: LAPACK-specific build configuration
- `Makefile`: Alternative make-based build system
- `make.inc.example`: Example configuration for make builds

### Documentation

#### `/DOCS/` and `/docs/`
- **Purpose**: LAPACK documentation
- Contains user guides, API documentation, and technical references

#### `/INSTALL/`
- **Purpose**: Installation guides and platform-specific build instructions

### Supporting Files

#### `/examples/`
- **Purpose**: Example programs demonstrating LAPACK usage

#### `/scripts/`
- **Purpose**: Build and maintenance scripts

#### `/data/`
- **Purpose**: Test data files

## Key Files for Rust FFI Development

### Primary Focus Areas
1. **LAPACKE Headers** (`/LAPACKE/include/`)
   - `lapacke.h`: Main LAPACKE interface
   - `lapacke_config.h`: Configuration and type definitions
   - `lapacke_utils.h`: Utility function declarations

2. **Build Configuration**
   - `CMakeLists.txt`: Understanding build options and dependencies
   - `/CMAKE/`: CMake modules for finding LAPACK/BLAS

3. **Source Reference**
   - `/SRC/`: For understanding routine implementations
   - `/LAPACKE/src/`: For understanding C wrapper patterns

## Directory Purposes Summary

| Directory | Purpose | Relevance to Rust FFI |
|-----------|---------|----------------------|
| `/LAPACKE/` | C interface to LAPACK | **Critical** - Primary FFI target |
| `/SRC/` | LAPACK Fortran source | Reference for routine behavior |
| `/CBLAS/` | C interface to BLAS | Important for BLAS dependencies |
| `/BLAS/` | BLAS Fortran source | Understanding BLAS integration |
| `/TESTING/` | Test suites | Test case reference |
| `/CMAKE/` | Build configuration | Build system understanding |

## Next Steps
1. Deep dive into LAPACKE headers to understand the C interface
2. Analyze CMake build configuration for LAPACKE
3. Identify core LAPACKE routines for initial Rust bindings