# LAPACK CMake Build System Analysis

## Overview
This document analyzes the LAPACK CMake build system with a focus on understanding how LAPACKE is built and integrated.

## Key Build Configuration

### Version Information
- LAPACK Version: 3.12.1
- CMake Minimum Version: 3.13
- C Language enabled for LAPACKE

### Library Names
The build system supports two naming schemes:
- **Standard (32-bit integers)**: `blas`, `lapack`, `lapacke`, `cblas`
- **Index-64 (64-bit integers)**: `blas64`, `lapack64`, `lapacke64`, `cblas64`

### Build Options

#### Core Build Options
```cmake
BUILD_SHARED_LIBS    # Build shared libraries (default: OFF)
BUILD_INDEX64        # Build Index-64 API libraries (default: OFF)
BUILD_INDEX64_EXT_API # Build Index-64 API with _64 suffix (default: ON for CMake >= 3.18)
BUILD_TESTING        # Build tests (default: OFF)
BUILD_DEPRECATED     # Build deprecated routines (default: OFF)
```

#### Precision Options
```cmake
BUILD_SINGLE         # Build single precision real (default: ON)
BUILD_DOUBLE         # Build double precision real (default: ON)
BUILD_COMPLEX        # Build single precision complex (default: ON)
BUILD_COMPLEX16      # Build double precision complex (default: ON)
```

#### LAPACKE-Specific Options
```cmake
LAPACKE              # Build LAPACKE C interface (default: OFF)
LAPACKE_WITH_TMG     # Build LAPACKE with tmglib routines (default: OFF)
LAPACKE_BUILD_SINGLE # Build LAPACKE single precision real (default: ON)
LAPACKE_BUILD_DOUBLE # Build LAPACKE double precision real (default: ON)
LAPACKE_BUILD_COMPLEX # Build LAPACKE single precision complex (default: ON)
LAPACKE_BUILD_COMPLEX16 # Build LAPACKE double precision complex (default: ON)
```

## LAPACKE Build Process

### 1. Enable LAPACKE
To build LAPACKE, set the CMake option:
```bash
cmake -DLAPACKE=ON ..
```

### 2. LAPACKE Dependencies
- Requires LAPACK library to be built first
- Optionally includes TMGLIB (test matrix generation library)
- Links against `${LAPACK_LIBRARIES}`

### 3. LAPACKE Source Organization
The LAPACKE build collects sources from:
- `/LAPACKE/src/`: Main LAPACKE wrapper implementations
- `/LAPACKE/utils/`: Utility functions
- `/LAPACKE/include/`: Header files

Sources are categorized by precision:
- `SOURCES_SINGLE`: Single precision real routines
- `SOURCES_DOUBLE`: Double precision real routines
- `SOURCES_COMPLEX`: Single precision complex routines
- `SOURCES_COMPLEX16`: Double precision complex routines

### 4. Build Targets

#### Object Library
```cmake
${LAPACKELIB}_obj    # Position-independent code object library
${LAPACKELIB}_64_obj # 64-bit integer API object library (if enabled)
```

#### Main Library
```cmake
${LAPACKELIB}        # Main LAPACKE library (static or shared)
```

### 5. Installation

#### Headers Installation
```
${CMAKE_INSTALL_INCLUDEDIR}/
├── lapacke.h
├── lapacke_config.h
├── lapacke_utils.h
└── lapacke_mangling.h
```

#### Library Installation
```
${CMAKE_INSTALL_LIBDIR}/
├── liblapacke.a (or .so/.dll)
└── pkgconfig/
    └── lapacke.pc
```

#### CMake Config Files
```
${CMAKE_INSTALL_LIBDIR}/cmake/lapacke-${VERSION}/
├── lapacke-config.cmake
├── lapacke-config-version.cmake
└── lapacke-targets.cmake
```

## Build Flags and Definitions

### Windows-Specific
```c
HAVE_LAPACK_CONFIG_H
LAPACK_COMPLEX_STRUCTURE
```

### 64-bit Integer API
```c
LAPACK_ILP64
LAPACKE_API64
WeirdNEC
CBLAS_API64
```

## Key CMake Variables for Rust FFI

### Important Variables
- `LAPACKE_INCLUDE`: List of LAPACKE header files
- `LAPACK_LIBRARIES`: LAPACK library dependencies
- `LAPACK_VERSION`: Version string
- `CMAKE_INSTALL_PREFIX`: Installation root

### Build Artifacts
- Headers: `${LAPACK_BINARY_DIR}/include/`
- Libraries: `${LAPACK_BINARY_DIR}/lib/`
- Binaries: `${LAPACK_BINARY_DIR}/bin/`

## Recommendations for Rust FFI

### 1. Minimal Build Command
```bash
cmake -DLAPACKE=ON -DBUILD_TESTING=OFF -DCMAKE_BUILD_TYPE=Release ..
make lapacke
```

### 2. Static Linking Build
```bash
cmake -DLAPACKE=ON -DBUILD_SHARED_LIBS=OFF ..
```

### 3. Required Files for FFI
- Headers: `lapacke.h`, `lapacke_config.h`, `lapacke_utils.h`
- Library: `liblapacke.a` or `liblapacke.so`
- Dependencies: LAPACK and BLAS libraries

### 4. pkg-config Integration
The build generates `lapacke.pc` for easy integration:
```bash
pkg-config --cflags --libs lapacke
```

## Next Steps
1. Examine LAPACKE header files to understand the C API
2. Identify naming conventions and type mappings
3. Create Rust FFI bindings based on LAPACKE interface