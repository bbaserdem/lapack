# Brainlift

Brainlift on modernizing lapack.

## Problem Statement

Modern scientific computing demands new features—such as enhanced portability,
support for contemporary hardware (GPUs, ARM processors),
better usability (Python bindings, package managers),
and modern testing/integration practices—that are often lacking in legacy
codebases written in old programming languages.
LAPACK, a foundational linear algebra library written in Fortran,
is a prime example: it remains essential to the ecosystem,
yet its architecture and tooling pose obstacles to adopting contemporary
features and development practices.

## Research

### Github issues

- **Cross-Platform Build Failures:**  
  - Issues building and testing on ARM64 and Apple Silicon hardware.
  - Frequent failures with new Linux/macOS architectures.

- **Integration Friction:**  
  - Problems with CMake integration and downstream consumption via
    `FetchContent_Declare` and `find_package`.
  - Missing headers (e.g., `lapacke_64.h`) complicating third-party use.

- **Linking and Argument Limitations:**  
  - Linker errors such as "argument list too long" when building certain
    LAPACK components.
  - Build system limits exposed on complex projects.

- **Inconsistent Numerical Results:**  
  - Variations in output across routines like `SLARUV` versus `SLARND`.
  - Concerns over backward compatibility due to small but impactful
    numerical changes.

- **Test Failures and Build Flag Issues:**  
  - Test failures triggered by aggressive compiler optimizations
    or particular BLAS backends.
  - Unreliable results on new hardware or with specialized build settings.

- **Ecosystem Pressure:**  
  - Dependency from NumPy, R, MATLAB, and other libraries keeps stability
    needs high.
  - Modern users expect pip/conda/package manager support,
    which is incomplete or inconsistent.

- **Modernization Gaps:**  
  - Wrappers, bindings, and attempts at supporting GPUs and multi-core
    CPUs exist but are behind hardware/industry trends.
  - Continuous integration and automated testing are less robust than in
    younger open-source projects.

- **User Experience Shortcomings:**  
  - Lack of Pythonic/idiomatic APIs, poor documentation for modern workflows,
    and confusion over configuration options compared to more recently developed
    scientific software.

### Uses

- MATLAB and GNU Octave: MATLAB’s core matrix operations and solvers are
built on LAPACK routines, enabling fast solutions for linear systems,
eigenproblems, and decompositions.

- NumPy/SciPy (Python): These Python libraries use LAPACK (often via wrappers) 
for array operations involving linear algebra, eigendecomposition, 
and singular value decomposition.

- R: The statistical computing environment utilizes LAPACK for core operations
like regression, PCA, SVD, and matrix factorization.

- Intel Math Kernel Library (MKL) and AMD Core Math Library (ACML):
Provide highly optimized implementations and interfaces to LAPACK routines.

- Eigen (C++): Though it has its own algorithms, Eigen can interface with
LAPACK for certain advanced operations.

- LAPACK++: A C++ wrapper and interface for LAPACK functionalities

- Virtual Client and benchmarking suites: Use LAPACK to measure compute
performance for various matrix and numerical routines

- High Performance Computing (HPC) applications: Many domain-specific and
engineering simulation tools link LAPACK directly for linear solvers.

- Database environments: Some database engines (e.g., via aggregate
user-defined functions in SQL or stored procedures) implement advanced
analytics by invoking LAPACK routines for things like large-scale
PCA and SVD

- Other technical and engineering applications: Finite element analysis (FEA),
signal processing, and computational physics codes often link directly to
LAPACK for problem-specific solvers

### Most Requested Unimplemented Features in LAPACK

1. Native Support for Sparse Matrices
  - LAPACK focuses on dense and banded matrices;
general sparse matrix routines are still not available,
although they are critical in scientific computing and
large-scale simulation.

2. GPU and Accelerator Integration
  - Direct support for GPUs, FPGAs, or heterogeneous architectures is missing.
  Users want seamless acceleration with CUDA, HIP, or SYCL for modern hardware
  environments.

3. Modern Language Interfaces and Bindings
  - First-class, native bindings for Python, Julia, and Rust.
  - Easier, better-documented C/C++ interfaces (beyond LAPACKE),
with robust ABI compatibility.

4. Mixed-Precision and Arbitrary Precision Arithmetic
  - Built-in support for mixed-precision algorithms or arbitrary/higher/lower
floating-point precision, which are increasingly important for AI and HPC
applications.

5. Asynchronous and Batched/Streamed Operations
  - Routines that allow for non-blocking, batched, or streamed computations
especially useful in data pipelines and parallel simulation.

6. Automatic Performance Tuning Tools
  - Tools for auto-tuning block sizes, algorithm choices,
and thresholds to optimize performance for any given hardware without
manual intervention.

7. Dynamic Memory Management and Workspace Allocation
  - High-level APIs that handle workspace memory automatically or
use modern dynamic allocation, reducing the burden of managing parameters
such as `WORK` arrays.

8. Enhanced Error Handling and Diagnostics
  - More informative error messages, exception hierarchies,
and runtime diagnostics rather than cryptic error codes.

9. Improved Algorithms & Functional Expansion
  - Newer, highly accurate algorithms for decomposition, eigenproblems,
or iterative refinement.
  - Routines for explicit matrix inversion and broader condition number
estimation.
  - Native support for updating/downdating of matrix factorizations and
additional eigenproblems.

10. Comprehensive, User-Friendly Documentation & Tutorials
  - Modernized documentation with beginner-friendly guides,
migration advice, and practical code examples in multiple programming languages.

11. Advanced Matrix Layout and Storage Options
  - Native handling for advanced matrix layouts (e.g., hierarchical,
tile, and block-sparse formats).
- Built-in conversion routines for interoperability with modern distributed
and storage-efficient formats.

12. Robust Continuous Integration & Testing Infrastructure
  - Expanded automated testing, platform coverage, and CI pipelines to
quickly catch numerical or compatibility issues in diverse environments.

**Summary Table: Key Unimplemented LAPACK Features**

| Feature                                 | Description                                                        |
|------------------------------------------|--------------------------------------------------------------------|
| Sparse matrix support                    | Efficient routines for arbitrary sparse matrices                    |
| GPU/Accelerator integration              | Direct kernels for GPUs, FPGAs, and heterogenous hardware          |
| Modern language interfaces               | High-level bindings for Python, Julia, Rust, modern C++            |
| Mixed/arbitrary precision arithmetic     | Built-in support for mixed and non-standard precision              |
| Asynchronous/batched routines            | Non-blocking and batched interface options                         |
| Auto-tuning/performance tooling          | Automated hardware adaptation and tuning utilities                 |
| Dynamic memory management                | Eliminate manual workspace parameters in high-level APIs           |
| Enhanced diagnostics/error handling      | More helpful error and exception handling                          |
| Expanded and modernized algorithms       | New numerical methods and updated factorizations                   |
| Improved documentation                   | Practical, accessible, and modern tutorials and guides             |


## Truths and Beliefs

## Truths

- **Large language models help parse and summarize undocumented or complex
legacy code.**
- **AI-powered code review can spot style errors, boilerplate, and common
patterns requiring cleanup.**
- **Modern AI solutions often fall short on deep system knowledge without
expert guidance; AI emulates reasoning through language, not true reasoning.**
- **AI occasionally invents plausible but incorrect code when working with
obsolete or unfamiliar APIs.**
- **Most effective results come from combining AI automation with expert
validation and benchmarking.**
- **AI can assist with surface-level modernization (e.g., wrapping APIs),
but struggles with intricate, system-specific requirements.**
- **Human oversight remains essential for correctness, performance,
and reliability.**
- **Easy wins: AI can help generate tests, catch deprecated idioms,
and fill documentation gaps quickly.**
- **AI can represent the inner workings of a codebase better than some
individual developers, aiding onboarding and comprehension for new contributors.**
- **There is optimism around AI-accelerated onboarding and comprehension
for new contributors to old projects.**

## Beliefs

- **AI tools can easily automate code refactoring, bug detection,
and basic optimizations in old codebases.**
(People claim AI is poor at this, but with sufficient training on legacy
code it could improve.)
- **AI is not good on legacy code due to lack of relevant training—though
this may be bias from a focus on modern codebases.**
- **Some expect AI to fully automate updating old code for new hardware
(GPUs, ARM, etc.), but this is not happening in practice.**
- **AI-generated code is always error-prone or incomplete for highly
performance-sensitive scenarios; however, AI can be pushed further to
optimize code.**
- **At scale, AI struggles with massive, custom build systems typical
in mature scientific codebases, but this might be overcome with better models.**
- **AI tools may not account for historical architectural decisions
or hidden dependencies, but this remains unproven as a fundamental limitation.**

## Spiky POVs

- AI will be used to recover and migrate obsolete code.
- AI should be trained on old software; COBOL, FORTRAN, C etc. to do this.
This has more utility than python.
