# Product Requirements Document (PRD)  

## Modernizing LAPACK for Enhanced Performance and Usability  
**Project Duration:** Tuesday, July 22, 2025 (evening) — Sunday, July 27, 2025 (evening)  
**Team Approach:** AI-assisted development with active human input

## Objective

Modernize the LAPACK codebase—entirely in **Fortran**—to increase modularity, improve testability and documentation, introduce new build systems, expand functionality (sparse matrices), and open integration paths for modern tools (Rust, Nix, Meson), while making the codebase more maintainable and accessible for both humans and AI[1][2].

## Feature Roadmap & Timeline


| Day | Task & Deliverable                                                                                                           |
|-----|-----------------------------------------------------------------------------------------------------------------------------|
| Wed | **Step 0:** Create a Fortran 2008 AI-friendly documentation guide (formatting standards, annotation protocol).              |
| Wed-Thu | **(1) Codebase Reorganization:**- Map and analyze codebase structure.- Construct a logical graph of routines and data flow.- Propose and start refactoring into logical submodules and subroutines.                                                   |
| Fri | **(2) Improved Testing & Coverage:**- Research Fortran testing tools (e.g., pFUnit, f90Unit, FRUIT).- Integrate basic test scaffold and coverage tool.- Extend with new/augmented tests.                                         |
| Sat AM | **(3) Build Systems:**- Implement Meson build configuration.- Build Nix flake for deterministic builds and developer environments[2].- Create Dockerfiles for cross-platform builds and CI.                                   |
| Sat PM | **(4) Sparse Matrix Support:**- Research optimal Fortran sparse matrix strategies (MATLAB, BLAS flanks).- Design core routines for sparse matrix multiplication.- Assess and prototype Fortran integration.                      |
| Sun AM | **(5) Rust Integration:**- Define C/Fortran interface boundaries for FFI.- Start a simple Rust wrapper crate (using FFI, `bindgen` or manual bindings).                                                         |
| Sun PM | **(6) API Documentation:**- Identify key entry points and public routines.- Write human-focused documentation and AI-friendly inline docstrings.- Visualize codebase modules with Mermaid and flow charts.                      |
| Sun EOD | **(7) Type Definitions Exploration:**- Investigate modern Fortran type annotations and conventions.- Document findings and apply where feasible.- Summarize unknowns for further research.                                      |


## Milestones

- **Fortran AI Doc Standard**: Complete Wednesday
- **Codebase graph and heavy refactoring plan**: Thursday
- **Initial test suite and coverage utility running**: Friday EOD
- **Meson, Nix, and Docker integration**: Saturday midday
- **First sparse matrix and Rust API prototypes**: Sunday midday
- **API docs and diagrams ready for project wrap-up**: Sunday evening

## Additional Notes

- **All progress and major bottlenecks should be documented for both human and AI consumption.**
- Sparse matrix work is a "reach" feature—may only reach prototype or API spec stage.
- Rust integration may be limited to proof of concept/wrapper skeleton, dependent on available Fortran/C ABI compatibility and team member familiarity.
- Type definitions in Fortran may involve modern `type` constructs or documentation of existing variable structures.
- Prioritize changes that enable clearer structure and maintainability—incremental PRs preferred for traceability.
- Use Nix for toolchain/troubleshooting to ensure reproducibility.

## Risks & Mitigations

- **Legacy system quirks may slow automated refactors**: Schedule human check-ins after each AI-assisted restructuring.
- **Sparse matrix and Rust tasks may require longer research**: Time-box research phases; build stubs if behind.
- **Testing and build integration unfamiliarity**: Leverage community forums and docs; document process for future contributors.

If adjustments or prioritization are needed as the project unfolds, address them in daily standups/check-ins.
