# Workflow

Main workflow chart, with each feature in a given stage.

## Stage 0: Infrastructure Setup
- **devops/docker2nix**: Convert Docker-based build system to Nix for reproducible builds
- Establish core build infrastructure
- Merge foundational changes to master

## Stage 1: Organization and Analysis
- **organize/map**: Generate comprehensive organizational map of LAPACK codebase
  - Parse all Fortran source files using `fortran-src` to extract AST
  - Extract procedure definitions, call relationships, and file locations
  - Design graph schema for routine dependencies and relationships
  - Process LAPACK naming conventions (precision prefixes: S/D/C/Z)
  - Create importable format for graph database (Neo4j)
  - Handle parser errors and edge cases gracefully
  - Generate visualization of code structure and dependencies
- **organize/docs**: Restructure and improve documentation
- **organize/reorg**: Reorganize code structure based on analysis
- **organize/tests**: Ensure test coverage through reorganization

## Stage 2: Feature Development
- **feature/docs**: Enhanced documentation system
- **feature/sparse**: Sparse matrix support improvements
- **feature/rust-binding**: Rust language bindings
- **feature/partial-comp**: Partial compilation capabilities

```mermaid
flowchart TD
  %% Root repo
  main(["main"])

  %% Milestones
  stage0{{"0"}}
  stage1{{"1"}}
  stage2{{"2"}}

  %% Main branches
  devops["devops"]
  master["master"]
  organize["Organize"]
  feature["feature"]

  %% Sub-branches (circles)
  docker2nix(["devops/docker2nix"])
  map(["map"])
  docs1(["docs"])
  reorg(["reorg"])
  tests(["tests"])

  %% Feature sub-branches
  docs2(["docs"])
  sparse(["sparse"])
  rustbinding(["rust-binding"])
  partialcomp(["partial-comp"])

  %% Structure
  main --> stage0
  stage0 --> devops --> docker2nix
  docker2nix --> master

  main --> master

  master --> stage1
  stage1 --> organize
  organize --> map --> docs1 --> reorg --> tests
  organize --> docs1

  tests --> master

  master --> stage2
  stage2 --> feature --> docs2 --> sparse --> rustbinding --> partialcomp

  %% Show docs2 links to all
  docs2 --> rustbinding
  docs2 --> partialcomp
```
