# Utility Toolkit

Seperate our lapack-util into; 

- A general fortran code analyzer called `fortran-mapper`.
- `fortran-mapper` would basically do the analysis we are doing on any FORTRAN codebase.
- It can take it configuration and hooks in a predefined manner,
we would abstract `lapack` specific behavior (filename processing) to hooks.

## Plan

We will define basic fortran code nodes.

- Function
- Subroutine
- Module
- Call
- Use
- Types/data structures
- File (filename, file path)

And link them on their own.

## Hooks

Custom hooks can introduce custom behavior; such as

- Determine extra properties (description, data type) from nodes (filename)
- We should have hooks that can add extra nodes too; such as data types in `lapack`.
- Hook into the extraction pipeline to include more information.
