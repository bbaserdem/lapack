# Fortran Mapper LAPACK Hooks

LAPACK-specific hooks for the fortran-mapper library.

## Installation

```bash
# Using uv (recommended)
uv pip install fortran-mapper-hooks-lapack

# For development
uv pip install -e .
```

## Overview

This package provides LAPACK domain knowledge to the generic fortran-mapper, including:

- **LAPACK Naming Convention Recognition**: Understands precision prefixes (S/D/C/Z), routine categories (GE/SY/PO), etc.
- **Node Enrichment**: Adds LAPACK-specific metadata to routine and file nodes
- **Custom Node Creation**: Creates precision, operation type, and computational category nodes
- **Relationship Mapping**: Establishes relationships based on LAPACK computational patterns

## Usage

```python
from fortran_mapper import FortranMapper
from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator

mapper = FortranMapper()
mapper.register_hook("node_enricher", LapackNodeEnricher())
mapper.register_hook("node_creator", LapackNodeCreator())

graph = mapper.parse_directory("path/to/lapack/src")
```

## Dependencies

- `fortran-mapper>=0.1.0`: Core parsing functionality

## License

MIT License
