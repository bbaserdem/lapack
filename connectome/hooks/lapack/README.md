# Connectome Hooks - LAPACK

LAPACK-specific hooks for connectome analysis.

## Overview

This package combines LAPACK domain expertise with connectome analysis to create rich computational connectomes:

- **LAPACK Computational Connectomes**: Specialized analysis for LAPACK routine relationships
- **Enhanced Node Types**: LAPACK-specific node enrichment and categorization
- **Computational Patterns**: Recognition of LAPACK computational patterns and dependencies
- **Domain Integration**: Seamless integration of LAPACK knowledge with connectome framework

## Features

- LAPACK routine categorization and analysis
- Precision-aware connectome construction
- Computational dependency mapping
- LAPACK-specific visualization and export

## Usage

```python
from connectome_hooks_lapack import LapackConnectomeAnalyzer

analyzer = LapackConnectomeAnalyzer()
connectome = analyzer.build_lapack_connectome("path/to/lapack/src")

# Export LAPACK-specific connectome
analyzer.export_to_neo4j(connectome, driver)
```

## Dependencies

- `connectome-hooks-fortran>=0.1.0`: Generic Fortran connectome foundation
- `fortran-mapper>=0.1.0`: Core Fortran parsing
- `networkx>=2.8`: Graph algorithms

## License

MIT License
