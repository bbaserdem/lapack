# Connectome Hooks - Fortran

Generic Fortran hooks for connectome analysis.

## Overview

This package provides the foundational Fortran analysis capabilities for connectome construction:

- **Generic Fortran Analysis**: Language-level parsing and analysis
- **Connectome Foundation**: Basic connectome node and relationship patterns
- **Extensible Framework**: Base classes for domain-specific extensions
- **Integration Layer**: Bridges fortran-mapper with connectome-specific analysis

## Features

- Generic Fortran routine analysis
- File-level dependency mapping
- Basic call graph construction
- Extensible hook architecture for domain-specific analysis

## Usage

```python
from connectome_hooks_fortran import FortranConnectomeAnalyzer

analyzer = FortranConnectomeAnalyzer()
connectome = analyzer.build_connectome("path/to/fortran/src")
```

## Dependencies

- `fortran-mapper>=0.1.0`: Core Fortran parsing
- `networkx>=2.8`: Graph algorithms

## License

MIT License
