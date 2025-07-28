# Connectome

LAPACK computational connectome analysis and visualization package.

## Overview

Connectome is a specialized tool for analyzing and visualizing the computational structure of LAPACK (Linear Algebra PACKage). Built on top of the generic `fortran-mapper` library, it provides domain-specific analysis capabilities for understanding the interconnected nature of LAPACK routines.

## Features

- **LAPACK-Aware Analysis**: Understands LAPACK naming conventions, precision types, and computational categories
- **Connectome Visualization**: Generate interactive visualizations of computational relationships
- **Neo4j Integration**: Store and query computational graphs in Neo4j database
- **Multiple Export Formats**: JSON, DOT, GraphML, and more
- **Statistical Analysis**: Compute metrics on computational complexity and dependencies

## Installation

```bash
# Install from source
pip install -e .

# With development dependencies
pip install -e ".[dev]"

# With visualization dependencies
pip install -e ".[visualization]"
```

## Quick Start

### Basic Analysis

```bash
# Analyze LAPACK source directory
connectome analyze /path/to/lapack/SRC --json lapack_graph.json

# Generate DOT visualization
connectome export /path/to/lapack/SRC --dot lapack_graph.dot

# Launch Neo4j visualization
connectome visualize /path/to/lapack/SRC --neo4j
```

### Python API

```python
from connectome import ConnectomeAnalyzer

# Create analyzer
analyzer = ConnectomeAnalyzer()

# Parse LAPACK source
graph = analyzer.analyze_directory("/path/to/lapack/SRC")

# Get computational statistics
stats = analyzer.compute_statistics(graph)
print(f"Total routines: {stats.routine_count}")
print(f"Precision types: {stats.precision_types}")

# Export results
analyzer.export_to_neo4j(graph, "bolt://localhost:7687")
```

## Architecture

Connectome builds on `fortran-mapper` to provide:

1. **LAPACK-Specific Parsing**: Custom hooks for LAPACK naming conventions
2. **Connectome Analysis**: Specialized algorithms for computational relationship analysis
3. **Advanced Visualization**: Domain-specific visualization templates
4. **Database Integration**: Optimized schemas for computational graph storage

## Dependencies

- `fortran-mapper`: Generic Fortran code analysis
- `neo4j`: Graph database for storage and querying
- `networkx`: Graph algorithms and analysis
- `matplotlib`/`plotly`: Visualization
- `pandas`: Data analysis

## Development

```bash
# Install in development mode
pip install -e ".[dev]"

# Run tests
pytest

# Lint and format code  
ruff check src/ tests/
ruff format src/ tests/

# Type checking
mypy src/
```

## License

MIT License - see LICENSE file for details.
