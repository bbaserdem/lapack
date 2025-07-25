# LAPACK Parser Examples

This directory contains example scripts demonstrating how to use the LAPACK parser and analysis tools.

## Available Examples

### 1. parse_single_file.py
**Purpose**: Parse a single Fortran file and display results

```bash
python examples/parse_single_file.py
```

Shows how to:
- Parse individual Fortran files
- Access routine information
- Display parsing results

### 2. analyze_dependencies.py
**Purpose**: Analyze dependencies and find critical routines

```bash
python examples/analyze_dependencies.py
```

Demonstrates:
- Building call graphs
- Finding most-called routines
- Identifying leaf routines and entry points
- Analyzing call chains
- Finding precision variants

### 3. export_to_formats.py
**Purpose**: Export parsing results to different file formats

```bash
python examples/export_to_formats.py
```

Exports to:
- JSON (complete graph structure)
- CSV (routines and dependencies)
- GraphML (for visualization tools)
- Summary report (human-readable)

### 4. find_circular_dependencies.py
**Purpose**: Detect circular dependencies in the codebase

```bash
python examples/find_circular_dependencies.py
```

Features:
- Find strongly connected components
- Detect mutual dependencies
- Identify self-loops
- Analyze indirect recursion

### 5. build_complete_graph.py
**Purpose**: Build and export the complete LAPACK/BLAS graph to Neo4j

```bash
# Basic usage
python examples/build_complete_graph.py

# Custom Neo4j connection
python examples/build_complete_graph.py \
  --neo4j-uri bolt://localhost:7687 \
  --neo4j-user neo4j \
  --neo4j-password mypassword

# Parse only BLAS
python examples/build_complete_graph.py --blas-only

# Skip Neo4j export and save statistics
python examples/build_complete_graph.py --skip-neo4j --output-stats stats.json
```

Options:
- Progress reporting during parsing
- Error handling and retry logic
- Comprehensive statistics
- Neo4j export with verification

### 6. lapack_to_neo4j_concept.py
**Purpose**: Original concept demonstration for Neo4j integration

### 7. lapack_testing.py
**Purpose**: Testing framework integration example

## Common Patterns

### Loading the Parser

All examples use a similar pattern to import the parser:

```python
from pathlib import Path
import sys

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.lapack_util.fortran_parser import FortranParser
```

### Basic Parsing Loop

```python
parser = FortranParser()

for f_file in Path("BLAS/SRC").glob("*.f"):
    result = parser.parse_file(f_file)
    if not result.error:
        # Process routines
        for routine in result.routines:
            print(f"{routine.name}: {routine.routine_type}")
```

### Accessing Parsed Data

The parser stores all parsed files internally:

```python
# Access all parsed data
for file_path, file_data in parser._parsed_files.items():
    for routine in file_data.routines:
        # Process routine
        pass
```

### Building Call Graphs

```python
from collections import defaultdict

# Build forward call graph
calls_to = defaultdict(set)
for file_path, file_data in parser._parsed_files.items():
    for routine in file_data.routines:
        if routine.calls:
            calls_to[routine.name] = routine.calls
```

## Tips for Writing Your Own Analysis

1. **Start Small**: Test with a subset of files first
   ```python
   # Parse just double precision GEMM variants
   for f_file in Path("BLAS/SRC").glob("d*gemm.f"):
       parser.parse_file(f_file)
   ```

2. **Handle Errors**: Always check for parsing errors
   ```python
   result = parser.parse_file(file_path)
   if result.error:
       print(f"Error parsing {file_path}: {result.error}")
   ```

3. **Use Progress Reporting**: For large parsing jobs
   ```python
   files = list(Path("SRC").glob("*.f"))
   for i, f_file in enumerate(files):
       if i % 50 == 0:
           print(f"Progress: {i}/{len(files)}")
       parser.parse_file(f_file)
   ```

4. **Export Incrementally**: For very large graphs
   ```python
   # Parse and export in batches
   batch_size = 100
   for i in range(0, len(files), batch_size):
       batch = files[i:i+batch_size]
       # Parse batch
       # Export to Neo4j
   ```

## Neo4j Integration

For Neo4j examples, ensure you have:

1. Neo4j running (Docker or Desktop)
2. Correct connection parameters
3. The neo4j Python driver installed

```bash
# Start Neo4j with Docker
docker run -d \
  --name neo4j \
  -p 7474:7474 -p 7687:7687 \
  -e NEO4J_AUTH=neo4j/password \
  neo4j:latest
```

## Further Reading

- [User Guide](../docs/USER_GUIDE.md) - Complete usage documentation
- [Graph Schema](../docs/GRAPH_SCHEMA.md) - Neo4j schema details
- [Neo4j Queries](../docs/NEO4J_QUERIES.md) - Example Cypher queries