# LAPACK Parser User Guide

A comprehensive tool for parsing and analyzing LAPACK/BLAS Fortran source code, building dependency graphs, and exporting to Neo4j graph database.

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Command Line Usage](#command-line-usage)
4. [Examples](#examples)
5. [Working with Neo4j](#working-with-neo4j)
6. [Python API Usage](#python-api-usage)
7. [Understanding the Output](#understanding-the-output)
8. [Troubleshooting](#troubleshooting)

## Installation

### Prerequisites

- Python 3.8 or higher
- Neo4j Database (optional, for graph database export)
- Git (for cloning the repository)

### Basic Installation

1. Clone the LAPACK repository:
```bash
git clone https://github.com/Reference-LAPACK/lapack.git
cd lapack
```

2. Install Python dependencies:
```bash
pip install -r requirements.txt
```

Or if using the Nix development environment:
```bash
nix develop
```

### Neo4j Setup (Optional)

If you want to export to Neo4j:

1. Install Neo4j Desktop or use Docker:
```bash
docker run -d \
  --name neo4j \
  -p 7474:7474 -p 7687:7687 \
  -e NEO4J_AUTH=neo4j/password \
  neo4j:latest
```

2. Access Neo4j Browser at http://localhost:7474

## Quick Start

### Parse a Single File

```bash
python lapack_parser.py --file BLAS/SRC/dgemm.f
```

### Parse All BLAS Files

```bash
python lapack_parser.py --directory BLAS/SRC --pattern "*.f"
```

### Parse and Export to Neo4j

```bash
python lapack_parser.py --directory . --neo4j \
  --neo4j-uri bolt://localhost:7687 \
  --neo4j-user neo4j \
  --neo4j-password password
```

## Command Line Usage

### Basic Syntax

```bash
python lapack_parser.py [OPTIONS]
```

### Options

#### Input Options (Required - choose one)
- `--file FILE, -f FILE`: Parse a single Fortran file
- `--directory DIR, -d DIR`: Parse all Fortran files in directory (recursive)

#### Pattern Options
- `--pattern PATTERN, -p PATTERN`: File patterns to match (can be used multiple times)
  - Default: `*.f`, `*.f90`, `*.F`, `*.F90`
  - Example: `--pattern "d*.f" --pattern "z*.f"`

#### Output Options
- `--output FILE, -o FILE`: Save results to file (JSON format)
- `--format {json,csv,graphml}`: Output format (default: json)

#### Neo4j Options
- `--neo4j`: Enable export to Neo4j database
- `--neo4j-uri URI`: Neo4j connection URI (default: bolt://localhost:7687)
- `--neo4j-user USER`: Neo4j username (default: neo4j)
- `--neo4j-password PASS`: Neo4j password (default: password)
- `--neo4j-clear`: Clear existing data before import

#### Analysis Options
- `--stats`: Show statistics about parsed code
- `--verbose, -v`: Enable verbose output

## Examples

### 1. Analyze Double Precision BLAS Routines

```bash
python lapack_parser.py --directory BLAS/SRC --pattern "d*.f" --stats --verbose
```

### 2. Parse LAPACK and Export Results

```bash
# Parse all LAPACK source files and save to JSON
python lapack_parser.py --directory SRC --output lapack_analysis.json --stats

# View the results
cat lapack_analysis.json | jq '.metadata'
```

### 3. Build Complete Dependency Graph in Neo4j

```bash
# Parse both BLAS and LAPACK
python lapack_parser.py --directory . \
  --pattern "*.f" \
  --neo4j \
  --neo4j-clear \
  --stats
```

### 4. Analyze Specific Operations

```bash
# Parse only GEMM variants
python lapack_parser.py --directory BLAS/SRC --pattern "*gemm.f" --verbose
```

### 5. Custom Pattern Matching

```bash
# Parse complex precision routines
python lapack_parser.py --directory . \
  --pattern "c*.f" \
  --pattern "z*.f" \
  --output complex_routines.json
```

## Working with Neo4j

### Connecting to Neo4j

After exporting to Neo4j, you can query the graph database using Cypher queries.

### Basic Queries

1. **Find all routines that call DGEMM:**
```cypher
MATCH (caller:Routine)-[:CALLS]->(callee:Routine {name: 'DGEMM'})
RETURN caller.name, caller.precision
ORDER BY caller.name
```

2. **Find all variants of GEMM:**
```cypher
MATCH (r:Routine)-[:IMPLEMENTS]->(op:Operation {name: 'gemm'})
RETURN r.name, r.precision
ORDER BY r.precision
```

3. **Analyze call depth:**
```cypher
MATCH path = (r:Routine {name: 'DGETRF'})-[:CALLS*1..3]->(dep:Routine)
RETURN path
```

4. **Find most called routines:**
```cypher
MATCH (r:Routine)<-[:CALLS]-(caller)
RETURN r.name, count(caller) as call_count
ORDER BY call_count DESC
LIMIT 10
```

### Visualization

Use Neo4j Browser to visualize:
- Call dependency graphs
- Precision variant relationships
- File organization structure

## Python API Usage

### Basic Parsing

```python
from src.lapack_util.fortran_parser import FortranParser

# Initialize parser
parser = FortranParser()

# Parse a single file
result = parser.parse_file("BLAS/SRC/dgemm.f")

# Access routine information
for routine in result.routines:
    print(f"{routine.name}: {routine.routine_type}")
    print(f"  Calls: {routine.calls}")
```

### Batch Processing

```python
from pathlib import Path
from src.lapack_util.fortran_parser import FortranParser

parser = FortranParser()

# Parse multiple files
for f_file in Path("BLAS/SRC").glob("*.f"):
    result = parser.parse_file(f_file)
    if not result.error:
        print(f"{f_file.name}: {len(result.routines)} routines")
```

### Neo4j Integration

```python
from src.lapack_util.lapack_to_neo4j import LapackToNeo4j
from src.lapack_util.fortran_parser import FortranParser

# Parse files
parser = FortranParser()
parser.parse_file("BLAS/SRC/dgemm.f")

# Export to Neo4j
neo4j = LapackToNeo4j(
    uri="bolt://localhost:7687",
    user="neo4j",
    password="password"
)
neo4j.process_parsed_data(parser._parsed_files)
```

## Understanding the Output

### JSON Output Structure

```json
{
  "metadata": {
    "total_files": 100,
    "total_routines": 500
  },
  "files": {
    "BLAS/SRC/dgemm.f": {
      "routines": [
        {
          "name": "DGEMM",
          "type": "subroutine",
          "precision": "d",
          "operation": "gemm",
          "calls": ["XERBLA"],
          "lines": [150, 450]
        }
      ]
    }
  }
}
```

### Routine Properties

- **name**: Full routine name (e.g., "DGEMM")
- **type**: Either "subroutine" or "function"
- **precision**: 
  - 's' = single precision real
  - 'd' = double precision real
  - 'c' = single precision complex
  - 'z' = double precision complex
- **operation**: Base operation without precision prefix
- **calls**: List of routines this routine calls
- **lines**: [start_line, end_line] in source file

## Troubleshooting

### Common Issues

#### 1. Import Errors
```
ImportError: No module named 'src.lapack_util'
```
**Solution**: Run from the LAPACK root directory or adjust PYTHONPATH:
```bash
export PYTHONPATH="${PYTHONPATH}:$(pwd)"
```

#### 2. Neo4j Connection Failed
```
ServiceUnavailable: Failed to establish connection
```
**Solution**: 
- Check Neo4j is running: `docker ps` or check Neo4j Desktop
- Verify connection details (URI, username, password)
- Check firewall settings for ports 7474 and 7687

#### 3. File Not Found
```
FileNotFoundError: [Errno 2] No such file or directory
```
**Solution**: Run from LAPACK root directory or use absolute paths

#### 4. Parsing Errors
```
Error parsing file: unexpected token
```
**Solution**: 
- Check if file is valid Fortran
- Some F90 features may not be supported
- Report issue with example file

### Performance Tips

1. **Large Codebases**: Use patterns to limit files parsed
2. **Neo4j Import**: Use `--neo4j-clear` to avoid duplicates
3. **Memory Usage**: Process directories in batches for very large codebases

### Getting Help

1. Use `--help` for command options:
```bash
python lapack_parser.py --help
```

2. Enable verbose mode for debugging:
```bash
python lapack_parser.py --file problem.f --verbose
```

3. Check the [Graph Schema Documentation](GRAPH_SCHEMA.md) for Neo4j structure

## Advanced Usage

### Custom Analysis Scripts

Create custom analysis using the parsed data:

```python
# find_critical_paths.py
from src.lapack_util.fortran_parser import FortranParser
from collections import defaultdict

parser = FortranParser()
# Parse your files...

# Build reverse call graph
called_by = defaultdict(set)
for file_data in parser._parsed_files.values():
    for routine in file_data.routines:
        for called in routine.calls:
            called_by[called].add(routine.name)

# Find critical routines
critical = [(name, len(callers)) 
            for name, callers in called_by.items() 
            if len(callers) > 10]

print("Most critical routines:")
for name, count in sorted(critical, key=lambda x: x[1], reverse=True)[:10]:
    print(f"  {name}: called by {count} routines")
```

### Integration with CI/CD

Add to your build pipeline:

```bash
# Check for circular dependencies
python lapack_parser.py --directory . --neo4j
echo "MATCH path = (r:Routine)-[:CALLS*2..10]->(r) RETURN path LIMIT 1" | cypher-shell
```

## Next Steps

- Explore the [Example Scripts](../examples/) directory
- Read the [Graph Schema Documentation](GRAPH_SCHEMA.md)
- Try the [Neo4j Query Examples](#working-with-neo4j)
- Contribute improvements via GitHub