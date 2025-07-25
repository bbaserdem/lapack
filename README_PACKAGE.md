# LAPACK Utilities Package

This package provides command-line utilities for analyzing LAPACK computational graphs.

## Installation

### For Development

```bash
# Install in editable mode
pip install -e .

# Or with uv
uv pip install -e .
```

### For Usage

```bash
# Install from the project directory
pip install .
```

## Usage

After installation, you can use the `lapack-util` command:

### Parse LAPACK Source

```bash
# Parse and export to Neo4j
lapack-util parse /path/to/lapack/src -o neo4j

# Parse and export to JSON
lapack-util parse /path/to/lapack/src -o output.json

# Parse with exclusions
lapack-util parse /path/to/lapack/src --exclude "*test*" --exclude "*old*"
```

### Analyze Dependencies

```bash
# Analyze a specific routine
lapack-util analyze DGETRF

# Analyze with depth
lapack-util analyze DGETRF -d 3
```

### Export Data

```bash
# Export to various formats
lapack-util export cypher lapack_graph.cypher
lapack-util export json lapack_graph.json
lapack-util export graphml lapack_graph.graphml
lapack-util export dot lapack_graph.dot
lapack-util export stats lapack_stats.json
```

### Import Data

```bash
# Import from Cypher
lapack-util import data/sample_lapack_graph.cypher

# Import from JSON
lapack-util import backup.json --clear
```

### Backup and Restore

```bash
# Create backup
lapack-util backup

# List backups
lapack-util list-backups

# Restore from backup
lapack-util restore backups/backup_20250125_210000
```

### Query Database

```bash
# Execute Cypher query
lapack-util query "MATCH (r:Routine) RETURN r.name LIMIT 10"

# Save query results
lapack-util query "MATCH (r:Routine)-[:CALLS]->() RETURN r" -o results.json
```

### Database Statistics

```bash
# Show database statistics
lapack-util stats
```

### Sample Data

```bash
# Create sample data
lapack-util sample

# Import sample data
lapack-util import data/sample_lapack_graph.cypher
```

## Connection Options

All commands support Neo4j connection options:

```bash
lapack-util --neo4j-uri bolt://localhost:7687 --username neo4j --password mypass <command>
```

## Examples

### Complete Workflow

```bash
# 1. Parse LAPACK source
lapack-util parse /path/to/lapack/SRC /path/to/lapack/BLAS/SRC -o neo4j

# 2. Create a backup
lapack-util backup

# 3. Analyze key routines
lapack-util analyze DGETRF
lapack-util analyze DGEMM

# 4. Export for visualization
lapack-util export dot lapack_graph.dot
lapack-util export graphml lapack_graph.graphml

# 5. Get statistics
lapack-util stats
```

### Research Workflow

```bash
# Find all BLAS Level 3 routines
lapack-util query "MATCH (r:Routine {type: 'blas3'}) RETURN r.name"

# Find most connected routines
lapack-util query "MATCH (r:Routine) WITH r, size((r)-[:CALLS]->()) as out RETURN r.name, out ORDER BY out DESC LIMIT 10"

# Export subgraph
lapack-util query "MATCH path=(r:Routine {name: 'DGETRF'})-[:CALLS*..2]->() RETURN path" -o dgetrf_deps.json
```