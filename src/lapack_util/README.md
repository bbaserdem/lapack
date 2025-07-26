# LAPACK Utilities

A command-line utility for analyzing LAPACK computational graphs and managing Neo4j database operations.

## Installation

This package is managed with `uv` and `nix`. To install dependencies:

```bash
# Dependencies are managed through pyproject.toml
# Use nix to set up the development environment
```

## Usage

### Neo4j Server Management

The `lapack-util` tool now includes built-in Neo4j server management commands:

```bash
# Start Neo4j server
lapack-util neo4j start

# Stop Neo4j server
lapack-util neo4j stop

# Check Neo4j server status
lapack-util neo4j status

# Run Neo4j in console mode (foreground)
lapack-util neo4j console

# Use a custom data directory
lapack-util neo4j start --data-dir /path/to/neo4j-data
```

### Parsing LAPACK Source Code

```bash
# Parse LAPACK source files and store in Neo4j
lapack-util parse src/

# Parse and export to JSON
lapack-util parse src/ -o output.json

# Parse with exclusions
lapack-util parse src/ --exclude "*test*" "*example*"
```

### Analyzing Dependencies

```bash
# Analyze routine dependencies
lapack-util analyze DGETRF -d 3

# Show database statistics
lapack-util stats
```

### Data Management

```bash
# Export data
lapack-util export cypher output.cypher
lapack-util export json output.json
lapack-util export graphml output.graphml

# Import data
lapack-util import input.cypher --clear

# Backup and restore
lapack-util backup
lapack-util restore backups/backup-20240126-120000.cypher
lapack-util list-backups
```

### Direct Cypher Queries

```bash
# Execute Cypher query
lapack-util query "MATCH (n:Routine) RETURN n.name LIMIT 10"

# Save query results
lapack-util query "MATCH (n:Routine) RETURN n" -o results.json
```

## Command-Line Options

### Global Options

- `--neo4j-uri`: Neo4j connection URI (default: bolt://localhost:7687)
- `--username`: Neo4j username
- `--password`: Neo4j password
- `--debug`: Enable debug logging
- `--log-file`: Log to file instead of stderr

### Neo4j Server Commands

The `neo4j` subcommand supports:
- `start`: Start the Neo4j server
- `stop`: Stop the Neo4j server
- `status`: Check server status
- `console`: Run in foreground mode

Options:
- `--data-dir`: Path to Neo4j data directory (default: ./neo4j-data)

## Dependencies

- `neo4j>=5.0`: Neo4j Python driver
- `psutil>=5.0`: Process and system monitoring (for Neo4j management)

## Neo4j Server Requirements

The Neo4j server management commands require:
- Neo4j installed and available in your PATH
- A properly configured `neo4j-data` directory with `neo4j.conf`

The commands will automatically detect Neo4j installations in common locations:
- System-wide installations (`/usr/bin/neo4j`)
- Nix profile installations (`~/.nix-profile/bin/neo4j`)
- Nix store paths

## Logging

Enable debug logging for troubleshooting:

```bash
# Log to stderr with debug level
lapack-util --debug neo4j status

# Log to file
lapack-util --log-file lapack.log parse src/
```

## Error Handling

The tool provides detailed error messages for common issues:
- Neo4j executable not found
- Data directory missing
- Port conflicts
- Permission issues

## Example Workflow

```bash
# 1. Start Neo4j server
lapack-util neo4j start

# 2. Parse LAPACK source code
lapack-util parse /path/to/lapack/SRC

# 3. Analyze a specific routine
lapack-util analyze DGETRF -d 2

# 4. Export the graph
lapack-util export graphml lapack-graph.graphml

# 5. Stop Neo4j when done
lapack-util neo4j stop
```