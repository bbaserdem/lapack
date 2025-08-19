# Fortran Mapper

A flexible, extensible Fortran code mapper with hook-based customization for analyzing and visualizing Fortran codebases.

## Features

- **Generic Fortran Parsing**: Parse Fortran 77/90/95 source files
- **Extensible Hook System**: Customize parsing behavior for specific domains
- **Multiple Export Formats**: JSON, DOT, GraphML, Neo4j
- **Graph Analysis**: Build and analyze call graphs, dependency trees
- **Domain Adapters**: Pre-built adapters for LAPACK/BLAS and other domains
- **Interactive Visualization**: Web-based force-directed graph visualization with D3.js

## Installation

```bash
pip install fortran-mapper

# With Neo4j support
pip install fortran-mapper[neo4j]

# With visualization support
pip install fortran-mapper[visualization]
```

## Quick Start

### Basic Usage

```python
from fortran_mapper import FortranMapper

# Create mapper instance
mapper = FortranMapper()

# Parse a directory
graph = mapper.parse_directory("path/to/fortran/src")

# Export to various formats
mapper.export_to_json(graph, "output.json")
mapper.export_to_dot(graph, "output.dot")
mapper.export_to_graphml(graph, "output.graphml")
```

### Using Hooks for Customization

```python
from fortran_mapper import FortranMapper
from fortran_mapper.hooks import NodeEnricher, NodeCreator

class MyNodeEnricher(NodeEnricher):
    def enrich_routine(self, name, file_path, properties):
        # Add custom properties based on your naming conventions
        if name.startswith("MY_"):
            properties["domain"] = "my_domain"
        return properties
    
    def extract_categories(self, name):
        # Extract categories from routine names
        if "UTIL" in name:
            return ["utility"]
        return []

# Use with mapper
mapper = FortranMapper()
mapper.register_hook("node_enricher", MyNodeEnricher())
graph = mapper.parse_directory("path/to/src")
```

### LAPACK/BLAS Example

First install the LAPACK hooks package:
```bash
# Using uv (recommended):
uv pip install fortran-mapper-hooks-lapack

# Or install from the local directory for development:
cd hooks/lapack
uv pip install -e .
```

Then use it:
```python
from fortran_mapper import FortranMapper
from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator

# Create mapper with LAPACK hooks
mapper = FortranMapper()
mapper.register_hook("node_enricher", LapackNodeEnricher())
mapper.register_hook("node_creator", LapackNodeCreator())

# Parse LAPACK source
graph = mapper.parse_directory("path/to/lapack/src")

# Export with LAPACK-specific nodes
mapper.export_to_neo4j(graph, driver)
```

## Command Line Interface

Fortran Mapper provides a comprehensive CLI for parsing, analyzing, and visualizing Fortran code:

```bash
# Parse Fortran source and export to Neo4j
fortran-mapper parse /path/to/src --neo4j-uri bolt://localhost:7687

# Start interactive visualization server
fortran-mapper visualize serve

# Export static visualization
fortran-mapper visualize export output.html --mode overview --limit 200

# Query Neo4j database
fortran-mapper query "MATCH (r:Routine) RETURN r.name LIMIT 10"

# Export data in various formats
fortran-mapper export json data.json
fortran-mapper export graphml data.graphml
fortran-mapper export cypher statements.cypher
```

### Visualization Commands

The visualization module provides interactive, web-based graph exploration:

```bash
# Start visualization server (opens browser automatically)
fortran-mapper visualize serve

# Custom host and port
fortran-mapper visualize serve --host 0.0.0.0 --port 8888

# Export static HTML visualization
fortran-mapper visualize export graph.html

# Export centered on specific node
fortran-mapper visualize export graph.html --mode centered --center-node DGEMM --depth 3

# Export call hierarchy
fortran-mapper visualize export graph.html --mode hierarchy --routine DGETRF --depth 4
```

## Hook System

The hook system allows you to customize the parsing behavior without modifying the core parser.

### Available Hooks

1. **NodeEnricher**: Add properties to nodes based on naming conventions
2. **NodeCreator**: Create additional nodes (e.g., precision, operation nodes)
3. **RelationshipCreator**: Create custom relationships between nodes
4. **ParseFilter**: Filter which files or routines to include

### Creating Custom Hooks

```python
from fortran_mapper.hooks import NodeEnricher

class MyCustomEnricher(NodeEnricher):
    def enrich_routine(self, name, file_path, properties):
        # Your enrichment logic here
        return properties
    
    def extract_categories(self, name):
        # Your categorization logic here
        return []
```

## Architecture

```
fortran-mapper/
├── core/
│   ├── parser.py          # Generic Fortran parser
│   ├── nodes.py           # Base node types
│   ├── relationships.py   # Base relationship types
│   └── graph.py           # Graph builder
├── hooks/
│   ├── base.py            # Hook interfaces
│   └── registry.py        # Hook registration
└── exporters/
    ├── json.py
    ├── neo4j.py
    ├── dot.py
    └── graphml.py
```

## License

MIT License