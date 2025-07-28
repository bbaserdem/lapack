# Migration Guide: From lapack-util to fortran-mapper

This guide helps you migrate from the domain-specific lapack-util to the generic, extensible fortran-mapper.

## Overview

The fortran-mapper package provides:
- **Generic Fortran parsing** (not limited to LAPACK/BLAS)
- **Extensible hook system** for domain-specific customization
- **Multiple export formats** (JSON, DOT, GraphML, Neo4j)
- **Better error handling** and type safety
- **Cleaner architecture** with separation of concerns

## Quick Migration

### 1. Installation

```bash
# Install the new package
pip install fortran-mapper

# With optional dependencies
pip install fortran-mapper[neo4j,visualization]
```

### 2. Code Changes

**Before (lapack-util):**
```python
from lapack_analyzer.parser import LapackParser

parser = LapackParser()
parser.parse_directory("path/to/lapack")
parser.export_to_json("output.json")
parser.export_to_neo4j(driver)
```

**After (fortran-mapper):**
```python
from fortran_mapper import FortranMapper
from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator

mapper = FortranMapper()
mapper.register_hook("node_enricher", LapackNodeEnricher())
mapper.register_hook("node_creator", LapackNodeCreator())

graph = mapper.parse_directory("path/to/lapack")
mapper.export_to_json(graph, "output.json")
mapper.export_to_neo4j(graph, driver)
```

Note: Install the LAPACK hooks package separately:
```bash
# Using uv:
uv pip install fortran-mapper-hooks-lapack
# Or for development:
cd hooks/lapack && uv pip install -e .
```

## Detailed Migration Steps

### Step 1: Update Imports

Replace your imports:

```python
# OLD
from lapack_analyzer.parser import LapackParser
from lapack_analyzer.graph_schema import create_schema_from_routines

# NEW
from fortran_mapper import FortranMapper
from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator
from fortran_mapper.core.nodes import NodeType
```

### Step 2: Replace Parser Initialization

**Before:**
```python
parser = LapackParser()
```

**After:**
```python
mapper = FortranMapper()
mapper.register_hook("node_enricher", LapackNodeEnricher())
mapper.register_hook("node_creator", LapackNodeCreator())
```

### Step 3: Update Parsing Logic

**Before:**
```python
parser.parse_directory(directory)
# Data is stored in parser.subroutines
```

**After:**
```python
graph = mapper.parse_directory(directory)
# Data is stored in graph structure
routines = graph.get_nodes_by_type(NodeType.ROUTINE)
files = graph.get_nodes_by_type(NodeType.FILE)
```

### Step 4: Update Export Methods

**Before:**
```python
parser.export_to_json("output.json")
parser.export_to_neo4j(driver)
```

**After:**
```python
mapper.export_to_json(graph, "output.json")
mapper.export_to_neo4j(graph, driver)
mapper.export_to_dot(graph, "output.dot")  # New format!
mapper.export_to_graphml(graph, "output.graphml")  # New format!
```

### Step 5: Update Data Access

**Before:**
```python
for name, sub in parser.subroutines.items():
    print(f"Routine: {name}")
    print(f"Calls: {sub.calls}")
    print(f"File: {sub.file_path}")
```

**After:**
```python
routines = graph.get_nodes_by_type(NodeType.ROUTINE)
for routine in routines:
    print(f"Routine: {routine.name}")
    
    # Get call relationships
    calls = graph.get_relationships_from(routine.node_id)
    call_names = [rel.to_node_id.split(':')[1] for rel in calls 
                  if rel.rel_type == RelationType.CALLS]
    print(f"Calls: {call_names}")
    
    # Get file relationship
    file_rels = [rel for rel in graph.get_relationships_to(routine.node_id)
                 if rel.rel_type == RelationType.DEFINED_IN]
    if file_rels:
        file_node = graph.get_node(file_rels[0].from_node_id)
        print(f"File: {file_node.properties['path']}")
```

## Feature Mapping

| lapack-util Feature | fortran-mapper Equivalent |
|---------------------|---------------------------|
| `LapackParser` | `FortranMapper + LapackNodeEnricher` |
| `parser.subroutines` | `graph.get_nodes_by_type(NodeType.ROUTINE)` |
| Built-in LAPACK logic | `LapackNodeEnricher` hook |
| Fixed export formats | Multiple exporters with `mapper.export_to_*()` |
| Error handling | Extensible through `ErrorHandler` hooks |
| LAPACK-only parsing | Generic Fortran + domain adapters |

## New Capabilities

### 1. Extensible Hook System

```python
# Create custom enricher
class MyEnricher(NodeEnricher):
    def enrich_routine(self, name, file_path, properties):
        if "MY_" in name:
            properties["domain"] = "my_domain"
        return properties
    
    def extract_categories(self, name):
        return ["custom_category"] if name.startswith("CUSTOM") else []

mapper.register_hook("node_enricher", MyEnricher())
```

### 2. Custom Filtering

```python
from fortran_mapper.hooks.base import ParseFilter

class OnlyPublicRoutinesFilter(ParseFilter):
    def should_include_routine(self, routine_name, routine_type):
        return not routine_name.startswith("_")  # Skip private routines
    
    def should_parse_file(self, file_path):
        return not "test" in file_path.lower()  # Skip test files

mapper.register_hook("parse_filter", OnlyPublicRoutinesFilter())
```

### 3. Multiple Export Formats

```python
# All of these are now available
mapper.export_to_json(graph, "output.json")
mapper.export_to_dot(graph, "output.dot")
mapper.export_to_graphml(graph, "output.graphml") 
mapper.export_to_neo4j(graph, driver)

# Or use exporters directly for more control
from fortran_mapper.exporters.dot import DOTExporter
exporter = DOTExporter()
exporter.export_call_graph_only(graph, "calls_only.dot")
```

### 4. Generic Fortran Support

```python
# Parse any Fortran code, not just LAPACK
mapper = FortranMapper()  # No hooks = generic parsing
graph = mapper.parse_directory("path/to/any/fortran")
```

## Advanced Migration

### Custom Node Types

If you were extending lapack-util with custom node types, use the hook system:

```python
from fortran_mapper.hooks.base import NodeCreator
from fortran_mapper.core.nodes import CustomNode

class MyNodeCreator(NodeCreator):
    def create_additional_nodes(self, routine_name, routine_properties):
        nodes = []
        if "SOLVER" in routine_name:
            nodes.append(CustomNode(
                custom_type="SolverType",
                name=f"{routine_name}_SOLVER",
                algorithm="iterative" if "ITER" in routine_name else "direct"
            ))
        return nodes

mapper.register_hook("node_creator", MyNodeCreator())
```

### Error Handling

```python
from fortran_mapper.hooks.base import ErrorHandler

class MyErrorHandler(ErrorHandler):
    def handle_parse_error(self, file_path, error):
        # Log errors, attempt recovery, etc.
        return {
            "error_type": "parse_failure",
            "file": file_path,
            "message": str(error)
        }

mapper.register_hook("error_handler", MyErrorHandler())
```

## Testing Your Migration

1. **Run both versions** side by side initially
2. **Compare outputs** using the JSON exports
3. **Verify node counts** and relationship counts match
4. **Test custom functionality** incrementally

### Sample Test Script

```python
def test_migration():
    # Old way
    old_parser = LapackParser()
    old_parser.parse_directory("test_data")
    old_count = len(old_parser.subroutines)
    
    # New way
    new_mapper = FortranMapper()
    new_mapper.register_hook("node_enricher", LapackNodeEnricher())
    graph = new_mapper.parse_directory("test_data")
    new_routines = graph.get_nodes_by_type(NodeType.ROUTINE)
    new_count = len(new_routines)
    
    assert old_count == new_count, f"Routine count mismatch: {old_count} vs {new_count}"
    print("✅ Migration successful!")
```

## Troubleshooting

### Common Issues

1. **Missing nodes**: Ensure you've registered the `LapackNodeCreator` hook
2. **Missing properties**: Verify the `LapackNodeEnricher` is registered
3. **Different relationships**: The new system creates more explicit relationships

### Getting Help

- Check the [examples/](examples/) directory for working code
- Review the test files for usage patterns
- Read the API documentation in the source code

### Performance

The new system may be slightly slower initially due to the hook system, but provides:
- Better error recovery
- More detailed analysis
- Extensible processing pipeline

## Benefits of Migration

✅ **Generic Fortran support** - Use with any Fortran codebase
✅ **Extensible architecture** - Add custom logic without forking
✅ **Better error handling** - Graceful degradation on parse failures  
✅ **Multiple export formats** - JSON, DOT, GraphML, Neo4j
✅ **Type safety** - Better IDE support and runtime checking
✅ **Cleaner API** - Separation of parsing and domain logic
✅ **Future-proof** - Extensible for new requirements

## Next Steps

1. Start with the simple migration for basic functionality
2. Add custom hooks for domain-specific needs
3. Explore the new export formats
4. Consider using the generic parser for other Fortran projects
5. Contribute domain adapters for other Fortran ecosystems

The migration provides immediate benefits while opening up new possibilities for Fortran code analysis and visualization.