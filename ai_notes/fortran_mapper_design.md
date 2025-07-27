# Fortran Mapper Design & Implementation Plan

## Analysis of Current Implementation

After analyzing the lapack-util codebase, I've identified the following structure and separation opportunities:

### Current LAPACK-Specific Elements

1. **fortran_parser.py**:
   - LAPACK naming convention parsing (lines 32-38)
   - Precision extraction from first character (s/d/c/z)
   - Operation extraction from remaining characters

2. **parser.py (LapackParser)**:
   - BLAS/LAPACK pattern recognition (lines 48-53)
   - Precision extraction method (lines 191-203)
   - Category extraction with matrix type mappings (lines 205-281)
   - LAPACK-specific matrix type codes (GE, GB, GT, PO, etc.)
   - BLAS/LAPACK coloring in visualization exports

3. **graph_schema.py**:
   - Already somewhat generic with node/relationship types
   - Has PRECISION and OPERATION nodes specific to LAPACK

### General Fortran Elements

1. **fortran_parser.py**:
   - AST parsing using fortran-src
   - Subroutine/function extraction
   - Call statement parsing
   - File parsing logic

2. **parser.py**:
   - General subroutine tracking
   - Call graph building
   - Export functionality (JSON, DOT, GraphML, Neo4j)

## Proposed Architecture

### 1. Core fortran-mapper Package Structure

```
fortran-mapper/
├── src/
│   ├── fortran_mapper/
│   │   ├── __init__.py
│   │   ├── core/
│   │   │   ├── parser.py          # Generic Fortran parser
│   │   │   ├── nodes.py           # Base node types
│   │   │   ├── relationships.py   # Base relationship types
│   │   │   └── graph.py           # Graph builder
│   │   ├── hooks/
│   │   │   ├── __init__.py
│   │   │   ├── base.py            # Hook interfaces
│   │   │   └── registry.py        # Hook registration
│   │   └── exporters/
│   │       ├── json.py
│   │       ├── neo4j.py
│   │       ├── dot.py
│   │       └── graphml.py
├── pyproject.toml
└── README.md
```

### 2. Hook Interface Design

```python
# hooks/base.py
from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, List

class NodeEnricher(ABC):
    """Hook for enriching node properties"""
    
    @abstractmethod
    def enrich_routine(self, name: str, file_path: str, 
                      properties: Dict[str, Any]) -> Dict[str, Any]:
        """Enrich routine node with additional properties"""
        pass
    
    @abstractmethod
    def extract_categories(self, name: str) -> List[str]:
        """Extract categories from routine name"""
        pass

class NodeCreator(ABC):
    """Hook for creating additional nodes"""
    
    @abstractmethod
    def create_additional_nodes(self, routine_name: str) -> List[Dict[str, Any]]:
        """Create additional nodes based on routine"""
        pass

class RelationshipCreator(ABC):
    """Hook for creating custom relationships"""
    
    @abstractmethod
    def create_relationships(self, from_node: str, to_node: str, 
                           context: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Create custom relationships"""
        pass
```

### 3. LAPACK Adapter Implementation

```python
# lapack_adapter.py
from fortran_mapper.hooks import NodeEnricher, NodeCreator

class LapackNodeEnricher(NodeEnricher):
    def __init__(self):
        self.precision_map = {
            'S': 'single',
            'D': 'double',
            'C': 'complex',
            'Z': 'double_complex'
        }
        self.matrix_types = {
            'GE': 'general',
            'GB': 'general_band',
            # ... etc
        }
    
    def enrich_routine(self, name: str, file_path: str, 
                      properties: Dict[str, Any]) -> Dict[str, Any]:
        # Add LAPACK-specific properties
        if name and len(name) > 1:
            properties['precision'] = self.precision_map.get(name[0].upper())
            properties['operation'] = name[1:].upper()
            properties['category'] = self._extract_category(name)
        return properties
    
    def extract_categories(self, name: str) -> List[str]:
        categories = []
        # BLAS level detection
        if self._is_blas1(name):
            categories.append('blas1')
        # etc...
        return categories

class LapackNodeCreator(NodeCreator):
    def create_additional_nodes(self, routine_name: str) -> List[Dict[str, Any]]:
        nodes = []
        # Create PRECISION node
        if routine_name and routine_name[0].upper() in 'SDCZ':
            nodes.append({
                'type': 'Precision',
                'properties': {'symbol': routine_name[0].upper()}
            })
        # Create OPERATION node
        if len(routine_name) > 1:
            nodes.append({
                'type': 'Operation',
                'properties': {'name': routine_name[1:].upper()}
            })
        return nodes
```

## Implementation Plan

### Phase 1: Core fortran-mapper Development

1. **Extract generic Fortran parsing logic**
   - Create base parser without LAPACK specifics
   - Define generic node types (Routine, Function, Module, File)
   - Implement basic call graph building

2. **Implement hook system**
   - Create hook interfaces
   - Build hook registry and plugin system
   - Add hook injection points in parser

3. **Port exporters**
   - Make exporters work with generic nodes
   - Add hook support for custom node types

### Phase 2: LAPACK Adapter

1. **Create LAPACK hooks**
   - Implement NodeEnricher for LAPACK naming conventions
   - Implement NodeCreator for PRECISION/OPERATION nodes
   - Port all LAPACK-specific logic

2. **Test compatibility**
   - Ensure feature parity with current lapack-util
   - Verify graph output matches

### Phase 3: Migration

1. **Update lapack-util**
   - Replace internal parser with fortran-mapper + LAPACK adapter
   - Maintain existing CLI interface
   - Add deprecation notices if needed

2. **Documentation**
   - Document hook API
   - Provide examples for other Fortran projects
   - Migration guide for lapack-util users

## Benefits

1. **Reusability**: Other Fortran projects can use fortran-mapper
2. **Extensibility**: Hook system allows customization without forking
3. **Maintainability**: Clear separation of concerns
4. **Testing**: Easier to test generic vs. specific functionality

## Hook Usage Example

```python
from fortran_mapper import FortranMapper
from lapack_adapter import LapackNodeEnricher, LapackNodeCreator

# Create mapper with LAPACK hooks
mapper = FortranMapper()
mapper.register_hook('node_enricher', LapackNodeEnricher())
mapper.register_hook('node_creator', LapackNodeCreator())

# Parse with LAPACK-specific enrichment
graph = mapper.parse_directory("path/to/lapack/src")

# Export with LAPACK nodes included
mapper.export_to_neo4j(graph, driver)
```

## Next Steps

1. Review and refine hook interface design
2. Start implementing core fortran-mapper
3. Create comprehensive test suite
4. Build LAPACK adapter
5. Plan migration strategy