#!/usr/bin/env python3
"""
Graph schema design for LAPACK code structure in Neo4j.

This module defines the graph schema and provides utilities for
converting parsed Fortran AST data into Neo4j graph format.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Set, Tuple
from enum import Enum
import json
from pathlib import Path


class NodeType(Enum):
    """Types of nodes in the graph"""
    ROUTINE = "Routine"
    FILE = "File"
    MODULE = "Module"
    LIBRARY = "Library"
    OPERATION = "Operation"
    PRECISION = "Precision"
    PARSE_ERROR = "ParseError"  # New node type for errors


class RelationType(Enum):
    """Types of relationships in the graph"""
    CALLS = "CALLS"
    DEFINED_IN = "DEFINED_IN"
    BELONGS_TO = "BELONGS_TO"
    DEPENDS_ON = "DEPENDS_ON"
    IMPLEMENTS = "IMPLEMENTS"
    HAS_PRECISION = "HAS_PRECISION"
    NEXT_VERSION = "NEXT_VERSION"  # For precision variants
    HAS_ERROR = "HAS_ERROR"  # File has parsing error
    PARTIAL_PARSE = "PARTIAL_PARSE"  # File was partially parsed despite errors


@dataclass
class GraphNode:
    """Represents a node in the graph"""
    node_type: NodeType
    properties: Dict[str, any]
    node_id: Optional[str] = None
    
    def __post_init__(self):
        if not self.node_id:
            # Generate unique ID based on type and key properties
            if self.node_type == NodeType.ROUTINE:
                self.node_id = f"routine:{self.properties.get('name', '')}"
            elif self.node_type == NodeType.FILE:
                self.node_id = f"file:{self.properties.get('path', '')}"
            elif self.node_type == NodeType.OPERATION:
                self.node_id = f"operation:{self.properties.get('name', '')}"
            elif self.node_type == NodeType.PRECISION:
                self.node_id = f"precision:{self.properties.get('symbol', '')}"
            elif self.node_type == NodeType.PARSE_ERROR:
                self.node_id = f"error:{self.properties.get('id', '')}"


@dataclass
class GraphRelationship:
    """Represents a relationship in the graph"""
    rel_type: RelationType
    from_node_id: str
    to_node_id: str
    properties: Dict[str, any] = field(default_factory=dict)


@dataclass
class GraphSchema:
    """Complete graph schema with nodes and relationships"""
    nodes: List[GraphNode] = field(default_factory=list)
    relationships: List[GraphRelationship] = field(default_factory=list)
    
    def add_node(self, node: GraphNode) -> None:
        """Add a node if it doesn't already exist"""
        if not any(n.node_id == node.node_id for n in self.nodes):
            self.nodes.append(node)
    
    def add_relationship(self, rel: GraphRelationship) -> None:
        """Add a relationship if it doesn't already exist"""
        if not any(r.from_node_id == rel.from_node_id and 
                  r.to_node_id == rel.to_node_id and
                  r.rel_type == rel.rel_type for r in self.relationships):
            self.relationships.append(rel)
    
    def to_cypher_statements(self) -> List[str]:
        """Convert to Cypher CREATE statements"""
        statements = []
        
        # Create constraints and indexes
        statements.extend([
            "CREATE CONSTRAINT IF NOT EXISTS FOR (r:Routine) REQUIRE r.name IS UNIQUE;",
            "CREATE CONSTRAINT IF NOT EXISTS FOR (f:File) REQUIRE f.path IS UNIQUE;",
            "CREATE CONSTRAINT IF NOT EXISTS FOR (o:Operation) REQUIRE o.name IS UNIQUE;",
            "CREATE CONSTRAINT IF NOT EXISTS FOR (p:Precision) REQUIRE p.symbol IS UNIQUE;",
            "CREATE CONSTRAINT IF NOT EXISTS FOR (e:ParseError) REQUIRE e.id IS UNIQUE;",
            "CREATE INDEX IF NOT EXISTS FOR (r:Routine) ON (r.precision);",
            "CREATE INDEX IF NOT EXISTS FOR (r:Routine) ON (r.operation);",
            "CREATE INDEX IF NOT EXISTS FOR (f:File) ON (f.library);",
            "CREATE INDEX IF NOT EXISTS FOR (f:File) ON (f.has_errors);",
            "CREATE INDEX IF NOT EXISTS FOR (e:ParseError) ON (e.severity);",
            "CREATE INDEX IF NOT EXISTS FOR (e:ParseError) ON (e.error_type);",
            "CREATE INDEX IF NOT EXISTS FOR (e:ParseError) ON (e.timestamp);"
        ])
        
        # Create nodes
        for node in self.nodes:
            props_str = ", ".join([f"{k}: ${k}" for k in node.properties.keys()])
            statement = f"MERGE (n:{node.node_type.value} {{{props_str}}})"
            statements.append((statement, node.properties))
        
        # Create relationships
        for rel in self.relationships:
            from_match = f"MATCH (from {{node_id: '{rel.from_node_id}'}})"
            to_match = f"MATCH (to {{node_id: '{rel.to_node_id}'}})"
            props_str = ""
            if rel.properties:
                props_str = " {" + ", ".join([f"{k}: ${k}" for k in rel.properties.keys()]) + "}"
            create = f"MERGE (from)-[:{rel.rel_type.value}{props_str}]->(to)"
            statement = f"{from_match}\n{to_match}\n{create}"
            statements.append((statement, rel.properties))
        
        return statements
    
    def to_json(self, file_path: Path) -> None:
        """Export schema to JSON for visualization or processing"""
        data = {
            "nodes": [
                {
                    "id": node.node_id,
                    "type": node.node_type.value,
                    "properties": node.properties
                }
                for node in self.nodes
            ],
            "relationships": [
                {
                    "type": rel.rel_type.value,
                    "from": rel.from_node_id,
                    "to": rel.to_node_id,
                    "properties": rel.properties
                }
                for rel in self.relationships
            ]
        }
        
        with open(file_path, 'w') as f:
            json.dump(data, f, indent=2)
    
    def to_csv(self, output_dir: Path) -> None:
        """Export to CSV files for neo4j-admin import"""
        import csv
        
        output_dir.mkdir(exist_ok=True)
        
        # Export nodes by type
        for node_type in NodeType:
            nodes_of_type = [n for n in self.nodes if n.node_type == node_type]
            if nodes_of_type:
                file_path = output_dir / f"{node_type.value.lower()}_nodes.csv"
                
                # Get all unique properties
                all_props = set()
                for node in nodes_of_type:
                    all_props.update(node.properties.keys())
                
                with open(file_path, 'w', newline='') as f:
                    fieldnames = ['node_id'] + sorted(all_props)
                    writer = csv.DictWriter(f, fieldnames=fieldnames)
                    writer.writeheader()
                    
                    for node in nodes_of_type:
                        row = {'node_id': node.node_id}
                        row.update(node.properties)
                        writer.writerow(row)
        
        # Export relationships by type
        for rel_type in RelationType:
            rels_of_type = [r for r in self.relationships if r.rel_type == rel_type]
            if rels_of_type:
                file_path = output_dir / f"{rel_type.value.lower()}_relationships.csv"
                
                # Get all unique properties
                all_props = set()
                for rel in rels_of_type:
                    all_props.update(rel.properties.keys())
                
                with open(file_path, 'w', newline='') as f:
                    fieldnames = ['from_node_id', 'to_node_id'] + sorted(all_props)
                    writer = csv.DictWriter(f, fieldnames=fieldnames)
                    writer.writeheader()
                    
                    for rel in rels_of_type:
                        row = {
                            'from_node_id': rel.from_node_id,
                            'to_node_id': rel.to_node_id
                        }
                        row.update(rel.properties)
                        writer.writerow(row)


def create_schema_from_routines_with_errors(routines_by_file: Dict[str, List], 
                                           errors_by_file: Dict[str, List] = None) -> GraphSchema:
    """Create a graph schema from parsed routine data including error tracking"""
    schema = GraphSchema()
    
    # Track unique operations and precisions
    operations = set()
    precisions = set()
    
    for file_path, routines in routines_by_file.items():
        # Create file node
        path = Path(file_path)
        library = "UNKNOWN"
        
        # Determine library from path
        if "BLAS" in str(path):
            library = "BLAS"
        elif "LAPACK" in str(path) or "SRC" in str(path):
            library = "LAPACK"
        
        # Check if this file has errors
        has_errors = errors_by_file and file_path in errors_by_file
        
        file_node = GraphNode(
            node_type=NodeType.FILE,
            properties={
                "path": str(path),
                "name": path.name,
                "library": library,
                "directory": str(path.parent),
                "has_errors": has_errors,
                "parse_status": "partial" if has_errors else "complete"
            }
        )
        schema.add_node(file_node)
        
        # Create error nodes if any
        if errors_by_file and file_path in errors_by_file:
            for idx, error in enumerate(errors_by_file[file_path]):
                import hashlib
                import time
                
                # Generate unique error ID
                error_hash = hashlib.md5(f"{file_path}_{idx}_{error.get('message', '')}".encode()).hexdigest()[:8]
                error_id = f"{int(time.time())}_{error_hash}"
                
                error_node = GraphNode(
                    node_type=NodeType.PARSE_ERROR,
                    properties={
                        "id": error_id,
                        "error_type": error.get('type', 'unknown_error'),
                        "severity": error.get('severity', 'error'),
                        "message": error.get('message', ''),
                        "line_number": error.get('line_number'),
                        "column_number": error.get('column_number'),
                        "context": error.get('context', ''),
                        "timestamp": error.get('timestamp', int(time.time())),
                        "parser_version": error.get('parser_version', 'unknown')
                    }
                )
                schema.add_node(error_node)
                
                # Create HAS_ERROR relationship
                schema.add_relationship(GraphRelationship(
                    rel_type=RelationType.HAS_ERROR,
                    from_node_id=file_node.node_id,
                    to_node_id=error_node.node_id,
                    properties={"position": idx + 1}
                ))
        
        # Create routine nodes and relationships
        for routine in routines:
            # Create routine node
            routine_node = GraphNode(
                node_type=NodeType.ROUTINE,
                properties={
                    "name": routine.name,
                    "type": routine.routine_type,
                    "precision": routine.precision or "unknown",
                    "operation": routine.operation or routine.name,
                    "line_start": routine.line_start,
                    "line_end": routine.line_end
                }
            )
            schema.add_node(routine_node)
            
            # Create relationship based on parse status
            if has_errors:
                # Use PARTIAL_PARSE for files with errors
                schema.add_relationship(GraphRelationship(
                    rel_type=RelationType.PARTIAL_PARSE,
                    from_node_id=file_node.node_id,
                    to_node_id=routine_node.node_id,
                    properties={
                        "confidence": 0.8,  # Could be calculated based on error severity
                        "warnings": ["File had parsing errors, routine may be incomplete"]
                    }
                ))
            else:
                # Use standard DEFINED_IN for clean parses
                schema.add_relationship(GraphRelationship(
                    rel_type=RelationType.DEFINED_IN,
                    from_node_id=routine_node.node_id,
                    to_node_id=file_node.node_id
                ))
            
            # Track operations and precisions
            if routine.operation:
                operations.add(routine.operation)
            if routine.precision:
                precisions.add(routine.precision)
            
            # Create CALLS relationships
            for called in routine.calls:
                called_id = f"routine:{called}"
                schema.add_relationship(GraphRelationship(
                    rel_type=RelationType.CALLS,
                    from_node_id=routine_node.node_id,
                    to_node_id=called_id,
                    properties={"direct": True}
                ))
    
    # Create operation nodes
    for op in operations:
        op_node = GraphNode(
            node_type=NodeType.OPERATION,
            properties={"name": op}
        )
        schema.add_node(op_node)
        
        # Link routines to operations
        for node in schema.nodes:
            if (node.node_type == NodeType.ROUTINE and 
                node.properties.get("operation") == op):
                schema.add_relationship(GraphRelationship(
                    rel_type=RelationType.IMPLEMENTS,
                    from_node_id=node.node_id,
                    to_node_id=op_node.node_id
                ))
    
    # Create precision nodes
    precision_map = {'s': 'single', 'd': 'double', 'c': 'complex', 'z': 'double complex'}
    for prec in precisions:
        prec_node = GraphNode(
            node_type=NodeType.PRECISION,
            properties={
                "symbol": prec,
                "name": precision_map.get(prec, prec)
            }
        )
        schema.add_node(prec_node)
        
        # Link routines to precisions
        for node in schema.nodes:
            if (node.node_type == NodeType.ROUTINE and 
                node.properties.get("precision") == prec):
                schema.add_relationship(GraphRelationship(
                    rel_type=RelationType.HAS_PRECISION,
                    from_node_id=node.node_id,
                    to_node_id=prec_node.node_id
                ))
    
    return schema


def create_schema_from_routines(routines_by_file: Dict[str, List]) -> GraphSchema:
    """Create a graph schema from parsed routine data"""
    schema = GraphSchema()
    
    # Track unique operations and precisions
    operations = set()
    precisions = set()
    
    for file_path, routines in routines_by_file.items():
        # Create file node
        path = Path(file_path)
        library = "UNKNOWN"
        
        # Determine library from path
        if "BLAS" in str(path):
            library = "BLAS"
        elif "LAPACK" in str(path) or "SRC" in str(path):
            library = "LAPACK"
        
        file_node = GraphNode(
            node_type=NodeType.FILE,
            properties={
                "path": str(path),
                "name": path.name,
                "library": library,
                "directory": str(path.parent)
            }
        )
        schema.add_node(file_node)
        
        # Create routine nodes and relationships
        for routine in routines:
            # Create routine node
            routine_node = GraphNode(
                node_type=NodeType.ROUTINE,
                properties={
                    "name": routine.name,
                    "type": routine.routine_type,
                    "precision": routine.precision or "unknown",
                    "operation": routine.operation or routine.name,
                    "line_start": routine.line_start,
                    "line_end": routine.line_end
                }
            )
            schema.add_node(routine_node)
            
            # Create DEFINED_IN relationship
            schema.add_relationship(GraphRelationship(
                rel_type=RelationType.DEFINED_IN,
                from_node_id=routine_node.node_id,
                to_node_id=file_node.node_id
            ))
            
            # Track operations and precisions
            if routine.operation:
                operations.add(routine.operation)
            if routine.precision:
                precisions.add(routine.precision)
            
            # Create CALLS relationships
            for called in routine.calls:
                # Note: The called routine might not exist yet
                called_id = f"routine:{called}"
                schema.add_relationship(GraphRelationship(
                    rel_type=RelationType.CALLS,
                    from_node_id=routine_node.node_id,
                    to_node_id=called_id,
                    properties={"direct": True}
                ))
    
    # Create operation nodes
    for op in operations:
        op_node = GraphNode(
            node_type=NodeType.OPERATION,
            properties={"name": op}
        )
        schema.add_node(op_node)
        
        # Link routines to operations
        for node in schema.nodes:
            if (node.node_type == NodeType.ROUTINE and 
                node.properties.get("operation") == op):
                schema.add_relationship(GraphRelationship(
                    rel_type=RelationType.IMPLEMENTS,
                    from_node_id=node.node_id,
                    to_node_id=op_node.node_id
                ))
    
    # Create precision nodes
    precision_map = {'s': 'single', 'd': 'double', 'c': 'complex', 'z': 'double complex'}
    for prec in precisions:
        prec_node = GraphNode(
            node_type=NodeType.PRECISION,
            properties={
                "symbol": prec,
                "name": precision_map.get(prec, prec)
            }
        )
        schema.add_node(prec_node)
        
        # Link routines to precisions
        for node in schema.nodes:
            if (node.node_type == NodeType.ROUTINE and 
                node.properties.get("precision") == prec):
                schema.add_relationship(GraphRelationship(
                    rel_type=RelationType.HAS_PRECISION,
                    from_node_id=node.node_id,
                    to_node_id=prec_node.node_id
                ))
    
    return schema


# Example usage and documentation
SCHEMA_DOCUMENTATION = """
# LAPACK Graph Database Schema

## Node Types

### 1. Routine
Represents a Fortran subroutine or function.

Properties:
- name: Full routine name (e.g., "DGEMM")
- type: "subroutine" or "function"
- precision: Single character precision indicator ('s', 'd', 'c', 'z')
- operation: Base operation name without precision (e.g., "gemm")
- line_start: Starting line number in source file
- line_end: Ending line number in source file

### 2. File
Represents a Fortran source file.

Properties:
- path: Full file path
- name: File name
- library: "BLAS", "LAPACK", or "UNKNOWN"
- directory: Parent directory path

### 3. Operation
Represents a mathematical operation (e.g., matrix multiply, factorization).

Properties:
- name: Operation identifier (e.g., "gemm", "getrf")

### 4. Precision
Represents floating-point precision types.

Properties:
- symbol: Single character ('s', 'd', 'c', 'z')
- name: Full name ("single", "double", "complex", "double complex")

## Relationship Types

### 1. CALLS
Routine calls another routine.
- From: Routine
- To: Routine
- Properties: direct (boolean)

### 2. DEFINED_IN
Routine is defined in a file.
- From: Routine
- To: File

### 3. IMPLEMENTS
Routine implements an operation.
- From: Routine
- To: Operation

### 4. HAS_PRECISION
Routine has a specific precision.
- From: Routine
- To: Precision

## Example Queries

```cypher
// Find all routines that call DGEMM
MATCH (r:Routine)-[:CALLS]->(dgemm:Routine {name: 'DGEMM'})
RETURN r.name, r.precision

// Find all double precision matrix multiply routines
MATCH (r:Routine)-[:HAS_PRECISION]->(p:Precision {symbol: 'd'})
WHERE r.operation = 'gemm'
RETURN r.name, r.type

// Find dependency chain for DGETRF
MATCH path = (r:Routine {name: 'DGETRF'})-[:CALLS*1..5]->(dep:Routine)
RETURN path

// Find most called routines
MATCH (r:Routine)<-[:CALLS]-(caller:Routine)
RETURN r.name, count(caller) as call_count
ORDER BY call_count DESC
LIMIT 10
```
"""