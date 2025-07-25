#!/usr/bin/env python3
"""Tests for the graph schema module"""

import pytest
from pathlib import Path
import json
import tempfile
import csv

from src.lapack_util.graph_schema import (
    GraphSchema, GraphNode, GraphRelationship,
    NodeType, RelationType, create_schema_from_routines
)
from src.lapack_util.fortran_parser import Routine


def test_graph_node_creation():
    """Test creating graph nodes"""
    # Test routine node
    node = GraphNode(
        node_type=NodeType.ROUTINE,
        properties={"name": "DGEMM", "type": "subroutine"}
    )
    assert node.node_id == "routine:DGEMM"
    assert node.properties["name"] == "DGEMM"
    
    # Test file node
    file_node = GraphNode(
        node_type=NodeType.FILE,
        properties={"path": "BLAS/SRC/dgemm.f"}
    )
    assert file_node.node_id == "file:BLAS/SRC/dgemm.f"


def test_graph_schema_add_node():
    """Test adding nodes to schema"""
    schema = GraphSchema()
    
    node1 = GraphNode(NodeType.ROUTINE, {"name": "DGEMM"})
    node2 = GraphNode(NodeType.ROUTINE, {"name": "DGEMM"})  # Duplicate
    node3 = GraphNode(NodeType.ROUTINE, {"name": "DTRSM"})
    
    schema.add_node(node1)
    schema.add_node(node2)  # Should not add duplicate
    schema.add_node(node3)
    
    assert len(schema.nodes) == 2
    assert any(n.properties["name"] == "DGEMM" for n in schema.nodes)
    assert any(n.properties["name"] == "DTRSM" for n in schema.nodes)


def test_graph_schema_add_relationship():
    """Test adding relationships to schema"""
    schema = GraphSchema()
    
    rel1 = GraphRelationship(
        rel_type=RelationType.CALLS,
        from_node_id="routine:DGETRF",
        to_node_id="routine:DGEMM"
    )
    
    rel2 = GraphRelationship(
        rel_type=RelationType.CALLS,
        from_node_id="routine:DGETRF",
        to_node_id="routine:DGEMM"  # Duplicate
    )
    
    schema.add_relationship(rel1)
    schema.add_relationship(rel2)  # Should not add duplicate
    
    assert len(schema.relationships) == 1


def test_create_schema_from_routines():
    """Test creating schema from parsed routines"""
    # Create test routines
    dgemm = Routine(
        name="DGEMM",
        file_path="BLAS/SRC/dgemm.f",
        routine_type="subroutine",
        calls=set()
    )
    
    dgetrf = Routine(
        name="DGETRF",
        file_path="SRC/dgetrf.f",
        routine_type="subroutine",
        calls={"DGEMM", "DTRSM"}
    )
    
    routines_by_file = {
        "BLAS/SRC/dgemm.f": [dgemm],
        "SRC/dgetrf.f": [dgetrf]
    }
    
    schema = create_schema_from_routines(routines_by_file)
    
    # Check nodes were created
    routine_nodes = [n for n in schema.nodes if n.node_type == NodeType.ROUTINE]
    file_nodes = [n for n in schema.nodes if n.node_type == NodeType.FILE]
    operation_nodes = [n for n in schema.nodes if n.node_type == NodeType.OPERATION]
    precision_nodes = [n for n in schema.nodes if n.node_type == NodeType.PRECISION]
    
    assert len(routine_nodes) >= 2
    assert len(file_nodes) == 2
    assert len(operation_nodes) >= 1  # At least "gemm" and "getrf"
    assert len(precision_nodes) >= 1  # At least "d"
    
    # Check relationships
    calls_rels = [r for r in schema.relationships if r.rel_type == RelationType.CALLS]
    defined_in_rels = [r for r in schema.relationships if r.rel_type == RelationType.DEFINED_IN]
    
    assert len(calls_rels) >= 2  # DGETRF calls DGEMM and DTRSM
    assert len(defined_in_rels) >= 2  # Each routine defined in a file


def test_schema_to_json():
    """Test exporting schema to JSON"""
    schema = GraphSchema()
    
    node = GraphNode(NodeType.ROUTINE, {"name": "DGEMM"})
    schema.add_node(node)
    
    rel = GraphRelationship(
        RelationType.CALLS,
        "routine:DGETRF",
        "routine:DGEMM",
        {"direct": True}
    )
    schema.add_relationship(rel)
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
        schema.to_json(Path(f.name))
        
        # Read back and verify
        with open(f.name, 'r') as rf:
            data = json.load(rf)
        
        assert len(data["nodes"]) == 1
        assert data["nodes"][0]["id"] == "routine:DGEMM"
        assert len(data["relationships"]) == 1
        assert data["relationships"][0]["type"] == "CALLS"


def test_schema_to_csv():
    """Test exporting schema to CSV files"""
    schema = GraphSchema()
    
    # Add some nodes
    schema.add_node(GraphNode(NodeType.ROUTINE, {"name": "DGEMM", "type": "subroutine"}))
    schema.add_node(GraphNode(NodeType.FILE, {"path": "test.f", "library": "BLAS"}))
    
    # Add a relationship
    schema.add_relationship(GraphRelationship(
        RelationType.DEFINED_IN,
        "routine:DGEMM",
        "file:test.f"
    ))
    
    with tempfile.TemporaryDirectory() as tmpdir:
        output_dir = Path(tmpdir)
        schema.to_csv(output_dir)
        
        # Check files were created
        assert (output_dir / "routine_nodes.csv").exists()
        assert (output_dir / "file_nodes.csv").exists()
        assert (output_dir / "defined_in_relationships.csv").exists()
        
        # Verify routine nodes CSV
        with open(output_dir / "routine_nodes.csv", 'r') as f:
            reader = csv.DictReader(f)
            rows = list(reader)
            assert len(rows) == 1
            assert rows[0]["name"] == "DGEMM"


def test_cypher_statements():
    """Test generating Cypher statements"""
    schema = GraphSchema()
    
    schema.add_node(GraphNode(NodeType.ROUTINE, {"name": "DGEMM"}))
    
    statements = schema.to_cypher_statements()
    
    # Should have constraints/indexes plus node creation
    assert len(statements) > 7
    
    # Check for constraint creation
    constraint_stmts = [s for s in statements if isinstance(s, str) and "CONSTRAINT" in s]
    assert len(constraint_stmts) >= 4  # One for each node type
    
    # Check for index creation
    index_stmts = [s for s in statements if isinstance(s, str) and "INDEX" in s]
    assert len(index_stmts) >= 3


def test_library_detection():
    """Test that library detection works correctly"""
    routines_by_file = {
        "BLAS/SRC/dgemm.f": [Routine("DGEMM", "BLAS/SRC/dgemm.f", "subroutine")],
        "SRC/dgetrf.f": [Routine("DGETRF", "SRC/dgetrf.f", "subroutine")],
        "unknown/path.f": [Routine("TEST", "unknown/path.f", "subroutine")]
    }
    
    schema = create_schema_from_routines(routines_by_file)
    
    file_nodes = [n for n in schema.nodes if n.node_type == NodeType.FILE]
    
    blas_file = next(n for n in file_nodes if "BLAS" in n.properties["path"])
    assert blas_file.properties["library"] == "BLAS"
    
    lapack_file = next(n for n in file_nodes if "SRC/dgetrf.f" in n.properties["path"])
    assert lapack_file.properties["library"] == "LAPACK"
    
    unknown_file = next(n for n in file_nodes if "unknown" in n.properties["path"])
    assert unknown_file.properties["library"] == "UNKNOWN"


if __name__ == "__main__":
    # Run basic tests
    test_graph_node_creation()
    test_graph_schema_add_node()
    test_graph_schema_add_relationship()
    test_create_schema_from_routines()
    test_schema_to_json()
    test_schema_to_csv()
    test_cypher_statements()
    test_library_detection()
    
    print("All tests passed!")