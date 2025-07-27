#!/usr/bin/env python3
"""Tests for the Exporter class"""

import pytest
from pathlib import Path
import json
import csv
import tempfile
import shutil

from src.lapack_util.exporter import Exporter
from src.lapack_util.graph_schema import (
    GraphSchema, GraphNode, GraphRelationship,
    NodeType, RelationType, create_schema_from_routines
)
from src.lapack_util.fortran_parser import Routine


@pytest.fixture
def sample_schema():
    """Create a sample graph schema for testing"""
    schema = GraphSchema()
    
    # Add routine nodes
    dgemm = GraphNode(
        node_type=NodeType.ROUTINE,
        properties={
            "name": "DGEMM",
            "type": "subroutine",
            "precision": "d",
            "operation": "gemm",
            "line_start": 1,
            "line_end": 100
        }
    )
    dgetrf = GraphNode(
        node_type=NodeType.ROUTINE,
        properties={
            "name": "DGETRF",
            "type": "subroutine",
            "precision": "d",
            "operation": "getrf",
            "line_start": 1,
            "line_end": 150
        }
    )
    schema.add_node(dgemm)
    schema.add_node(dgetrf)
    
    # Add file nodes
    dgemm_file = GraphNode(
        node_type=NodeType.FILE,
        properties={
            "path": "BLAS/SRC/dgemm.f",
            "name": "dgemm.f",
            "library": "BLAS",
            "directory": "BLAS/SRC"
        }
    )
    dgetrf_file = GraphNode(
        node_type=NodeType.FILE,
        properties={
            "path": "SRC/dgetrf.f",
            "name": "dgetrf.f",
            "library": "LAPACK",
            "directory": "SRC"
        }
    )
    schema.add_node(dgemm_file)
    schema.add_node(dgetrf_file)
    
    # Add operation nodes
    gemm_op = GraphNode(
        node_type=NodeType.OPERATION,
        properties={"name": "gemm"}
    )
    getrf_op = GraphNode(
        node_type=NodeType.OPERATION,
        properties={"name": "getrf"}
    )
    schema.add_node(gemm_op)
    schema.add_node(getrf_op)
    
    # Add precision node
    double_prec = GraphNode(
        node_type=NodeType.PRECISION,
        properties={
            "symbol": "d",
            "name": "double"
        }
    )
    schema.add_node(double_prec)
    
    # Add relationships
    schema.add_relationship(GraphRelationship(
        rel_type=RelationType.DEFINED_IN,
        from_node_id=dgemm.node_id,
        to_node_id=dgemm_file.node_id
    ))
    schema.add_relationship(GraphRelationship(
        rel_type=RelationType.DEFINED_IN,
        from_node_id=dgetrf.node_id,
        to_node_id=dgetrf_file.node_id
    ))
    schema.add_relationship(GraphRelationship(
        rel_type=RelationType.CALLS,
        from_node_id=dgetrf.node_id,
        to_node_id=dgemm.node_id,
        properties={"direct": True}
    ))
    schema.add_relationship(GraphRelationship(
        rel_type=RelationType.IMPLEMENTS,
        from_node_id=dgemm.node_id,
        to_node_id=gemm_op.node_id
    ))
    schema.add_relationship(GraphRelationship(
        rel_type=RelationType.IMPLEMENTS,
        from_node_id=dgetrf.node_id,
        to_node_id=getrf_op.node_id
    ))
    schema.add_relationship(GraphRelationship(
        rel_type=RelationType.HAS_PRECISION,
        from_node_id=dgemm.node_id,
        to_node_id=double_prec.node_id
    ))
    schema.add_relationship(GraphRelationship(
        rel_type=RelationType.HAS_PRECISION,
        from_node_id=dgetrf.node_id,
        to_node_id=double_prec.node_id
    ))
    
    return schema


@pytest.fixture
def temp_dir():
    """Create a temporary directory for testing"""
    temp_dir = tempfile.mkdtemp()
    yield Path(temp_dir)
    shutil.rmtree(temp_dir)


def test_exporter_initialization(sample_schema):
    """Test that Exporter initializes correctly"""
    exporter = Exporter(sample_schema)
    assert exporter.graph == sample_schema


def test_json_export(sample_schema, temp_dir):
    """Test JSON export functionality"""
    exporter = Exporter(sample_schema)
    json_path = temp_dir / "test_export.json"
    
    # Export to JSON
    exporter.to_json(json_path)
    
    # Verify file exists
    assert json_path.exists()
    
    # Load and verify content
    with open(json_path, 'r') as f:
        data = json.load(f)
    
    assert 'nodes' in data
    assert 'relationships' in data
    assert len(data['nodes']) == 7  # 2 routines, 2 files, 2 operations, 1 precision
    assert len(data['relationships']) == 7  # 2 defined_in, 1 calls, 2 implements, 2 has_precision
    
    # Check node structure
    routine_nodes = [n for n in data['nodes'] if n['type'] == 'Routine']
    assert len(routine_nodes) == 2
    assert any(n['properties']['name'] == 'DGEMM' for n in routine_nodes)
    assert any(n['properties']['name'] == 'DGETRF' for n in routine_nodes)
    
    # Check relationship structure
    calls_rels = [r for r in data['relationships'] if r['type'] == 'CALLS']
    assert len(calls_rels) == 1
    assert calls_rels[0]['properties']['direct'] == True
    
    # Validate export
    assert exporter.validate_export('json', json_path)


def test_csv_export(sample_schema, temp_dir):
    """Test CSV export functionality"""
    exporter = Exporter(sample_schema)
    csv_dir = temp_dir / "csv_export"
    
    # Export to CSV
    csv_files = exporter.to_csv(csv_dir)
    
    # Verify directory exists
    assert csv_dir.exists()
    assert len(csv_files) > 0
    
    # Check for expected files
    expected_files = [
        'routine_nodes.csv',
        'file_nodes.csv',
        'operation_nodes.csv',
        'precision_nodes.csv',
        'defined_in_relationships.csv',
        'calls_relationships.csv',
        'implements_relationships.csv',
        'has_precision_relationships.csv'
    ]
    
    actual_files = [f.name for f in csv_files]
    for expected in expected_files:
        assert expected in actual_files
    
    # Verify routine nodes CSV content
    routine_csv = csv_dir / 'routine_nodes.csv'
    with open(routine_csv, 'r') as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        assert len(rows) == 2
        names = [r['name'] for r in rows]
        assert 'DGEMM' in names
        assert 'DGETRF' in names
    
    # Verify relationships CSV content
    calls_csv = csv_dir / 'calls_relationships.csv'
    with open(calls_csv, 'r') as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        assert len(rows) == 1
        assert rows[0]['from_node_id'] == 'routine:DGETRF'
        assert rows[0]['to_node_id'] == 'routine:DGEMM'
        assert rows[0]['direct'] == 'True'
    
    # Validate export
    assert exporter.validate_export('csv', csv_dir)


def test_cypher_export(sample_schema, temp_dir):
    """Test Cypher export functionality"""
    exporter = Exporter(sample_schema)
    cypher_path = temp_dir / "import.cypher"
    
    # Export to Cypher
    exporter.to_cypher(cypher_path)
    
    # Verify file exists
    assert cypher_path.exists()
    
    # Read and verify content
    with open(cypher_path, 'r') as f:
        content = f.read()
    
    # Check for expected elements
    assert "CREATE CONSTRAINT" in content
    assert "CREATE INDEX" in content
    assert "MERGE" in content
    assert "Routine" in content
    assert "File" in content
    assert "Operation" in content
    assert "Precision" in content
    assert "CALLS" in content
    assert "DEFINED_IN" in content
    assert "IMPLEMENTS" in content
    assert "HAS_PRECISION" in content
    
    # Check for specific nodes
    assert "DGEMM" in content
    assert "DGETRF" in content
    assert "gemm" in content
    assert "getrf" in content
    
    # Validate export
    assert exporter.validate_export('cypher', cypher_path)


def test_export_all(sample_schema, temp_dir):
    """Test exporting to all formats at once"""
    exporter = Exporter(sample_schema)
    
    # Export all formats
    results = exporter.export_all(temp_dir)
    
    # Check that all exports were created
    assert 'json' in results
    assert 'csv' in results
    assert 'cypher' in results
    assert 'csv_files' in results
    
    # Verify files exist
    assert results['json'].exists()
    assert results['csv'].exists()
    assert results['cypher'].exists()
    assert len(results['csv_files']) > 0
    
    # Validate all exports
    assert exporter.validate_export('json', results['json'])
    assert exporter.validate_export('csv', results['csv'])
    assert exporter.validate_export('cypher', results['cypher'])


def test_data_integrity_across_formats(sample_schema, temp_dir):
    """Test that data integrity is preserved across all export formats"""
    exporter = Exporter(sample_schema)
    
    # Export all formats
    results = exporter.export_all(temp_dir)
    
    # Load JSON data
    with open(results['json'], 'r') as f:
        json_data = json.load(f)
    
    # Count nodes and relationships in JSON
    json_node_count = len(json_data['nodes'])
    json_rel_count = len(json_data['relationships'])
    
    # Count CSV files and rows
    csv_node_count = 0
    csv_rel_count = 0
    
    for csv_file in results['csv_files']:
        with open(csv_file, 'r') as f:
            reader = csv.DictReader(f)
            rows = list(reader)
            if '_nodes.csv' in csv_file.name:
                csv_node_count += len(rows)
            elif '_relationships.csv' in csv_file.name:
                csv_rel_count += len(rows)
    
    # Verify counts match for JSON and CSV
    assert json_node_count == csv_node_count == len(sample_schema.nodes)
    assert json_rel_count == csv_rel_count == len(sample_schema.relationships)
    
    # Verify Cypher file contains expected content
    with open(results['cypher'], 'r') as f:
        cypher_content = f.read()
    
    # Check that Cypher contains all expected node types and relationships
    assert 'MERGE (n:Routine' in cypher_content
    assert 'MERGE (n:File' in cypher_content
    assert 'MERGE (n:Operation' in cypher_content
    assert 'MERGE (n:Precision' in cypher_content
    assert 'MERGE (from)-[:CALLS' in cypher_content
    assert 'MERGE (from)-[:DEFINED_IN' in cypher_content
    assert 'MERGE (from)-[:IMPLEMENTS' in cypher_content
    assert 'MERGE (from)-[:HAS_PRECISION' in cypher_content


def test_export_empty_schema(temp_dir):
    """Test exporting an empty schema"""
    empty_schema = GraphSchema()
    exporter = Exporter(empty_schema)
    
    # Export all formats
    results = exporter.export_all(temp_dir)
    
    # Verify exports work with empty data
    with open(results['json'], 'r') as f:
        json_data = json.load(f)
    assert json_data['nodes'] == []
    assert json_data['relationships'] == []
    
    # CSV directory should exist but may not have data files
    assert results['csv'].exists()
    
    # Cypher file should have constraints/indexes but no data
    with open(results['cypher'], 'r') as f:
        cypher_content = f.read()
    assert "CREATE CONSTRAINT" in cypher_content
    assert "CREATE INDEX" in cypher_content


def test_export_with_complex_properties(temp_dir):
    """Test exporting nodes with various property types"""
    schema = GraphSchema()
    
    # Add node with various property types
    complex_node = GraphNode(
        node_type=NodeType.ROUTINE,
        properties={
            "name": "TEST_ROUTINE",
            "line_count": 150,
            "is_deprecated": False,
            "precision": "s",
            "authors": ["author1", "author2"],  # List property
            "metadata": {"version": "1.0", "date": "2024"}  # Dict property
        }
    )
    schema.add_node(complex_node)
    
    exporter = Exporter(schema)
    
    # Export to JSON (should handle complex types)
    json_path = temp_dir / "complex.json"
    exporter.to_json(json_path)
    
    with open(json_path, 'r') as f:
        data = json.load(f)
    
    assert len(data['nodes']) == 1
    node_props = data['nodes'][0]['properties']
    assert node_props['name'] == "TEST_ROUTINE"
    assert node_props['line_count'] == 150
    assert node_props['is_deprecated'] == False
    assert node_props['authors'] == ["author1", "author2"]
    assert node_props['metadata'] == {"version": "1.0", "date": "2024"}


def test_validate_export_invalid_cases(sample_schema, temp_dir):
    """Test validation with invalid exports"""
    exporter = Exporter(sample_schema)
    
    # Test non-existent file
    assert not exporter.validate_export('json', temp_dir / 'nonexistent.json')
    
    # Test empty file
    empty_file = temp_dir / 'empty.json'
    empty_file.touch()
    assert not exporter.validate_export('json', empty_file)
    
    # Test invalid JSON
    invalid_json = temp_dir / 'invalid.json'
    with open(invalid_json, 'w') as f:
        f.write("{invalid json")
    assert not exporter.validate_export('json', invalid_json)
    
    # Test JSON with wrong structure
    wrong_structure = temp_dir / 'wrong.json'
    with open(wrong_structure, 'w') as f:
        json.dump({"wrong": "structure"}, f)
    assert not exporter.validate_export('json', wrong_structure)
    
    # Test CSV validation with file instead of directory
    assert not exporter.validate_export('csv', empty_file)
    
    # Test Cypher validation with file without expected content
    bad_cypher = temp_dir / 'bad.cypher'
    with open(bad_cypher, 'w') as f:
        f.write("-- Just a comment")
    assert not exporter.validate_export('cypher', bad_cypher)


if __name__ == "__main__":
    # Run tests
    pytest.main([__file__, "-v"])