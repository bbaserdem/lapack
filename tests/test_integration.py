#!/usr/bin/env python3
"""Integration tests for the complete LAPACK analysis pipeline"""

import pytest
from pathlib import Path
import json
import csv
import tempfile
import shutil

from src.lapack_util.fortran_parser import FortranParser
from src.lapack_util.graph_schema import create_schema_from_routines
from src.lapack_util.exporter import Exporter


def test_full_pipeline_with_sample_files():
    """Test the complete pipeline from parsing to export with real LAPACK files"""
    # Parse sample LAPACK files
    parser = FortranParser()
    examples_dir = Path("examples/lapack")
    
    dgemm_file = examples_dir / "dgemm.f"
    dgetrf_file = examples_dir / "dgetrf.f"
    
    assert dgemm_file.exists(), f"Sample file {dgemm_file} not found"
    assert dgetrf_file.exists(), f"Sample file {dgetrf_file} not found"
    
    # Parse files
    dgemm_result = parser.parse_file(dgemm_file)
    dgetrf_result = parser.parse_file(dgetrf_file)
    
    # Check for parsing errors
    assert dgemm_result.error is None, f"Error parsing DGEMM: {dgemm_result.error}"
    assert dgetrf_result.error is None, f"Error parsing DGETRF: {dgetrf_result.error}"
    
    dgemm_routines = dgemm_result.routines
    dgetrf_routines = dgetrf_result.routines
    
    # Verify parsing results
    assert len(dgemm_routines) == 1
    assert dgemm_routines[0].name == "DGEMM"
    assert dgemm_routines[0].routine_type == "subroutine"
    assert dgemm_routines[0].precision == "d"
    assert dgemm_routines[0].operation == "gemm"
    # XERBLA is called with CALL statement
    assert "XERBLA" in dgemm_routines[0].calls
    # Note: LSAME is a function called but not tracked by our parser currently
    
    assert len(dgetrf_routines) == 1
    assert dgetrf_routines[0].name == "DGETRF"
    assert dgetrf_routines[0].routine_type == "subroutine"
    assert dgetrf_routines[0].precision == "d"
    assert dgetrf_routines[0].operation == "getrf"
    assert "DGEMM" in dgetrf_routines[0].calls
    assert "DTRSM" in dgetrf_routines[0].calls
    assert "DGETRF2" in dgetrf_routines[0].calls
    
    # Create graph schema
    routines_by_file = {
        str(dgemm_file): dgemm_routines,
        str(dgetrf_file): dgetrf_routines
    }
    
    schema = create_schema_from_routines(routines_by_file)
    
    # Verify schema creation
    routine_nodes = [n for n in schema.nodes if n.node_type.value == "Routine"]
    file_nodes = [n for n in schema.nodes if n.node_type.value == "File"]
    
    assert len(routine_nodes) >= 2
    assert any(n.properties["name"] == "DGEMM" for n in routine_nodes)
    assert any(n.properties["name"] == "DGETRF" for n in routine_nodes)
    assert len(file_nodes) == 2
    
    # Verify relationships
    calls_rels = [r for r in schema.relationships if r.rel_type.value == "CALLS"]
    assert any(r.from_node_id == "routine:DGETRF" and r.to_node_id == "routine:DGEMM" for r in calls_rels)
    
    # Test export functionality
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        exporter = Exporter(schema)
        
        # Export all formats
        results = exporter.export_all(temp_path)
        
        # Verify all exports exist and are valid
        assert exporter.validate_export('json', results['json'])
        assert exporter.validate_export('csv', results['csv'])
        assert exporter.validate_export('cypher', results['cypher'])
        
        # Verify JSON export content
        with open(results['json'], 'r') as f:
            json_data = json.load(f)
        
        assert 'nodes' in json_data
        assert 'relationships' in json_data
        
        # Check that DGEMM and DGETRF are in the exported data
        node_names = [n['properties']['name'] for n in json_data['nodes'] if n['type'] == 'Routine']
        assert 'DGEMM' in node_names
        assert 'DGETRF' in node_names
        
        # Check relationships
        calls_rels = [r for r in json_data['relationships'] if r['type'] == 'CALLS']
        assert len(calls_rels) > 0
        
        # Verify CSV export files
        csv_files = list(results['csv'].glob("*.csv"))
        assert len(csv_files) > 0
        
        routine_csv = results['csv'] / 'routine_nodes.csv'
        assert routine_csv.exists()
        
        with open(routine_csv, 'r') as f:
            reader = csv.DictReader(f)
            routines = list(reader)
            routine_names = [r['name'] for r in routines]
            assert 'DGEMM' in routine_names
            assert 'DGETRF' in routine_names
        
        # Verify Cypher export
        with open(results['cypher'], 'r') as f:
            cypher_content = f.read()
        
        assert 'CREATE CONSTRAINT' in cypher_content
        assert 'MERGE (n:Routine' in cypher_content
        assert 'DGEMM' in cypher_content
        assert 'DGETRF' in cypher_content
        assert 'MERGE (from)-[:CALLS' in cypher_content


def test_export_preserves_call_hierarchy():
    """Test that export formats correctly preserve the call hierarchy"""
    parser = FortranParser()
    examples_dir = Path("examples/lapack")
    
    # Parse files
    dgemm_result = parser.parse_file(examples_dir / "dgemm.f")
    dgetrf_result = parser.parse_file(examples_dir / "dgetrf.f")
    
    # Create schema
    routines_by_file = {
        str(examples_dir / "dgemm.f"): dgemm_result.routines,
        str(examples_dir / "dgetrf.f"): dgetrf_result.routines
    }
    schema = create_schema_from_routines(routines_by_file)
    
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        exporter = Exporter(schema)
        
        # Export to JSON
        json_path = temp_path / "graph.json"
        exporter.to_json(json_path)
        
        with open(json_path, 'r') as f:
            data = json.load(f)
        
        # Build a map of node IDs to names
        node_map = {n['id']: n['properties'].get('name', '') for n in data['nodes']}
        
        # Find the DGETRF -> DGEMM call relationship
        dgetrf_calls_dgemm = False
        for rel in data['relationships']:
            if (rel['type'] == 'CALLS' and
                node_map.get(rel['from']) == 'DGETRF' and
                node_map.get(rel['to']) == 'DGEMM'):
                dgetrf_calls_dgemm = True
                break
        
        assert dgetrf_calls_dgemm, "DGETRF -> DGEMM call relationship not found in JSON export"
        
        # Export to CSV and verify
        csv_dir = temp_path / "csv"
        exporter.to_csv(csv_dir)
        
        calls_csv = csv_dir / "calls_relationships.csv"
        if calls_csv.exists():
            with open(calls_csv, 'r') as f:
                reader = csv.DictReader(f)
                calls = list(reader)
            
            dgetrf_calls_found = any(
                r['from_node_id'] == 'routine:DGETRF' and 
                r['to_node_id'] == 'routine:DGEMM'
                for r in calls
            )
            assert dgetrf_calls_found, "DGETRF -> DGEMM call not found in CSV export"
        
        # Export to Cypher and verify
        cypher_path = temp_path / "import.cypher"
        exporter.to_cypher(cypher_path)
        
        with open(cypher_path, 'r') as f:
            cypher_content = f.read()
        
        # Check for the specific relationship pattern
        assert "MATCH (from {node_id: 'routine:DGETRF'})" in cypher_content
        assert "MATCH (to {node_id: 'routine:DGEMM'})" in cypher_content


def test_roundtrip_data_integrity():
    """Test that data can be exported and imported without loss of information"""
    parser = FortranParser()
    examples_dir = Path("examples/lapack")
    
    # Parse files
    routines_by_file = {}
    for f in examples_dir.glob("*.f"):
        result = parser.parse_file(f)
        if result.routines and not result.error:
            routines_by_file[str(f)] = result.routines
    
    original_schema = create_schema_from_routines(routines_by_file)
    
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        exporter = Exporter(original_schema)
        
        # Export to JSON
        json_path = temp_path / "export.json"
        exporter.to_json(json_path)
        
        # Read back JSON
        with open(json_path, 'r') as f:
            json_data = json.load(f)
        
        # Verify all nodes are present
        assert len(json_data['nodes']) == len(original_schema.nodes)
        
        # Verify all relationships are present
        assert len(json_data['relationships']) == len(original_schema.relationships)
        
        # Verify node properties are preserved
        original_node_props = {
            n.node_id: n.properties 
            for n in original_schema.nodes
        }
        
        for json_node in json_data['nodes']:
            node_id = json_node['id']
            assert node_id in original_node_props
            
            # Check key properties
            if json_node['type'] == 'Routine':
                assert json_node['properties']['name'] == original_node_props[node_id]['name']
                assert json_node['properties']['type'] == original_node_props[node_id]['type']
        
        # Verify relationship properties are preserved
        original_rels = {
            (r.from_node_id, r.to_node_id, r.rel_type.value): r.properties
            for r in original_schema.relationships
        }
        
        for json_rel in json_data['relationships']:
            key = (json_rel['from'], json_rel['to'], json_rel['type'])
            assert key in original_rels
            
            # Check properties match
            if original_rels[key]:
                for prop_key, prop_value in original_rels[key].items():
                    assert json_rel['properties'][prop_key] == prop_value


if __name__ == "__main__":
    # Run tests
    pytest.main([__file__, "-v"])