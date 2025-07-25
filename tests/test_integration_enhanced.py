#!/usr/bin/env python3
"""Enhanced integration tests for LAPACK analysis with multiple routine types and cross-file dependencies"""

import pytest
from pathlib import Path
import json
import csv
import tempfile
import shutil

from src.lapack_util.fortran_parser import FortranParser
from src.lapack_util.graph_schema import create_schema_from_routines
from src.lapack_util.exporter import Exporter


class TestMultiFileIntegration:
    """Test parsing multiple related LAPACK files and building complete dependency graphs"""
    
    @pytest.fixture
    def parser(self):
        return FortranParser()
    
    @pytest.fixture
    def lapack_files(self):
        """Dictionary of LAPACK files categorized by type"""
        examples_dir = Path("examples/lapack")
        return {
            'blas_level1': examples_dir / "ddot.f",      # BLAS Level 1: vector-vector
            'blas_level2': examples_dir / "dgemv.f",     # BLAS Level 2: matrix-vector
            'blas_level3': examples_dir / "dgemm.f",     # BLAS Level 3: matrix-matrix
            'blas_level3_solver': examples_dir / "dtrsm.f",  # BLAS Level 3: triangular solve
            'lapack_solver': examples_dir / "dgetrf.f",  # LAPACK: LU factorization
            'lapack_banded': examples_dir / "dgbtrf.f",  # LAPACK: banded LU factorization
        }
    
    def test_parse_multiple_routine_types(self, parser, lapack_files):
        """Test parsing different types of LAPACK routines"""
        routines_by_file = {}
        routine_types = {}
        
        # Parse all files
        for routine_type, file_path in lapack_files.items():
            if file_path.exists():
                result = parser.parse_file(file_path)
                assert result.error is None, f"Error parsing {file_path}: {result.error}"
                
                if result.routines:
                    routines_by_file[str(file_path)] = result.routines
                    routine_types[routine_type] = result.routines[0]
        
        # Verify we have different routine types
        assert len(routine_types) >= 4, "Need at least 4 different routine types for comprehensive testing"
        
        # Check BLAS Level 1 (DDOT)
        if 'blas_level1' in routine_types:
            ddot = routine_types['blas_level1']
            assert ddot.name == "DDOT"
            assert ddot.routine_type == "function"  # DDOT is a function, not subroutine
            assert ddot.precision == "d"
        
        # Check BLAS Level 2 (DGEMV)
        if 'blas_level2' in routine_types:
            dgemv = routine_types['blas_level2']
            assert dgemv.name == "DGEMV"
            assert dgemv.routine_type == "subroutine"
            assert dgemv.precision == "d"
            assert "XERBLA" in dgemv.calls  # Error handling
        
        # Check BLAS Level 3 (DGEMM)
        if 'blas_level3' in routine_types:
            dgemm = routine_types['blas_level3']
            assert dgemm.name == "DGEMM"
            assert dgemm.routine_type == "subroutine"
            assert dgemm.precision == "d"
            assert "XERBLA" in dgemm.calls
        
        # Check LAPACK solver (DGETRF)
        if 'lapack_solver' in routine_types:
            dgetrf = routine_types['lapack_solver']
            assert dgetrf.name == "DGETRF"
            assert dgetrf.routine_type == "subroutine"
            # Check it calls BLAS routines
            assert "DGEMM" in dgetrf.calls
            assert "DTRSM" in dgetrf.calls
            assert "DGETRF2" in dgetrf.calls  # Recursive factorization
        
        # Test completed successfully
    
    def test_cross_file_dependency_resolution(self, parser, lapack_files):
        """Test that dependencies between files are correctly resolved"""
        # Parse all available files
        routines_by_file = {}
        for _, file_path in lapack_files.items():
            if file_path.exists():
                result = parser.parse_file(file_path)
                if not result.error and result.routines:
                    routines_by_file[str(file_path)] = result.routines
        
        # Create graph schema
        schema = create_schema_from_routines(routines_by_file)
        
        # Build a map of routine names to their files
        routine_to_file = {}
        for file_path, routines in routines_by_file.items():
            for routine in routines:
                routine_to_file[routine.name] = Path(file_path).name
        
        # Verify cross-file dependencies
        cross_file_deps = []
        for rel in schema.relationships:
            if rel.rel_type.value == "CALLS":
                from_routine = rel.from_node_id.replace("routine:", "")
                to_routine = rel.to_node_id.replace("routine:", "")
                
                # Check if routines are from different files
                if (from_routine in routine_to_file and 
                    to_routine in routine_to_file and
                    routine_to_file[from_routine] != routine_to_file[to_routine]):
                    cross_file_deps.append({
                        'from': from_routine,
                        'to': to_routine,
                        'from_file': routine_to_file[from_routine],
                        'to_file': routine_to_file[to_routine]
                    })
        
        # Verify we found cross-file dependencies
        assert len(cross_file_deps) > 0, "No cross-file dependencies found"
        
        # Specific expected dependencies
        expected_deps = [
            ('DGETRF', 'DGEMM'),  # DGETRF calls DGEMM (different files)
            ('DGETRF', 'DTRSM'),  # DGETRF calls DTRSM (different files)
        ]
        
        for from_routine, to_routine in expected_deps:
            found = any(
                dep['from'] == from_routine and dep['to'] == to_routine
                for dep in cross_file_deps
            )
            assert found, f"Expected cross-file dependency {from_routine} -> {to_routine} not found"
        
        # Test completed successfully
    
    def test_complete_dependency_graph_building(self, parser, lapack_files):
        """Test building a complete dependency graph from multiple files"""
        # Parse all files
        routines_by_file = {}
        for _, file_path in lapack_files.items():
            if file_path.exists():
                result = parser.parse_file(file_path)
                if not result.error and result.routines:
                    routines_by_file[str(file_path)] = result.routines
        
        # Create schema
        schema = create_schema_from_routines(routines_by_file)
        
        # Verify graph completeness
        routine_nodes = [n for n in schema.nodes if n.node_type.value == "Routine"]
        file_nodes = [n for n in schema.nodes if n.node_type.value == "File"]
        
        # Should have nodes for all parsed routines
        parsed_routine_count = sum(len(routines) for routines in routines_by_file.values())
        assert len(routine_nodes) >= parsed_routine_count
        
        # Should have file nodes for all files
        assert len(file_nodes) == len(routines_by_file)
        
        # Check relationship types
        rel_types = set(r.rel_type.value for r in schema.relationships)
        assert "CALLS" in rel_types
        assert "DEFINED_IN" in rel_types
        
        # Build call graph for analysis
        call_graph = {}
        for rel in schema.relationships:
            if rel.rel_type.value == "CALLS":
                from_routine = rel.from_node_id.replace("routine:", "")
                to_routine = rel.to_node_id.replace("routine:", "")
                if from_routine not in call_graph:
                    call_graph[from_routine] = []
                call_graph[from_routine].append(to_routine)
        
        # Verify DGETRF's dependencies
        if "DGETRF" in call_graph:
            dgetrf_calls = call_graph["DGETRF"]
            assert "DGEMM" in dgetrf_calls, "DGETRF should call DGEMM"
            assert "DTRSM" in dgetrf_calls, "DGETRF should call DTRSM"
            assert "DGETRF2" in dgetrf_calls, "DGETRF should call DGETRF2"
        
        # Test completed successfully
    
    def test_export_complex_dependency_graph(self, parser, lapack_files):
        """Test exporting a complex dependency graph with multiple routine types"""
        # Build complete graph
        routines_by_file = {}
        for _, file_path in lapack_files.items():
            if file_path.exists():
                result = parser.parse_file(file_path)
                if not result.error and result.routines:
                    routines_by_file[str(file_path)] = result.routines
        
        schema = create_schema_from_routines(routines_by_file)
        
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)
            exporter = Exporter(schema)
            
            # Export all formats
            results = exporter.export_all(temp_path)
            
            # Validate all exports
            assert exporter.validate_export('json', results['json'])
            assert exporter.validate_export('csv', results['csv'])
            assert exporter.validate_export('cypher', results['cypher'])
            
            # Check JSON export for complex relationships
            with open(results['json'], 'r') as f:
                json_data = json.load(f)
            
            # Build node map
            node_map = {n['id']: n for n in json_data['nodes']}
            
            # Check for different routine types in export
            routine_types = set()
            for node in json_data['nodes']:
                if node['type'] == 'Routine':
                    props = node['properties']
                    if 'operation' in props:
                        routine_types.add(props['operation'])
            
            # Should have multiple routine types
            assert len(routine_types) >= 3, f"Expected multiple routine types, found: {routine_types}"
            
            # Verify cross-file relationships in export
            cross_file_calls = 0
            for rel in json_data['relationships']:
                if rel['type'] == 'CALLS':
                    # Skip if nodes don't exist (external routines)
                    if rel['from'] not in node_map or rel['to'] not in node_map:
                        continue
                    from_node = node_map[rel['from']]
                    to_node = node_map[rel['to']]
                    
                    # Get file names from DEFINED_IN relationships
                    from_file = None
                    to_file = None
                    
                    for r in json_data['relationships']:
                        if r['type'] == 'DEFINED_IN':
                            if r['from'] == from_node['id']:
                                file_node = node_map.get(r['to'])
                                if file_node and file_node['type'] == 'File':
                                    from_file = file_node['properties']['path']
                            if r['from'] == to_node['id']:
                                file_node = node_map.get(r['to'])
                                if file_node and file_node['type'] == 'File':
                                    to_file = file_node['properties']['path']
                    
                    if from_file and to_file and from_file != to_file:
                        cross_file_calls += 1
            
            assert cross_file_calls > 0, "No cross-file call relationships found in export"
            
            # Verify CSV export contains all nodes
            routine_csv = results['csv'] / 'routine_nodes.csv'
            with open(routine_csv, 'r') as f:
                reader = csv.DictReader(f)
                csv_routines = list(reader)
            
            # Should have entries for all routine types
            csv_routine_names = {r['name'] for r in csv_routines}
            expected_routines = {'DGEMM', 'DGETRF', 'DTRSM'}
            for routine in expected_routines:
                if routine in {r.name for routines in routines_by_file.values() for r in routines}:
                    assert routine in csv_routine_names, f"{routine} not found in CSV export"
    
    def test_blas_level_hierarchy(self, parser, lapack_files):
        """Test that BLAS level hierarchy is correctly captured"""
        # Parse files
        routines_by_file = {}
        blas_routines = {}
        
        for routine_type, file_path in lapack_files.items():
            if file_path.exists() and 'blas' in routine_type:
                result = parser.parse_file(file_path)
                if not result.error and result.routines:
                    routines_by_file[str(file_path)] = result.routines
                    routine = result.routines[0]
                    blas_routines[routine.name] = {
                        'level': routine_type,
                        'calls': routine.calls
                    }
        
        # Create schema
        schema = create_schema_from_routines(routines_by_file)
        
        # Check BLAS routine properties
        for node in schema.nodes:
            if node.node_type.value == "Routine" and node.properties['name'] in blas_routines:
                routine_name = node.properties['name']
                assert 'type' in node.properties
                assert 'precision' in node.properties
                
                # Verify precision is set correctly
                assert node.properties['precision'] == 'd', f"{routine_name} should have double precision"
        
        # Export and verify BLAS hierarchy is preserved
        with tempfile.TemporaryDirectory() as temp_dir:
            exporter = Exporter(schema)
            json_path = Path(temp_dir) / "blas_hierarchy.json"
            exporter.to_json(json_path)
            
            with open(json_path, 'r') as f:
                data = json.load(f)
            
            # Verify BLAS routines have correct properties in export
            for node in data['nodes']:
                if node['type'] == 'Routine' and node['properties']['name'] in blas_routines:
                    assert 'operation' in node['properties']
                    assert 'precision' in node['properties']


class TestCrossFileDependencies:
    """Focused tests for cross-file dependency resolution"""
    
    def test_lapack_to_blas_dependencies(self):
        """Test that LAPACK routines correctly reference BLAS routines from different files"""
        parser = FortranParser()
        examples_dir = Path("examples/lapack")
        
        # Parse LAPACK solver that uses BLAS
        dgetrf_result = parser.parse_file(examples_dir / "dgetrf.f")
        assert not dgetrf_result.error
        
        dgetrf = dgetrf_result.routines[0]
        
        # DGETRF should call these BLAS routines
        expected_blas_calls = ["DGEMM", "DTRSM"]
        for blas_routine in expected_blas_calls:
            assert blas_routine in dgetrf.calls, f"DGETRF should call {blas_routine}"
        
        # Now parse the BLAS files
        dgemm_result = parser.parse_file(examples_dir / "dgemm.f")
        dtrsm_result = parser.parse_file(examples_dir / "dtrsm.f")
        
        # Create schema with all files
        routines_by_file = {
            str(examples_dir / "dgetrf.f"): [dgetrf],
            str(examples_dir / "dgemm.f"): dgemm_result.routines if not dgemm_result.error else [],
            str(examples_dir / "dtrsm.f"): dtrsm_result.routines if not dtrsm_result.error else []
        }
        
        schema = create_schema_from_routines(routines_by_file)
        
        # Verify cross-file CALLS relationships exist
        calls_rels = [r for r in schema.relationships if r.rel_type.value == "CALLS"]
        
        # Check specific cross-file dependencies
        dgetrf_to_dgemm = any(
            r.from_node_id == "routine:DGETRF" and r.to_node_id == "routine:DGEMM"
            for r in calls_rels
        )
        assert dgetrf_to_dgemm, "DGETRF -> DGEMM cross-file dependency not found"
        
        dgetrf_to_dtrsm = any(
            r.from_node_id == "routine:DGETRF" and r.to_node_id == "routine:DTRSM"
            for r in calls_rels
        )
        assert dgetrf_to_dtrsm, "DGETRF -> DTRSM cross-file dependency not found"
    
    def test_missing_dependency_handling(self):
        """Test handling of calls to routines not in parsed files"""
        parser = FortranParser()
        examples_dir = Path("examples/lapack")
        
        # Parse only DGETRF (which calls routines not in our set)
        dgetrf_result = parser.parse_file(examples_dir / "dgetrf.f")
        
        routines_by_file = {
            str(examples_dir / "dgetrf.f"): dgetrf_result.routines
        }
        
        schema = create_schema_from_routines(routines_by_file)
        
        # Should still create relationships for missing routines
        calls_rels = [r for r in schema.relationships if r.rel_type.value == "CALLS"]
        
        # Check that external routine nodes are created
        all_called_routines = set()
        for rel in calls_rels:
            if rel.from_node_id == "routine:DGETRF":
                called_routine = rel.to_node_id.replace("routine:", "")
                all_called_routines.add(called_routine)
        
        # Should have calls to routines even if they're not parsed
        assert "DGETRF2" in all_called_routines, "Should create node for DGETRF2 even if not parsed"
        # Note: ILAENV is a function call, not a CALL statement, so it might not be captured
        # by our current parser which focuses on CALL statements
        
        # Note: The current implementation creates relationships to external routines
        # but doesn't create nodes for them. The relationships point to node IDs
        # that don't exist in the schema. This is a design choice that allows
        # the schema to represent calls to external routines without requiring
        # all external routines to be parsed.
        
        # Verify that only parsed routines have nodes
        routine_nodes = [n for n in schema.nodes if n.node_type.value == "Routine"]
        routine_names = {n.properties['name'] for n in routine_nodes}
        
        # Only DGETRF should have a node since it's the only parsed routine
        assert "DGETRF" in routine_names
        assert len(routine_names) == 1, "Should only have nodes for parsed routines"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])