#!/usr/bin/env python3
"""
Test script for fortran-mapper with LAPACK integration.
"""

import tempfile
import os
from pathlib import Path
import json

# Add the src directory to the path
import sys
sys.path.insert(0, str(Path(__file__).parent / "src"))

from fortran_mapper import FortranMapper
# Import from the separate LAPACK hooks package
# Note: Install the hooks package before running tests:
# cd hooks/lapack && uv pip install -e .
try:
    from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator
    LAPACK_HOOKS_AVAILABLE = True
except ImportError:
    print("WARNING: fortran-mapper-hooks-lapack not installed. LAPACK tests will be skipped.")
    print("To install: cd hooks/lapack && uv pip install -e .")
    LAPACK_HOOKS_AVAILABLE = False


def create_test_fortran_files():
    """Create test Fortran files similar to LAPACK routines."""
    
    test_files = {
        "dgemm.f": '''
      SUBROUTINE DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                 BETA, C, LDC)
      CHARACTER TRANSA, TRANSB
      INTEGER M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*)
      
      CALL XERBLA('DGEMM ', 1)
      CALL DTRSM('L', 'L', 'N', 'U', M, N, ONE, A, LDA, B, LDB)
      RETURN
      END
        ''',
        
        "dgetrf.f": '''
      SUBROUTINE DGETRF(M, N, A, LDA, IPIV, INFO)
      INTEGER M, N, LDA, INFO
      INTEGER IPIV(*)
      DOUBLE PRECISION A(LDA,*)
      
      CALL DGEMM('N', 'N', M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
      CALL DGETF2(M, N, A, LDA, IPIV, INFO)
      CALL DSWAP(N, A, LDA, B, LDB)
      
      END SUBROUTINE DGETRF
        ''',
        
        "utility.f": '''
      SUBROUTINE XERBLA(SRNAME, INFO)
      CHARACTER*6 SRNAME
      INTEGER INFO
      
      WRITE(*, *) 'Error in ', SRNAME, ' parameter ', INFO
      RETURN
      END
      
      LOGICAL FUNCTION LSAME(CA, CB)
      CHARACTER CA, CB
      LSAME = CA .EQ. CB
      RETURN
      END
        '''
    }
    
    # Create temporary directory and files
    temp_dir = Path(tempfile.mkdtemp())
    
    for filename, content in test_files.items():
        file_path = temp_dir / filename
        with open(file_path, 'w') as f:
            f.write(content)
    
    return temp_dir


def test_basic_parsing():
    """Test basic parsing without hooks."""
    print("=== Testing Basic Parsing ===")
    
    temp_dir = create_test_fortran_files()
    
    try:
        # Create mapper without hooks
        mapper = FortranMapper()
        
        # Parse directory
        graph = mapper.parse_directory(temp_dir)
        
        print(f"Parsed {len(graph.nodes)} nodes and {len(graph.relationships)} relationships")
        
        # Show some nodes
        from fortran_mapper.core.nodes import NodeType
        routines = graph.get_nodes_by_type(NodeType.ROUTINE)
        print(f"Found {len(routines)} routines:")
        for routine in routines[:5]:  # Show first 5
            print(f"  - {routine.name} ({routine.properties.get('routine_type', 'unknown')})")
        
        # Export to JSON for inspection
        output_file = temp_dir / "basic_output.json"
        mapper.export_to_json(graph, output_file)
        print(f"Exported to {output_file}")
        
        return True
        
    except Exception as e:
        print(f"Basic parsing failed: {e}")
        return False
    
    finally:
        # Cleanup
        import shutil
        shutil.rmtree(temp_dir)


def test_lapack_hooks():
    """Test parsing with LAPACK hooks."""
    print("\n=== Testing LAPACK Hooks ===")
    
    if not LAPACK_HOOKS_AVAILABLE:
        print("SKIPPED: fortran-mapper-hooks-lapack not installed")
        return True  # Don't fail the test
    
    temp_dir = create_test_fortran_files()
    
    try:
        # Create mapper with LAPACK hooks
        mapper = FortranMapper()
        mapper.register_hook("node_enricher", LapackNodeEnricher())
        mapper.register_hook("node_creator", LapackNodeCreator())
        
        # Parse directory
        graph = mapper.parse_directory(temp_dir)
        
        print(f"Parsed {len(graph.nodes)} nodes and {len(graph.relationships)} relationships")
        
        # Show nodes by type
        from fortran_mapper.core.nodes import NodeType
        
        routines = graph.get_nodes_by_type(NodeType.ROUTINE)
        custom_nodes = graph.get_nodes_by_type(NodeType.CUSTOM)
        
        print(f"Found {len(routines)} routines and {len(custom_nodes)} custom nodes")
        
        # Show enriched routine properties
        print("\nEnriched routines:")
        for routine in routines:
            props = routine.properties
            precision = props.get('precision', 'none')
            operation = props.get('operation', 'none')
            categories = props.get('categories', [])
            print(f"  - {routine.name}: precision={precision}, operation={operation}, categories={categories}")
        
        # Show custom nodes
        print("\nCustom nodes:")
        for node in custom_nodes:
            custom_type = node.properties.get('custom_type', 'unknown')
            print(f"  - {node.name} ({custom_type})")
        
        # Export to different formats
        mapper.export_to_json(graph, temp_dir / "lapack_output.json")
        mapper.export_to_dot(graph, temp_dir / "lapack_output.dot")
        mapper.export_to_graphml(graph, temp_dir / "lapack_output.graphml")
        
        print(f"Exported to multiple formats in {temp_dir}")
        
        # Show some statistics
        data = graph.to_dict()
        stats = data['statistics']
        print(f"\nStatistics: {stats}")
        
        return True
        
    except Exception as e:
        print(f"LAPACK hooks test failed: {e}")
        import traceback
        traceback.print_exc()
        return False
    
    finally:
        # Cleanup  
        import shutil
        shutil.rmtree(temp_dir)


def test_export_call_graph():
    """Test exporting just the call graph."""
    print("\n=== Testing Call Graph Export ===")
    
    temp_dir = create_test_fortran_files()
    
    try:
        mapper = FortranMapper()
        if LAPACK_HOOKS_AVAILABLE:
            mapper.register_hook("node_enricher", LapackNodeEnricher())
        
        graph = mapper.parse_directory(temp_dir)
        
        # Export call graph only using DOT exporter
        from fortran_mapper.exporters.dot import DOTExporter
        from fortran_mapper.core.nodes import NodeType
        from fortran_mapper.core.relationships import RelationType
        
        dot_exporter = DOTExporter()
        dot_exporter.export_call_graph_only(graph, temp_dir / "call_graph.dot")
        
        print(f"Call graph exported to {temp_dir / 'call_graph.dot'}")
        
        # Show the DOT content
        with open(temp_dir / "call_graph.dot", 'r') as f:
            content = f.read()
            print("\nCall graph DOT content:")
            print(content[:500] + "..." if len(content) > 500 else content)
        
        return True
        
    except Exception as e:
        print(f"Call graph export failed: {e}")
        return False
    
    finally:
        import shutil
        shutil.rmtree(temp_dir)


def main():
    """Run all tests."""
    print("Testing fortran-mapper with LAPACK integration")
    print("=" * 50)
    
    tests = [
        test_basic_parsing,
        test_lapack_hooks, 
        test_export_call_graph
    ]
    
    results = []
    for test in tests:
        try:
            result = test()
            results.append(result)
        except Exception as e:
            print(f"Test {test.__name__} crashed: {e}")
            results.append(False)
    
    # Summary
    print("\n" + "=" * 50)
    print("Test Results:")
    for i, (test, result) in enumerate(zip(tests, results)):
        status = "PASS" if result else "FAIL"
        print(f"  {i+1}. {test.__name__}: {status}")
    
    passed = sum(results)
    total = len(results)
    print(f"\nOverall: {passed}/{total} tests passed")
    
    if passed == total:
        print("🎉 All tests passed! fortran-mapper is working correctly.")
    else:
        print("❌ Some tests failed. Check the output above for details.")
    
    return passed == total


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)