#!/usr/bin/env python3
"""Test the complete analysis pipeline with modular architecture."""

import sys
from pathlib import Path
import tempfile
import json

# Add src to path for testing
sys.path.insert(0, str(Path(__file__).parent / "src"))

print("=== Testing Fortran-Mapper Analysis Pipeline ===\n")

# Step 1: Test imports
print("1. Testing imports...")
try:
    from fortran_mapper import FortranMapper, Graph, NodeType
    print("   ✅ Core imports successful")
except ImportError as e:
    print(f"   ❌ Core import failed: {e}")
    sys.exit(1)

try:
    from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator
    print("   ✅ LAPACK hooks available")
    LAPACK_AVAILABLE = True
except ImportError:
    print("   ⚠️  LAPACK hooks not installed (install with: cd hooks/lapack && uv pip install -e .)")
    LAPACK_AVAILABLE = False

# Step 2: Create test LAPACK-style code
print("\n2. Creating test Fortran files...")
test_code = {
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
    "sgetrf.f": '''
      SUBROUTINE SGETRF(M, N, A, LDA, IPIV, INFO)
      INTEGER M, N, LDA, INFO
      INTEGER IPIV(*)
      REAL A(LDA,*)
      
      CALL SGETF2(M, N, A, LDA, IPIV, INFO)
      END
    '''
}

with tempfile.TemporaryDirectory() as tmpdir:
    tmpdir = Path(tmpdir)
    
    # Create test files
    for filename, content in test_code.items():
        (tmpdir / filename).write_text(content)
    print(f"   ✅ Created {len(test_code)} test files in {tmpdir}")
    
    # Step 3: Test basic parsing (no hooks)
    print("\n3. Testing basic parsing (no hooks)...")
    mapper = FortranMapper()
    basic_graph = mapper.parse_directory(tmpdir)
    
    print(f"   ✅ Parsed {len(basic_graph.nodes)} nodes, {len(basic_graph.relationships)} relationships")
    
    # Step 4: Test with LAPACK hooks
    if LAPACK_AVAILABLE:
        print("\n4. Testing parsing with LAPACK hooks...")
        mapper_lapack = FortranMapper()
        mapper_lapack.register_hook("node_enricher", LapackNodeEnricher())
        mapper_lapack.register_hook("node_creator", LapackNodeCreator())
        
        lapack_graph = mapper_lapack.parse_directory(tmpdir)
        
        print(f"   ✅ Parsed {len(lapack_graph.nodes)} nodes, {len(lapack_graph.relationships)} relationships")
        
        # Check node types
        routines = lapack_graph.get_nodes_by_type(NodeType.ROUTINE)
        custom = lapack_graph.get_nodes_by_type(NodeType.CUSTOM)
        files = lapack_graph.get_nodes_by_type(NodeType.FILE)
        
        print(f"   📊 Node breakdown:")
        print(f"      - Routines: {len(routines)}")
        print(f"      - Files: {len(files)}")
        print(f"      - Custom (LAPACK): {len(custom)}")
        
        # Show enrichment
        print("\n   📋 Routine enrichment:")
        for routine in routines:
            props = routine.properties
            print(f"      {routine.name}: precision={props.get('precision', 'N/A')}, "
                  f"categories={props.get('categories', [])}")
        
        # Show custom nodes
        if custom:
            print("\n   🏷️  Custom nodes created:")
            for node in custom:
                print(f"      - {node.properties['custom_type']}: {node.name}")
    else:
        print("\n4. Skipping LAPACK hooks test (package not installed)")
        lapack_graph = basic_graph
    
    # Step 5: Test exports
    print("\n5. Testing export formats...")
    
    # JSON export
    json_file = tmpdir / "export.json"
    mapper.export_to_json(lapack_graph, json_file)
    with open(json_file) as f:
        json_data = json.load(f)
    print(f"   ✅ JSON export: {len(json_data['nodes'])} nodes")
    
    # DOT export
    dot_file = tmpdir / "export.dot"
    mapper.export_to_dot(lapack_graph, dot_file)
    print(f"   ✅ DOT export: {dot_file.stat().st_size} bytes")
    
    # GraphML export
    graphml_file = tmpdir / "export.graphml"
    mapper.export_to_graphml(lapack_graph, graphml_file)
    print(f"   ✅ GraphML export: {graphml_file.stat().st_size} bytes")
    
    # Neo4j Cypher export
    from fortran_mapper.exporters.neo4j import Neo4jExporter
    cypher_file = tmpdir / "export.cypher"
    exporter = Neo4jExporter()
    exporter.export_cypher_statements(lapack_graph, cypher_file)
    print(f"   ✅ Cypher export: {cypher_file.stat().st_size} bytes")
    
    # Step 6: Test Neo4j compatibility
    print("\n6. Testing Neo4j compatibility...")
    try:
        from neo4j import GraphDatabase
        print("   ✅ Neo4j driver available")
        print("   ℹ️  To test actual Neo4j export, ensure Neo4j is running and use:")
        print("      driver = GraphDatabase.driver('bolt://localhost:7687', auth=('neo4j', 'password'))")
        print("      mapper.export_to_neo4j(graph, driver)")
    except ImportError:
        print("   ⚠️  Neo4j driver not installed (optional)")

print("\n=== Pipeline Test Summary ===")
print("✅ Core parsing functionality works")
if LAPACK_AVAILABLE:
    print("✅ LAPACK hooks integration works")
else:
    print("⚠️  LAPACK hooks not tested (install package first)")
print("✅ All export formats work")
print("\n🎉 The modular fortran-mapper pipeline is functional!")

if not LAPACK_AVAILABLE:
    print("\nTo enable LAPACK analysis:")
    print("  cd hooks/lapack")
    print("  uv pip install -e .")
    print("  cd ../..")
    print("  python test_analysis_pipeline.py")