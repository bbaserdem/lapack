# Fortran-Mapper Modular Architecture Test Workflow

This document provides a comprehensive testing workflow for the modular fortran-mapper with separated LAPACK hooks.

## Prerequisites

1. Ensure the development environment is set up:
   ```bash
   # Enter the nix development shell
   direnv allow
   # or manually: nix develop
   ```

2. Install the fortran-mapper package:
   ```bash
   # From the root directory
   uv sync
   ```

3. Install the LAPACK hooks package (optional):
   ```bash
   cd hooks/lapack
   uv pip install -e .
   cd ../..
   ```

4. Verify fortran-src is available:
   ```bash
   which fortran-src
   fortran-src --version
   ```

## 1. Package Installation and Import Verification

### 1.1 Core Package Test
```bash
# Test Python import of core package
python -c "import fortran_mapper; print(f'Core version: {fortran_mapper.__version__}')"

# Test core imports
python -c "from fortran_mapper import FortranMapper, Graph, NodeType; print('Core imports successful')"
```

**Expected Result**: No import errors, version should display correctly.

### 1.2 LAPACK Hooks Package Test
```bash
# Test LAPACK hooks import
python -c "from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator; print('LAPACK hooks imported successfully')"

# Run LAPACK hooks test
cd hooks/lapack
python test_lapack_hooks.py
cd ../..
```

**Expected Result**: 
- LAPACK hooks should import successfully
- Test script should show all tests passing

## 2. Basic Fortran Parsing (Without LAPACK hooks)

### 2.1 Generic Fortran Parsing Test
```bash
# Run the basic test (without LAPACK hooks)
python test_fortran_mapper.py
```

**Expected Result**:
- Basic parsing test should pass
- LAPACK hooks test should be skipped if package not installed
- Call graph export should work

### 2.2 Parse Generic Fortran Code
```python
# Test generic parsing without domain-specific hooks
cat > test_generic_parsing.py << 'EOF'
from fortran_mapper import FortranMapper
from pathlib import Path
import tempfile

# Create a test Fortran file
test_code = '''
      SUBROUTINE MYSUB(X, Y, N)
      INTEGER N
      REAL X(N), Y(N)
      
      CALL HELPER(X, N)
      Y = X * 2.0
      
      END SUBROUTINE
      
      SUBROUTINE HELPER(A, M)
      INTEGER M
      REAL A(M)
      A = A + 1.0
      END
'''

with tempfile.TemporaryDirectory() as tmpdir:
    test_file = Path(tmpdir) / "test.f"
    test_file.write_text(test_code)
    
    # Parse without any hooks
    mapper = FortranMapper()
    graph = mapper.parse_file(test_file)
    
    print(f"Nodes: {len(graph.nodes)}")
    print(f"Relationships: {len(graph.relationships)}")
    
    # Export to JSON
    mapper.export_to_json(graph, Path(tmpdir) / "output.json")
    print("Export successful!")
EOF

python test_generic_parsing.py
```

**Expected Result**:
- Should parse 3 nodes (2 routines, 1 file)
- Should find 1 CALLS relationship
- JSON export should work

## 3. LAPACK-Specific Parsing (With hooks)

### 3.1 Test LAPACK Hooks Integration
```python
# Test LAPACK-specific parsing
cat > test_lapack_parsing.py << 'EOF'
from fortran_mapper import FortranMapper
from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator
from pathlib import Path
import tempfile

# Create LAPACK-style test file
lapack_code = '''
      SUBROUTINE DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, 
     $                 B, LDB, BETA, C, LDC)
      CHARACTER TRANSA, TRANSB
      INTEGER M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*)
      
      CALL XERBLA('DGEMM ', 1)
      END
      
      SUBROUTINE SGETRF(M, N, A, LDA, IPIV, INFO)
      INTEGER M, N, LDA, INFO
      INTEGER IPIV(*)
      REAL A(LDA,*)
      
      CALL SGETF2(M, N, A, LDA, IPIV, INFO)
      END
'''

with tempfile.TemporaryDirectory() as tmpdir:
    test_file = Path(tmpdir) / "lapack_test.f"
    test_file.write_text(lapack_code)
    
    # Parse with LAPACK hooks
    mapper = FortranMapper()
    mapper.register_hook("node_enricher", LapackNodeEnricher())
    mapper.register_hook("node_creator", LapackNodeCreator())
    
    graph = mapper.parse_file(test_file)
    
    print(f"Total nodes: {len(graph.nodes)}")
    print(f"Relationships: {len(graph.relationships)}")
    
    # Check for custom nodes
    from fortran_mapper.core.nodes import NodeType
    custom_nodes = graph.get_nodes_by_type(NodeType.CUSTOM)
    print(f"Custom nodes: {len(custom_nodes)}")
    
    for node in custom_nodes:
        print(f"  - {node.properties['custom_type']}: {node.name}")
    
    # Check enrichment
    routines = graph.get_nodes_by_type(NodeType.ROUTINE)
    for routine in routines:
        props = routine.properties
        print(f"\n{routine.name}:")
        print(f"  Precision: {props.get('precision', 'N/A')}")
        print(f"  Operation: {props.get('operation', 'N/A')}")
        print(f"  Categories: {props.get('categories', [])}")
EOF

python test_lapack_parsing.py
```

**Expected Result**:
- Should create additional custom nodes (Precision, Operation, MatrixType)
- Routines should have precision and operation properties
- Categories should be identified (blas3, lapack, etc.)

## 4. Neo4j Export Test

### 4.1 Test Neo4j Export Structure
```python
# Test Neo4j export functionality
cat > test_neo4j_export.py << 'EOF'
from fortran_mapper import FortranMapper
from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator
from pathlib import Path
import tempfile

# Create test data
test_code = '''
      SUBROUTINE DGETRF(M, N, A, LDA, IPIV, INFO)
      INTEGER M, N, LDA, INFO
      DOUBLE PRECISION A(LDA,*)
      CALL DGETF2(M, N, A, LDA, IPIV, INFO)
      END
'''

with tempfile.TemporaryDirectory() as tmpdir:
    test_file = Path(tmpdir) / "test.f"
    test_file.write_text(test_code)
    
    mapper = FortranMapper()
    mapper.register_hook("node_enricher", LapackNodeEnricher())
    mapper.register_hook("node_creator", LapackNodeCreator())
    
    graph = mapper.parse_file(test_file)
    
    # Export to Cypher statements
    from fortran_mapper.exporters.neo4j import Neo4jExporter
    exporter = Neo4jExporter()
    cypher_file = Path(tmpdir) / "test.cypher"
    exporter.export_cypher_statements(graph, cypher_file)
    
    # Show Cypher content
    print("Generated Cypher statements:")
    print(cypher_file.read_text()[:500] + "...")
EOF

python test_neo4j_export.py
```

**Expected Result**:
- Should generate valid Cypher statements
- Should include CREATE statements for all node types
- Should include relationship creation

## 5. Full Integration Test with LAPACK Source

### 5.1 Parse Real LAPACK Files
```bash
# Assuming LAPACK source is available
# Adjust path as needed
LAPACK_SRC="/path/to/lapack/SRC"

if [ -d "$LAPACK_SRC" ]; then
    python << 'EOF'
import sys
from pathlib import Path
from fortran_mapper import FortranMapper
from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator

lapack_src = Path(sys.argv[1])

# Create mapper with LAPACK hooks
mapper = FortranMapper()
mapper.register_hook("node_enricher", LapackNodeEnricher())
mapper.register_hook("node_creator", LapackNodeCreator())

# Parse a subset of files
test_files = list(lapack_src.glob("dge*.f"))[:5]
print(f"Parsing {len(test_files)} files...")

for file_path in test_files:
    print(f"  Parsing {file_path.name}")
    graph = mapper.parse_file(file_path)

# Parse as directory
graph = mapper.parse_directory(lapack_src, extensions=['.f'])

print(f"\nTotal nodes: {len(graph.nodes)}")
print(f"Total relationships: {len(graph.relationships)}")

# Analyze by node type
from fortran_mapper.core.nodes import NodeType
for node_type in NodeType:
    nodes = graph.get_nodes_by_type(node_type)
    if nodes:
        print(f"{node_type.value}: {len(nodes)}")

# Export summary
mapper.export_to_json(graph, Path("lapack_analysis.json"))
print("\nExported to lapack_analysis.json")
EOF
    "$LAPACK_SRC"
else
    echo "LAPACK source not found at $LAPACK_SRC"
fi
```

## 6. CLI Tool Test

### 6.1 Test CLI Entry Point
```bash
# Check if CLI is available
fortran-mapper --help

# Test global debug flag
fortran-mapper --debug parse --help
```

**Expected Result**: Help text should show all available commands and options.

### 6.2 Test Hook Management
```bash
# List available hooks
fortran-mapper list-hooks

# List only installed hooks
fortran-mapper list-hooks --installed
```

**Expected Result**: 
- Should show lapack hooks
- Should indicate installation status

### 6.3 Test Neo4j Server Management
```bash
# Check Neo4j status
fortran-mapper neo4j status

# Start Neo4j server (if not running)
fortran-mapper neo4j start

# Check status again
fortran-mapper neo4j status

# Stop server
fortran-mapper neo4j stop
```

**Expected Result**: Server should start/stop successfully.

### 6.4 Test Parsing with CLI
```bash
# Parse with LAPACK hooks using new --hooks flag
fortran-mapper parse BLAS/SRC/ SRC/ --hooks lapack --json output.json

# Parse with direct Neo4j export
fortran-mapper parse BLAS/SRC/ --hooks lapack -o neo4j

# Parse with multiple directories and exclusions
fortran-mapper --debug parse SRC/ --exclude "*test*" "*example*" -o neo4j
```

**Expected Result**: 
- Parsing should complete without errors
- Should show statistics after parsing
- Neo4j export should work

### 6.5 Test Query and Analysis Commands
```bash
# Execute a Cypher query
fortran-mapper query "MATCH (r:Routine) RETURN r.name LIMIT 5"

# Query with different output formats
fortran-mapper query "MATCH (r:Routine) RETURN r.name, r.precision LIMIT 5" --format json
fortran-mapper query "MATCH (r:Routine) RETURN r.name, r.precision LIMIT 5" --format csv

# Analyze routine dependencies
fortran-mapper analyze DGETRF -d 3
fortran-mapper analyze DGESV -d 2
```

**Expected Result**:
- Queries should return results in requested format
- Analysis should show dependency tree

### 6.6 Test Export/Import Commands
```bash
# Export to different formats
fortran-mapper export json test_export.json
fortran-mapper export cypher test_export.cypher
fortran-mapper export stats test_stats.txt

# Import from backup
fortran-mapper import test_export.cypher --clear
```

**Expected Result**: All export formats should work correctly.

### 6.7 Test Explore Commands
```bash
# Show graph summary
fortran-mapper explore summary

# Find hub routines
fortran-mapper explore hubs --limit 10

# Explore specific routine
fortran-mapper explore routine DGEMM --depth 2

# Analyze precision distribution
fortran-mapper explore precision

# Show operation categories
fortran-mapper explore categories
```

**Expected Result**: Each explore command should provide meaningful analysis.

## 7. Export Format Tests

### 7.1 Test All Export Formats
```python
cat > test_all_exports.py << 'EOF'
from fortran_mapper import FortranMapper
from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator
from pathlib import Path
import tempfile
import json

# Create test file
test_code = '''
      SUBROUTINE DGEMM(M, N, K, A, B, C)
      INTEGER M, N, K
      DOUBLE PRECISION A(*), B(*), C(*)
      CALL DGEMV(M, N, A, B, C)
      END
'''

with tempfile.TemporaryDirectory() as tmpdir:
    tmpdir = Path(tmpdir)
    test_file = tmpdir / "test.f"
    test_file.write_text(test_code)
    
    # Parse with hooks
    mapper = FortranMapper()
    mapper.register_hook("node_enricher", LapackNodeEnricher())
    mapper.register_hook("node_creator", LapackNodeCreator())
    
    graph = mapper.parse_file(test_file)
    
    # Test all export formats
    mapper.export_to_json(graph, tmpdir / "test.json")
    mapper.export_to_dot(graph, tmpdir / "test.dot")
    mapper.export_to_graphml(graph, tmpdir / "test.graphml")
    
    # Verify JSON
    with open(tmpdir / "test.json") as f:
        data = json.load(f)
        print(f"JSON export: {len(data['nodes'])} nodes, {len(data['relationships'])} relationships")
    
    # Verify DOT
    dot_content = (tmpdir / "test.dot").read_text()
    print(f"DOT export: {'digraph' in dot_content}")
    
    # Verify GraphML
    graphml_content = (tmpdir / "test.graphml").read_text()
    print(f"GraphML export: {'<graphml' in graphml_content}")
EOF

python test_all_exports.py
```

## Verification Checklist

- [ ] Core fortran-mapper package imports successfully
- [ ] LAPACK hooks package installs and imports correctly
- [ ] Generic Fortran parsing works without hooks
- [ ] LAPACK-specific parsing creates enriched nodes
- [ ] Custom nodes (Precision, Operation, MatrixType) are created
- [ ] All export formats (JSON, DOT, GraphML) work correctly
- [ ] Neo4j Cypher export generates valid statements
- [ ] Hook registration system works properly
- [ ] Fallback regex parser activates when fortran-src unavailable
- [ ] Categories and properties are correctly extracted
- [ ] File and routine relationships are established

## Key Differences from Original Workflow

1. **Modular Architecture**: LAPACK functionality is now in a separate package
2. **Hook Registration**: Use `--hooks lapack` flag instead of `--lapack`
3. **Full CLI Implementation**: All commands from original workflow are now available
4. **Import Paths**: Changed from `fortran_mapper.adapters.lapack` to `fortran_mapper_hooks_lapack`
5. **Installation**: Uses `uv pip install` instead of `pip`
6. **Dynamic Hook Loading**: Can load multiple hooks with `--hooks hook1 hook2`
7. **Hook Discovery**: `list-hooks` command shows available hooks

## New Features

1. **Global --debug flag**: Enable debug logging across all commands
2. **Multiple directory parsing**: Can parse multiple directories in one command
3. **--exclude patterns**: Exclude files matching patterns during parsing
4. **-o neo4j shorthand**: Quick Neo4j export
5. **list-hooks command**: Discover and check hook installation status
6. **Flexible query output**: Support for table, JSON, and CSV formats

## Complete Feature Checklist

- [x] Neo4j server management (start/stop/status/console)
- [x] Parse command with hook support
- [x] Query command with multiple output formats
- [x] Analyze command for dependency analysis
- [x] Export command (json/cypher/graphml/stats)
- [x] Import command for backups
- [x] Explore commands (summary/hubs/routine/precision/categories)
- [x] Dynamic hook loading with --hooks flag
- [x] List available hooks command
- [x] Full feature parity with original lapack-util

The modular architecture is fully implemented with complete CLI functionality!