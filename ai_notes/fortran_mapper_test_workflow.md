# Fortran-Mapper Refactoring Test Workflow

This document provides a comprehensive testing workflow to verify that the refactoring from `lapack-util` to `fortran-mapper` maintains all existing functionality.

## Prerequisites

1. Ensure the development environment is set up:
   ```bash
   # Enter the nix development shell
   direnv allow
   # or manually: nix develop
   ```

2. Verify the package installation:
   ```bash
   # Check if fortran-mapper command is available
   which fortran-mapper
   fortran-mapper --help
   ```

## 1. Package Installation and CLI Verification

### 1.1 Basic CLI Function Test
```bash
# Test basic command help
fortran-mapper --help

# Test subcommand help
fortran-mapper neo4j --help
fortran-mapper parse --help
fortran-mapper analyze --help
fortran-mapper export --help
```

**Expected Result**: All help commands should work without errors and show the fortran-mapper branding.

### 1.2 Version and Import Test
```bash
# Test Python import
python -c "import fortran_mapper; print(fortran_mapper.__version__)"

# Test module accessibility
python -c "from fortran_mapper import LapackParser, Neo4jClient; print('Import successful')"
```

**Expected Result**: No import errors, version should display correctly.

## 2. Neo4j Server Management

### 2.1 Neo4j Server Lifecycle
```bash
# Check if Neo4j server can be managed
fortran-mapper neo4j status

# Start Neo4j server (if not running)
fortran-mapper neo4j start

# Verify server is running
fortran-mapper neo4j status

# Stop server for clean state
fortran-mapper neo4j stop
```

**Expected Result**: 
- Status commands should report server state correctly
- Start/stop commands should work without errors
- Server should start on expected ports (7474, 7687)

### 2.2 Neo4j Configuration Verification
```bash
# Check if data directory is created properly
ls -la neo4j-data/

# Verify configuration file exists
cat neo4j-data/neo4j.conf | head -20
```

**Expected Result**:
- `neo4j-data/` directory should exist
- Configuration files should be properly set up

## 3. LAPACK Source Code Parsing

### 3.1 Basic Parsing Test
```bash
# Start Neo4j server
fortran-mapper neo4j start

# Parse a small subset of LAPACK files (NOTE: Use directories, not individual files)
fortran-mapper --debug parse BLAS/SRC/ SRC/

# Check parsing succeeded
fortran-mapper stats
```

**Expected Result**:
- Parsing should complete without errors
- Should find ~2,263 subroutines and ~9,385 call relationships
- Stats should show parsed routines, files, and relationships

### 3.3 Neo4j Export Test
```bash
# Export parsed data to Neo4j (this takes time for large datasets)
# For faster testing, parse smaller subset first:
fortran-mapper --debug parse BLAS/SRC/ -o neo4j

# Or export after parsing to memory:
fortran-mapper --debug parse BLAS/SRC/ SRC/
fortran-mapper query "MATCH (n) DETACH DELETE n"  # Clear existing data
fortran-mapper --debug parse BLAS/SRC/ SRC/ -o neo4j  # Re-export to Neo4j
```

**Expected Result**:
- Neo4j export should complete (may take several minutes for full dataset)
- Database should contain all parsed routines with proper properties

### 3.2 Batch Parsing Test
```bash
# Parse multiple files with exclusions (NOTE: --debug comes before subcommand)
fortran-mapper --debug parse SRC/ --exclude "*test*" "*example*"

# Verify parsing results
fortran-mapper stats
```

**Expected Result**:
- Should parse multiple files successfully
- Should respect exclusion patterns
- Database should contain multiple routines

## 4. Node Types and Content Verification

### 4.1 Routine Node Verification
```bash
# Query routine nodes (includes line_number property)
fortran-mapper query "MATCH (r:Routine) RETURN r.name, r.precision, r.category, r.file_path, r.line_number LIMIT 10"

# Check specific routine details
fortran-mapper query "MATCH (r:Routine {name: 'DGEMM'}) RETURN r"
```

**Expected Node Properties**:
- `name`: Routine name (e.g., "DGEMM")
- `precision`: Data precision (e.g., "double")
- `category`: Operation category (e.g., "matrix_multiplication")
- `file_path`: Source file location
- `line_number`: Line in source file

### 4.2 Precision Information Verification
```bash
# Query precision values (stored as properties, not separate nodes)
fortran-mapper query "MATCH (r:Routine) RETURN DISTINCT r.precision ORDER BY r.precision"

# Count routines by precision
fortran-mapper query "MATCH (r:Routine) RETURN r.precision, count(r) as count ORDER BY count DESC"
```

**Expected Precision Values**:
- `single` (S prefix routines)
- `double` (D prefix routines) 
- `complex` (C prefix routines)
- `double_complex` (Z prefix routines)
- `null` (utility routines without precision)

### 4.3 Operation Category Verification
```bash
# Query operation categories (stored as properties, not separate nodes)
fortran-mapper query "MATCH (r:Routine) RETURN DISTINCT r.category ORDER BY r.category"

# Count routines by category
fortran-mapper query "MATCH (r:Routine) RETURN r.category, count(r) as count ORDER BY count DESC LIMIT 20"
```

**Expected Operation Categories**:
- `matrix_multiplication` (GEMM, GEMV routines)
- `vector_operation` (COPY, SWAP, SCAL)
- `general_factorization` (GETRF)
- `general_solve` (GETRS)
- `utility` (XERBLA, LSAME)

### 4.4 File Node Verification
```bash
# Query file nodes
fortran-mapper query "MATCH (f:File) RETURN f.path, f.name LIMIT 10"

# Check file relationships
fortran-mapper query "MATCH (r:Routine)-[:DEFINED_IN]->(f:File) RETURN r.name, f.name LIMIT 10"
```

**Expected File Properties**:
- `path`: Full file path (e.g., "SRC/clalsa.f")
- `name`: File name (e.g., "clalsa.f")
- Should have DEFINED_IN relationships from routines

**Verification Commands**:
```bash
# Count total file relationships (should match routine count)
fortran-mapper query "MATCH (r:Routine)-[:DEFINED_IN]->(f:File) RETURN count(*) as total_relationships"

# Show files with most routines
fortran-mapper query "MATCH (f:File)<-[:DEFINED_IN]-(r:Routine) RETURN f.name, count(r) as routine_count ORDER BY routine_count DESC LIMIT 10"
```

## 5. Relationship Verification

### 5.1 Call Relationships
```bash
# Query call relationships
fortran-mapper query "MATCH (r1:Routine)-[:CALLS]->(r2:Routine) RETURN r1.name, r2.name LIMIT 10"

# Check call depth
fortran-mapper query "MATCH p=(r1:Routine)-[:CALLS*1..3]->(r2:Routine) WHERE r1.name='DGESV' RETURN p LIMIT 5"
```

**Expected Result**:
- Should show calling relationships between routines
- Call chains should be traversable

### 5.2 Dependency Analysis
```bash
# Analyze specific routine dependencies
fortran-mapper analyze DGETRF -d 3

# Analyze another routine
fortran-mapper analyze DGESV -d 2
```

**Expected Result**:
- Should show dependency tree
- Should include precision and operation information
- Should show call depth levels

## 6. Data Export/Import Functionality

### 6.1 Export Tests
```bash
# Export to different formats
fortran-mapper export json test_export.json
fortran-mapper export cypher test_export.cypher
fortran-mapper export graphml test_export.graphml
fortran-mapper export stats test_stats.txt

# Verify export files
ls -la test_export.*
head -20 test_export.json
head -20 test_export.cypher
```

**Expected Result**:
- All export formats should be created successfully
- Files should contain valid data in respective formats
- JSON should be valid JSON
- Cypher should contain CREATE/MERGE statements

### 6.2 Import Tests
```bash
# Clear database and import from backup
fortran-mapper import test_export.cypher --clear

# Verify import worked
fortran-mapper stats
fortran-mapper query "MATCH (r:Routine) RETURN count(r)"
```

**Expected Result**:
- Import should restore data correctly
- Statistics should match pre-export state

## 7. Advanced Query and Analysis

### 7.1 Complex Query Tests
```bash
# Test precision pattern analysis (using properties)
fortran-mapper query "
MATCH (r:Routine)
WHERE r.precision IS NOT NULL
RETURN r.precision, count(r) as routine_count
ORDER BY routine_count DESC
"

# Test operation category analysis (using properties)
fortran-mapper query "
MATCH (r:Routine)
WHERE r.category IS NOT NULL
RETURN r.category, count(r) as routine_count
ORDER BY routine_count DESC
"

# Test file coupling analysis
fortran-mapper query "
MATCH (f1:File)<-[:DEFINED_IN]-(r1:Routine)-[:CALLS]->(r2:Routine)-[:DEFINED_IN]->(f2:File)
WHERE f1 <> f2
RETURN f1.name, f2.name, count(*) as coupling_strength
ORDER BY coupling_strength DESC
LIMIT 10
"
```

**Expected Result**:
- Queries should execute without errors
- Should return meaningful analytical data
- Results should be consistent with LAPACK structure

### 7.2 Graph Exploration Tests
```bash
# Test graph exploration commands (if implemented)
fortran-mapper explore summary
fortran-mapper explore hubs --limit 10
fortran-mapper explore routine DGEMM --depth 2
fortran-mapper explore precision
fortran-mapper explore categories
```

**Expected Result**:
- Commands should provide useful analysis
- Should show top connected routines
- Should provide precision and category breakdowns

## 8. Error Handling and Edge Cases

### 8.1 Invalid Input Handling
```bash
# Test with non-existent directories/files
fortran-mapper parse nonexistent_directory/

# Test with invalid Neo4j connection
fortran-mapper --neo4j-uri bolt://invalid:7687 stats

# Test invalid queries
fortran-mapper query "INVALID CYPHER SYNTAX"
```

**Expected Result**:
- Should provide clear error messages
- Should not crash or corrupt data
- Should suggest corrective actions

### 8.2 Large Dataset Handling
```bash
# Parse larger subset of LAPACK (NOTE: Global --debug flag placement)
fortran-mapper --debug parse SRC/ --exclude "*test*"

# Test performance with large queries
fortran-mapper query "MATCH (r:Routine) RETURN r LIMIT 1000"

# Test export of large dataset
fortran-mapper export json large_export.json
```

**Expected Result**:
- Should handle larger datasets gracefully
- Memory usage should be reasonable
- Export should complete successfully

## 9. Backwards Compatibility Verification

### 9.1 Data Format Compatibility
```bash
# If you have old lapack-util exports, test importing them
# fortran-mapper import old_lapack_util_export.cypher

# Verify old query patterns still work
fortran-mapper query "MATCH (r:Routine) WHERE r.name CONTAINS 'GEMM' RETURN r.name"
```

### 9.2 Configuration Compatibility
```bash
# Test with old Neo4j data directory structure
# Check if existing neo4j-data works with new version
fortran-mapper neo4j status
```

## 10. Performance and Resource Usage

### 10.1 Memory Usage Monitoring
```bash
# Monitor memory during parsing
top -p $(pgrep -f fortran-mapper)

# Or use htop for better visualization
htop -p $(pgrep -f fortran-mapper)
```

### 10.2 Database Performance
```bash
# Test query performance
time fortran-mapper query "MATCH (r:Routine)-[:CALLS*1..3]->(target) RETURN count(*)"

# Test large export performance
time fortran-mapper export json performance_test.json
```

## Verification Checklist

- [ ] CLI commands work with new `fortran-mapper` name
- [ ] All subcommands are accessible and functional
- [ ] Python imports work correctly
- [ ] Neo4j server management functions properly
- [ ] LAPACK source parsing works without errors
- [ ] All node types are created with correct properties
- [ ] Relationships between nodes are established correctly
- [ ] Precision information is extracted and stored
- [ ] Operation categories are assigned correctly
- [ ] File relationships are maintained
- [ ] Call relationships are detected and stored
- [ ] Export functionality works for all formats
- [ ] Import functionality restores data correctly
- [ ] Complex queries execute successfully
- [ ] Error handling provides clear messages
- [ ] Performance is acceptable for expected data sizes
- [ ] Memory usage is reasonable
- [ ] Backwards compatibility is maintained

## Common Issues and Troubleshooting

### Issue: Command not found
**Solution**: Ensure the package is properly installed and the development environment is activated.

### Issue: Neo4j connection errors
**Solution**: Verify Neo4j server is running and accessible on the specified port.

### Issue: Parsing errors
**Solution**: Check that source files exist and are valid Fortran files.

### Issue: Missing node properties
**Solution**: Verify that the parsing logic correctly extracts LAPACK-specific information.

### Issue: "unrecognized arguments: --debug"
**Solution**: Global flags like `--debug` must come BEFORE the subcommand, not after.
- ❌ Wrong: `fortran-mapper parse SRC/ --debug`
- ✅ Correct: `fortran-mapper --debug parse SRC/`

### Issue: "Parsed 0 subroutines"
**Solution**: Use directory paths, not individual file paths. The parser uses glob patterns to find `*.f` files within directories.
- ❌ Wrong: `fortran-mapper parse file1.f file2.f`
- ✅ Correct: `fortran-mapper parse SRC/ BLAS/SRC/`

### Issue: Import/export failures
**Solution**: Check file permissions and disk space, verify file format validity.

## Success Criteria

The refactoring is successful if:
1. All commands work identically to the previous `lapack-util` version
2. All node types and relationships are preserved
3. Data integrity is maintained across export/import cycles
4. Performance characteristics are similar to the previous version
5. Error handling is robust and user-friendly
6. The codebase maintains the same functionality with the new package structure