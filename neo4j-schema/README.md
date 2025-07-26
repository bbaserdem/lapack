# Neo4j Schema Creation for LAPACK Dependencies

This directory contains Cypher scripts to create and manage the Neo4j database schema for the LAPACK dependency graph.

## Prerequisites

- Neo4j server running (version 4.x or 5.x)
- Access credentials (default: neo4j/neo4j, should be changed on first login)
- Cypher-shell command line tool

## Files

1. **create_lapack_schema.cypher** - Main schema creation script
   - Creates uniqueness constraints
   - Creates performance indexes
   - Initializes precision nodes

2. **verify_schema.cypher** - Schema verification script
   - Checks all constraints are created
   - Verifies indexes are online
   - Confirms precision nodes exist

3. **test_constraints.cypher** - Constraint testing script
   - Creates test data
   - Verifies constraints work properly
   - Includes cleanup commands

## Usage

### 1. Create the Schema

Using cypher-shell (recommended):
```bash
cypher-shell -u neo4j -p <your-password> < create_lapack_schema.cypher
```

Or through Neo4j Browser:
1. Open http://localhost:7474
2. Copy contents of `create_lapack_schema.cypher`
3. Paste and execute in the query window

### 2. Verify the Schema

```bash
cypher-shell -u neo4j -p <your-password> < verify_schema.cypher
```

Expected output should show:
- 4 node labels created
- 4 uniqueness constraints
- 4+ indexes (constraints create implicit indexes)
- 4 precision nodes

### 3. Test the Constraints (Optional)

```bash
cypher-shell -u neo4j -p <your-password> < test_constraints.cypher
```

This creates test data to ensure constraints are working. Remember to run the cleanup commands at the end of the script.

## Schema Overview

### Node Types
- **Routine**: Fortran subroutines/functions
- **File**: Source files
- **Operation**: Mathematical operations (gemm, getrf, etc.)
- **Precision**: Floating-point precision types (s, d, c, z)

### Relationships
- **CALLS**: Routine → Routine
- **DEFINED_IN**: Routine → File
- **IMPLEMENTS**: Routine → Operation
- **HAS_PRECISION**: Routine → Precision

### Constraints
- Routine.name is unique
- File.path is unique
- Operation.name is unique
- Precision.symbol is unique

## Next Steps

After schema creation:
1. Run the LAPACK parser to analyze source files
2. Use the Neo4j export functionality to populate the database
3. Query the graph using examples from [NEO4J_QUERIES.md](../docs/NEO4J_QUERIES.md)

## Troubleshooting

### Connection Issues
```bash
# Check Neo4j is running
neo4j status

# Test connection
cypher-shell -u neo4j -p <password> "RETURN 1;"
```

### Schema Already Exists
The scripts use `IF NOT EXISTS` clauses, so they're safe to run multiple times.

### Cleanup
To completely reset the schema:
```cypher
// Remove all nodes and relationships
MATCH (n) DETACH DELETE n;

// Drop all constraints
CALL db.constraints() YIELD name
CALL db.constraint.drop(name) YIELD name as dropped
RETURN dropped;

// Drop all indexes
CALL db.indexes() YIELD name
WHERE NOT name STARTS WITH 'constraint'
CALL db.index.drop(name) YIELD name as dropped
RETURN dropped;
```

## Performance Considerations

The schema is optimized for common LAPACK analysis queries:
- Finding routine dependencies
- Analyzing precision variants
- Exploring call graphs
- File organization queries

Additional indexes can be added based on specific query patterns.