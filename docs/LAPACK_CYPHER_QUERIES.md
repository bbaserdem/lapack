# Useful Cypher Queries for LAPACK Computational Graph

These queries are designed to help you explore the LAPACK computational graph in Neo4j Browser.

## Node Properties

### Routine Nodes
- `name`: The routine name (e.g., 'DGEMM', 'XERBLA')
- `type`: BLAS/LAPACK classification ('blas1', 'blas2', 'blas3', 'lapack', 'unknown')
- `precision`: Precision type ('single', 'double', 'complex', 'double_complex', or null)
- `category`: Functional category (e.g., 'general_factorization', 'symmetric_solve', 'utility', 'other')
- `argument_count`: Number of arguments the routine accepts
- `line_number`: Line number in source file where routine is defined

### File Nodes
- `path`: Full file path
- `name`: File name only

## 1. Graph Overview - Sample of Connections

```cypher
// Graph overview - sample of connections
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WITH r1, r2, rand() as random
ORDER BY random
LIMIT 100
RETURN r1, r2
```

This query shows a random sample of 100 call relationships to get a quick overview of the graph structure.

## 2. Most Connected Routines (Hubs)

```cypher
// Most connected routines
MATCH (r:Routine)
WITH r, COUNT{(r)-[:CALLS]-()} as degree
ORDER BY degree DESC
LIMIT 25
MATCH (r)-[rel:CALLS]-(connected)
RETURN r, rel, connected
```

This query finds the 25 most connected routines and shows their connections. These are the central hubs of LAPACK.

## 3. Precision Network

```cypher
// Network showing how different precisions interact
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WHERE r1.precision IN ['double', 'single'] AND r2.precision IN ['double', 'single']
WITH r1, r2, rand() as random
ORDER BY random
LIMIT 100
RETURN r1, r2
```

This query visualizes how single and double precision routines interact with each other.

## 4. File Clusters

```cypher
// Routines clustered by file
MATCH (f:File)<-[:DEFINED_IN]-(r:Routine)
WITH f, collect(r) as routines
WHERE size(routines) > 5
LIMIT 5
UNWIND routines as r
OPTIONAL MATCH (r)-[c:CALLS]-(other:Routine)
WHERE other IN routines
RETURN f, r, c, other
```

This query shows routines grouped by their source files, focusing on files with more than 5 routines.

## 5. Call Chains of Depth 3

```cypher
// Call chains of depth 3 for general matrix routines
MATCH path = (r1:Routine)-[:CALLS*3]->(r3:Routine)
WHERE r1.category STARTS WITH 'general'
WITH path LIMIT 20
RETURN path
```

This query traces call chains of depth 3 starting from general matrix routines.

## 6. Isolated Routines

```cypher
// Find isolated routines (no calls in or out)
MATCH (r:Routine)
WHERE NOT (r)-[:CALLS]-()
RETURN r.name, r.precision, r.category
LIMIT 50
```

This query finds routines that have no incoming or outgoing calls.

## Additional Useful Queries

### Find Circular Dependencies

```cypher
// Find potential circular dependencies (cycles)
MATCH path = (r:Routine)-[:CALLS*2..5]->(r)
RETURN path
LIMIT 20
```

### Analyze Specific Routine

```cypher
// Analyze a specific routine (replace 'XERBLA' with your routine name)
MATCH (r:Routine {name: 'XERBLA'})
OPTIONAL MATCH (r)-[:DEFINED_IN]->(f:File)
OPTIONAL MATCH (r)-[:CALLS]->(called:Routine)
OPTIONAL MATCH (caller:Routine)-[:CALLS]->(r)
RETURN r, f, 
       collect(DISTINCT called) as calls_to,
       collect(DISTINCT caller) as called_by
```

### Find Common Dependencies

```cypher
// Find routines that are called by many others
MATCH (r:Routine)<-[:CALLS]-(caller:Routine)
WITH r, count(caller) as caller_count
WHERE caller_count > 10
RETURN r.name as routine, r.precision, r.category, caller_count
ORDER BY caller_count DESC
```

### File Coupling Analysis

```cypher
// Analyze cross-file dependencies
MATCH (f1:File)<-[:DEFINED_IN]-(r1:Routine)-[:CALLS]->(r2:Routine)-[:DEFINED_IN]->(f2:File)
WHERE f1 <> f2
WITH f1.path as file1, f2.path as file2, count(*) as cross_calls
RETURN file1, file2, cross_calls
ORDER BY cross_calls DESC
LIMIT 20
```

### Find Entry Points

```cypher
// Find potential entry points (routines with no incoming calls)
MATCH (r:Routine)
WHERE NOT (r)<-[:CALLS]-()
AND (r)-[:CALLS]->()
RETURN r.name as entry_point, r.precision, r.category, 
       COUNT{(r)-[:CALLS]->()} as calls_count
ORDER BY calls_count DESC
```

### Analyze Call Depth

```cypher
// Find the longest call chains
MATCH path = (start:Routine)-[:CALLS*]->(end:Routine)
WHERE NOT (start)<-[:CALLS]-() 
  AND NOT (end)-[:CALLS]->()
WITH path, length(path) as depth
ORDER BY depth DESC
LIMIT 10
RETURN [n in nodes(path) | n.name] as call_chain, depth
```

### Precision Mixing Analysis

```cypher
// Find where different precisions mix
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WHERE r1.precision <> r2.precision
  AND r1.precision IS NOT NULL 
  AND r2.precision IS NOT NULL
RETURN r1.name as caller, r1.precision as caller_precision,
       r2.name as callee, r2.precision as callee_precision
LIMIT 50
```

### Routines by Category

```cypher
// Count routines by category
MATCH (r:Routine)
WHERE r.category IS NOT NULL
RETURN r.category as category, count(*) as count
ORDER BY count DESC
```

### Find Factorization Routines

```cypher
// Find all factorization routines
MATCH (r:Routine)
WHERE r.category CONTAINS 'factorization'
RETURN r.name, r.precision, r.category
ORDER BY r.name
```

### BLAS vs LAPACK Distribution

```cypher
// Count BLAS vs LAPACK routines
MATCH (r:Routine)
RETURN r.type as type, count(*) as count
ORDER BY count DESC
```

### Complex Matrix Operations

```cypher
// Find complex/double_complex matrix operations
MATCH (r:Routine)
WHERE r.precision IN ['complex', 'double_complex']
  AND r.category <> 'other' AND r.category <> 'utility'
RETURN r.name, r.precision, r.category
ORDER BY r.category, r.name
LIMIT 50
```

### High-Argument Routines

```cypher
// Find routines with many arguments (potentially complex interfaces)
MATCH (r:Routine)
WHERE r.argument_count > 15
RETURN r.name, r.argument_count, r.category, r.precision
ORDER BY r.argument_count DESC
LIMIT 20
```

### Category Interaction Matrix

```cypher
// How different categories of routines call each other
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WHERE r1.category IS NOT NULL AND r2.category IS NOT NULL
WITH r1.category as from_category, r2.category as to_category, count(*) as calls
RETURN from_category, to_category, calls
ORDER BY calls DESC
LIMIT 30
```

### Eigenvalue Solver Dependencies

```cypher
// Find all eigenvalue-related routines and their dependencies
MATCH (r:Routine)
WHERE r.category CONTAINS 'eigenvalue' OR r.name CONTAINS 'EV'
OPTIONAL MATCH (r)-[:CALLS]->(dep:Routine)
RETURN r.name, r.category, collect(dep.name) as dependencies
ORDER BY size(collect(dep.name)) DESC
```

### Utility Function Usage

```cypher
// Which routines use utility functions most
MATCH (r:Routine)-[:CALLS]->(u:Routine {category: 'utility'})
WITH r, count(u) as utility_calls
RETURN r.name, r.category, utility_calls
ORDER BY utility_calls DESC
LIMIT 20
```

## Tips for Using These Queries

1. **In Neo4j Browser**: Copy and paste these queries directly into the query editor
2. **Modify LIMIT values**: Adjust the LIMIT clauses to see more or fewer results
3. **Filter by properties**: Use the new precision and category properties for targeted analysis
4. **Combine conditions**: Mix precision, category, and name patterns for specific searches
5. **Visualize results**: Neo4j Browser will automatically visualize nodes and relationships
6. **Export results**: You can export query results as CSV or JSON from Neo4j Browser

## Understanding LAPACK Properties

### Precision Values
- `single`: Single precision real (REAL*4)
- `double`: Double precision real (REAL*8)
- `complex`: Single precision complex (COMPLEX*8)
- `double_complex`: Double precision complex (COMPLEX*16)

### Category Examples
- `general_factorization`: LU factorization for general matrices
- `symmetric_solve`: Solvers for symmetric systems
- `positive_definite_factorization`: Cholesky factorization
- `orthogonal`: Operations with orthogonal matrices
- `eigenvalue`: Eigenvalue and eigenvector computations
- `blas1`, `blas2`, `blas3`: BLAS level 1, 2, 3 operations
- `utility`: Helper functions like XERBLA
- `vector_operation`: Vector operations (COPY, SWAP, SCAL)
- `matrix_multiplication`: Matrix-matrix and matrix-vector products

### Type Classification
- `blas1`: Vector operations (DOT, AXPY, NRM2, etc.)
- `blas2`: Matrix-vector operations (GEMV, TRMV, etc.)
- `blas3`: Matrix-matrix operations (GEMM, TRSM, etc.)
- `lapack`: LAPACK routines (factorizations, solvers, etc.)
- `unknown`: Routines not matching known patterns

## Performance Notes

- For large graphs, start with smaller LIMIT values
- Use indexes on frequently queried properties (already created for name and path)
- The COUNT{} syntax is more efficient than size() for counting patterns in newer Neo4j versions
- Consider using WHERE clauses early in the query to reduce the search space