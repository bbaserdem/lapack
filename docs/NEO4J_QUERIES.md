# Neo4j Query Examples for LAPACK Analysis

This document provides a collection of useful Cypher queries for analyzing the LAPACK/BLAS codebase in Neo4j.

## Table of Contents

1. [Basic Queries](#basic-queries)
2. [Dependency Analysis](#dependency-analysis)
3. [Precision Analysis](#precision-analysis)
4. [Performance Critical Paths](#performance-critical-paths)
5. [Code Organization](#code-organization)
6. [Advanced Analysis](#advanced-analysis)
7. [Visualization Queries](#visualization-queries)

## Basic Queries

### Count all nodes by type
```cypher
MATCH (n)
RETURN labels(n)[0] as NodeType, count(n) as Count
ORDER BY Count DESC
```

### Find a specific routine
```cypher
MATCH (r:Routine {name: 'DGEMM'})
RETURN r
```

### List all routines in a file
```cypher
MATCH (r:Routine)-[:DEFINED_IN]->(f:File {name: 'dgemm.f'})
RETURN r.name, r.type, r.precision
ORDER BY r.name
```

## Dependency Analysis

### Direct dependencies of a routine
```cypher
MATCH (r:Routine {name: 'DGETRF'})-[:CALLS]->(dep:Routine)
RETURN dep.name, dep.precision
ORDER BY dep.name
```

### All callers of a routine
```cypher
MATCH (caller:Routine)-[:CALLS]->(r:Routine {name: 'DGEMM'})
RETURN caller.name, caller.precision
ORDER BY caller.name
```

### Dependency tree (up to 3 levels)
```cypher
MATCH path = (r:Routine {name: 'DGESV'})-[:CALLS*1..3]->(dep:Routine)
RETURN path
```

### Find unused routines (no callers)
```cypher
MATCH (r:Routine)
WHERE NOT (r)<-[:CALLS]-()
RETURN r.name, r.type
ORDER BY r.name
LIMIT 20
```

### Most frequently called routines
```cypher
MATCH (r:Routine)<-[:CALLS]-(caller)
RETURN r.name, count(caller) as CallCount
ORDER BY CallCount DESC
LIMIT 20
```

### Find mutual dependencies
```cypher
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)-[:CALLS]->(r1)
WHERE r1.name < r2.name
RETURN r1.name, r2.name
```

## Precision Analysis

### All precision variants of an operation
```cypher
MATCH (r:Routine)-[:IMPLEMENTS]->(op:Operation {name: 'gemm'})
MATCH (r)-[:HAS_PRECISION]->(p:Precision)
RETURN r.name, p.symbol, p.name
ORDER BY p.symbol
```

### Count routines by precision
```cypher
MATCH (r:Routine)-[:HAS_PRECISION]->(p:Precision)
RETURN p.symbol, p.name, count(r) as Count
ORDER BY p.symbol
```

### Find operations with all precision variants
```cypher
MATCH (op:Operation)
WITH op
MATCH (r:Routine)-[:IMPLEMENTS]->(op)
MATCH (r)-[:HAS_PRECISION]->(p:Precision)
WITH op, collect(DISTINCT p.symbol) as precisions
WHERE size(precisions) = 4
RETURN op.name, precisions
ORDER BY op.name
```

### Compare call patterns across precisions
```cypher
MATCH (r1:Routine {operation: 'getrf'})-[:CALLS]->(called1:Routine)
WITH r1.precision as prec, collect(DISTINCT called1.operation) as calls
RETURN prec, calls
ORDER BY prec
```

## Performance Critical Paths

### Find longest dependency chains
```cypher
MATCH path = (r:Routine)-[:CALLS*]->(dep:Routine)
WHERE NOT (dep)-[:CALLS]->()
RETURN r.name as Start, dep.name as End, length(path) as Depth
ORDER BY Depth DESC
LIMIT 10
```

### Identify bottleneck routines (high in-degree)
```cypher
MATCH (r:Routine)<-[:CALLS]-(caller)
WITH r, count(caller) as inDegree
WHERE inDegree > 10
MATCH (r)-[:CALLS]->(called)
WITH r, inDegree, count(called) as outDegree
RETURN r.name, inDegree, outDegree, inDegree * outDegree as Impact
ORDER BY Impact DESC
LIMIT 20
```

### Find critical paths through specific routine
```cypher
MATCH p1 = (start:Routine)-[:CALLS*]->(middle:Routine {name: 'DGEMM'})
MATCH p2 = (middle)-[:CALLS*]->(end:Routine)
WHERE NOT (start)<-[:CALLS]-() AND NOT (end)-[:CALLS]->()
RETURN p1, p2
LIMIT 5
```

## Code Organization

### Files with most routines
```cypher
MATCH (r:Routine)-[:DEFINED_IN]->(f:File)
RETURN f.path, count(r) as RoutineCount
ORDER BY RoutineCount DESC
LIMIT 20
```

### Library distribution
```cypher
MATCH (r:Routine)-[:DEFINED_IN]->(f:File)
RETURN f.library, count(DISTINCT r) as Routines, count(DISTINCT f) as Files
ORDER BY Routines DESC
```

### Find files that define multiple operations
```cypher
MATCH (r:Routine)-[:DEFINED_IN]->(f:File)
WITH f, collect(DISTINCT r.operation) as operations
WHERE size(operations) > 1
RETURN f.path, operations, size(operations) as OpCount
ORDER BY OpCount DESC
```

### Cross-library dependencies
```cypher
MATCH (r1:Routine)-[:DEFINED_IN]->(f1:File),
      (r2:Routine)-[:DEFINED_IN]->(f2:File),
      (r1)-[:CALLS]->(r2)
WHERE f1.library <> f2.library
RETURN f1.library as Source, f2.library as Target, count(*) as Dependencies
```

## Advanced Analysis

### Circular dependencies
```cypher
MATCH path = (r:Routine)-[:CALLS*2..]->(r)
RETURN path
LIMIT 10
```

### Strongly connected components
```cypher
CALL gds.graph.project(
  'callGraph',
  'Routine',
  'CALLS'
)
YIELD graphName;

CALL gds.scc.stream('callGraph')
YIELD nodeId, componentId
WITH componentId, collect(gds.util.asNode(nodeId).name) as routines
WHERE size(routines) > 1
RETURN componentId, routines, size(routines) as Size
ORDER BY Size DESC
LIMIT 10
```

### PageRank for importance
```cypher
CALL gds.graph.project(
  'callGraph',
  'Routine',
  'CALLS'
)
YIELD graphName;

CALL gds.pageRank.stream('callGraph')
YIELD nodeId, score
RETURN gds.util.asNode(nodeId).name AS routine, score
ORDER BY score DESC
LIMIT 20
```

### Community detection
```cypher
CALL gds.graph.project(
  'callGraph',
  'Routine',
  'CALLS'
)
YIELD graphName;

CALL gds.louvain.stream('callGraph')
YIELD nodeId, communityId
WITH communityId, collect(gds.util.asNode(nodeId).name) as routines
WHERE size(routines) > 5
RETURN communityId, routines[0..10] as SampleRoutines, size(routines) as Size
ORDER BY Size DESC
```

## Visualization Queries

### Subgraph around a routine (1 level)
```cypher
MATCH (center:Routine {name: 'DGETRF'})
MATCH (center)-[:CALLS]-(connected:Routine)
RETURN center, connected
```

### Call hierarchy for an operation
```cypher
MATCH (r:Routine {operation: 'gesv', precision: 'd'})
MATCH path = (r)-[:CALLS*0..2]->(dep:Routine)
RETURN path
```

### Precision variant comparison
```cypher
MATCH (op:Operation {name: 'gemm'})<-[:IMPLEMENTS]-(r:Routine)
MATCH (r)-[:HAS_PRECISION]->(p:Precision)
MATCH (r)-[:CALLS]->(called:Routine)
RETURN r, p, called
```

### File dependency graph
```cypher
MATCH (f1:File)<-[:DEFINED_IN]-(r1:Routine)-[:CALLS]->(r2:Routine)-[:DEFINED_IN]->(f2:File)
WHERE f1 <> f2
WITH f1, f2, count(*) as weight
RETURN f1.name, f2.name, weight
ORDER BY weight DESC
LIMIT 50
```

## Query Performance Tips

1. **Use indexes**: Ensure indexes are created on frequently queried properties
```cypher
CREATE INDEX ON :Routine(name);
CREATE INDEX ON :File(path);
CREATE INDEX ON :Operation(name);
```

2. **Limit path length**: Always specify maximum path length in variable-length patterns
```cypher
// Good
MATCH path = (r:Routine)-[:CALLS*1..5]->(dep:Routine)

// Bad - can be very slow
MATCH path = (r:Routine)-[:CALLS*]->(dep:Routine)
```

3. **Use LIMIT**: Always limit results when exploring
```cypher
MATCH (r:Routine)
RETURN r
LIMIT 100
```

4. **Profile queries**: Use EXPLAIN or PROFILE to understand query performance
```cypher
PROFILE
MATCH (r:Routine {name: 'DGEMM'})-[:CALLS*1..3]->(dep:Routine)
RETURN count(dep)
```

## Creating Custom Views

### Create a view of BLAS Level 3 routines
```cypher
MATCH (r:Routine)
WHERE r.name =~ '.*GEMM|.*SYMM|.*HEMM|.*SYRK|.*HERK|.*SYR2K|.*HER2K|.*TRMM|.*TRSM'
SET r:BLASLevel3
RETURN count(r)
```

### Tag frequently called routines
```cypher
MATCH (r:Routine)<-[:CALLS]-(caller)
WITH r, count(caller) as callCount
WHERE callCount > 20
SET r:FrequentlyCalled
RETURN r.name, callCount
```

## Data Export Queries

### Export call relationships to CSV
```cypher
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
RETURN r1.name as Caller, r2.name as Callee
ORDER BY Caller, Callee
```

### Export routine metadata
```cypher
MATCH (r:Routine)-[:DEFINED_IN]->(f:File)
OPTIONAL MATCH (r)-[:HAS_PRECISION]->(p:Precision)
RETURN r.name, r.type, p.symbol as precision, r.operation, f.path
ORDER BY r.name
```