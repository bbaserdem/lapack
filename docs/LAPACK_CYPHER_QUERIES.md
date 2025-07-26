# Useful Cypher Queries for LAPACK Computational Graph

These queries are designed to help you explore the LAPACK computational graph in Neo4j Browser.

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
// Network colored by precision
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WHERE r1.precision IN ['d', 's'] AND r2.precision IN ['d', 's']
WITH r1, r2, rand() as random
ORDER BY random
LIMIT 100
RETURN r1, r2
```

This query shows how single ('s') and double ('d') precision routines interact with each other.

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
// Call chains of depth 3
MATCH path = (r1:Routine)-[:CALLS*3]->(r3:Routine)
WHERE r1.name STARTS WITH 'dge'
WITH path LIMIT 20
RETURN path
```

This query traces call chains of depth 3 starting from routines whose names begin with 'dge'.

## 6. Isolated Routines

```cypher
// Find isolated groups
MATCH (r:Routine)
WHERE NOT (r)-[:CALLS]-()
RETURN r
LIMIT 50
```

This query finds routines that have no incoming or outgoing calls (isolated nodes).

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
RETURN r.name as routine, caller_count
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
RETURN r.name as entry_point, COUNT{(r)-[:CALLS]->()} as calls_count
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

### Precision Mixing

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

## Tips for Using These Queries

1. **In Neo4j Browser**: Copy and paste these queries directly into the query editor
2. **Modify LIMIT values**: Adjust the LIMIT clauses to see more or fewer results
3. **Filter by name patterns**: Use `WHERE r.name CONTAINS 'pattern'` to focus on specific routines
4. **Visualize results**: Neo4j Browser will automatically visualize nodes and relationships
5. **Export results**: You can export query results as CSV or JSON from Neo4j Browser

## Performance Notes

- For large graphs, start with smaller LIMIT values
- Use indexes on frequently queried properties (already created for name and path)
- The COUNT{} syntax is more efficient than size() for counting patterns in newer Neo4j versions