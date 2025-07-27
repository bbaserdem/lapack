// LAPACK Neo4j Schema Summary
// Shows the current state of the database schema

// Count all constraints
SHOW CONSTRAINTS
YIELD type
WHERE type = 'UNIQUENESS'
WITH count(*) as constraintCount

// Count all indexes (excluding constraint-backing indexes)
CALL {
  SHOW INDEXES
  YIELD name, owningConstraint
  WHERE owningConstraint IS NULL
  RETURN count(*) as customIndexCount
}

// Count precision nodes
CALL {
  MATCH (p:Precision)
  RETURN count(p) as precisionCount, collect(p.symbol + ' (' + p.name + ')') as precisionList
}

// Count data nodes
CALL {
  MATCH (r:Routine)
  RETURN count(r) as routineCount
}

CALL {
  MATCH (f:File)
  RETURN count(f) as fileCount
}

CALL {
  MATCH (o:Operation)
  RETURN count(o) as operationCount
}

// Generate summary report
RETURN {
  timestamp: datetime(),
  schema: {
    constraints: constraintCount,
    customIndexes: customIndexCount,
    totalIndexes: constraintCount + customIndexCount
  },
  nodes: {
    precisions: {
      count: precisionCount,
      values: precisionList
    },
    data: {
      routines: routineCount,
      files: fileCount,
      operations: operationCount
    }
  },
  status: CASE
    WHEN constraintCount >= 4 AND precisionCount = 4 
    THEN '✅ Schema is fully configured and ready for data import'
    ELSE '⚠️ Schema may be incomplete'
  END
} as summary;