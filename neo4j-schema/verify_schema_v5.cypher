// LAPACK Schema Verification Script for Neo4j 5.x
// This script verifies that all schema elements are correctly created

// ============================================
// Check Constraints using SHOW CONSTRAINTS
// ============================================
SHOW CONSTRAINTS
YIELD name, type, entityType, labelsOrTypes, properties
WHERE type = 'UNIQUENESS'
WITH collect({
  name: name,
  type: type,
  entity: entityType,
  labels: labelsOrTypes,
  properties: properties
}) as constraints

// ============================================
// Check Indexes using SHOW INDEXES
// ============================================
SHOW INDEXES
YIELD name, state, type, labelsOrTypes, properties
WHERE state = 'ONLINE'
WITH constraints, collect({
  name: name,
  type: type,
  labels: labelsOrTypes,
  properties: properties,
  state: state
}) as indexes

// ============================================
// Check Precision Nodes
// ============================================
MATCH (p:Precision)
WITH constraints, indexes, 
     collect({symbol: p.symbol, name: p.name}) as precisions
ORDER BY p.symbol

// ============================================
// Check for Test Nodes (from any prior runs)
// ============================================
OPTIONAL MATCH (r:Routine)
WITH constraints, indexes, precisions, count(r) as routineCount
OPTIONAL MATCH (f:File)
WITH constraints, indexes, precisions, routineCount, count(f) as fileCount
OPTIONAL MATCH (o:Operation)
WITH constraints, indexes, precisions, routineCount, fileCount, count(o) as operationCount

// ============================================
// Generate Verification Report
// ============================================
RETURN {
  schemaValid: size(precisions) = 4 AND size(constraints) >= 4,
  summary: {
    constraints: size(constraints),
    indexes: size(indexes),
    precisionNodes: size(precisions),
    existingData: {
      routines: routineCount,
      files: fileCount,
      operations: operationCount
    }
  },
  precisions: precisions,
  constraintDetails: constraints,
  indexDetails: indexes,
  status: CASE
    WHEN size(precisions) < 4 THEN 'ERROR: Missing precision nodes'
    WHEN size(constraints) < 4 THEN 'WARNING: Missing some constraints'
    ELSE 'SUCCESS: Schema is properly configured'
  END
} as verification;