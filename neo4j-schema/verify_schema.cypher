// LAPACK Schema Verification Script
// This script verifies that all schema elements are correctly created

// ============================================
// Check Constraints
// ============================================
CALL db.constraints() YIELD name, description, type
WHERE type CONTAINS 'UNIQUENESS'
WITH collect({name: name, description: description}) as constraints

// ============================================
// Check Indexes
// ============================================
CALL db.indexes() YIELD name, state, type, labelsOrTypes, properties
WHERE state = 'ONLINE'
WITH constraints, collect({
  name: name,
  type: type,
  labels: labelsOrTypes,
  properties: properties
}) as indexes

// ============================================
// Check Node Labels
// ============================================
CALL db.labels() YIELD label
WHERE label IN ['Routine', 'File', 'Operation', 'Precision']
WITH constraints, indexes, collect(label) as labels

// ============================================
// Check Precision Nodes
// ============================================
MATCH (p:Precision)
WITH constraints, indexes, labels, 
     collect({symbol: p.symbol, name: p.name}) as precisions

// ============================================
// Generate Verification Report
// ============================================
RETURN {
  schemaValid: 
    size(labels) = 4 AND 
    size(precisions) = 4 AND
    size(constraints) >= 4,
  details: {
    nodeLabels: {
      expected: ['Routine', 'File', 'Operation', 'Precision'],
      found: labels,
      count: size(labels)
    },
    constraints: {
      expected: 4,
      found: size(constraints),
      list: constraints
    },
    indexes: {
      found: size(indexes),
      list: indexes
    },
    precisionNodes: {
      expected: 4,
      found: size(precisions),
      list: precisions
    }
  },
  recommendations: CASE
    WHEN size(labels) < 4 THEN 'Missing node labels. Run schema creation script.'
    WHEN size(precisions) < 4 THEN 'Missing precision nodes. Run schema creation script.'
    WHEN size(constraints) < 4 THEN 'Missing constraints. Run schema creation script.'
    ELSE 'Schema is correctly configured.'
  END
} as verification;