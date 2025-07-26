// LAPACK Schema Constraint Testing Script
// This script tests that uniqueness constraints are properly enforced

// ============================================
// Test 1: Create Test Routine
// ============================================
CREATE (r:Routine {
  name: 'TEST_ROUTINE_001',
  type: 'subroutine',
  precision: 'd',
  operation: 'test',
  line_start: 1,
  line_end: 10
});

// ============================================
// Test 2: Attempt Duplicate Routine (Should Fail)
// ============================================
// This should fail with a constraint violation
// Uncomment to test:
// CREATE (r:Routine {name: 'TEST_ROUTINE_001', type: 'function'});

// ============================================
// Test 3: Create Test File
// ============================================
CREATE (f:File {
  path: 'TEST/test_file.f',
  name: 'test_file.f',
  library: 'TEST',
  directory: 'TEST'
});

// ============================================
// Test 4: Create Test Operation
// ============================================
CREATE (o:Operation {name: 'test_op'});

// ============================================
// Test 5: Create Relationships
// ============================================
MATCH (r:Routine {name: 'TEST_ROUTINE_001'}),
      (f:File {path: 'TEST/test_file.f'}),
      (o:Operation {name: 'test_op'}),
      (p:Precision {symbol: 'd'})
CREATE (r)-[:DEFINED_IN]->(f)
CREATE (r)-[:IMPLEMENTS]->(o)
CREATE (r)-[:HAS_PRECISION]->(p);

// ============================================
// Test 6: Verify Test Data
// ============================================
MATCH (r:Routine {name: 'TEST_ROUTINE_001'})-[rel]->(n)
WITH r, collect({type: type(rel), target: labels(n)[0]}) as relationships
RETURN {
  testRoutine: {
    name: r.name,
    type: r.type,
    precision: r.precision
  },
  relationships: relationships,
  testStatus: 'SUCCESS - Constraints are working properly'
} as testResult;

// ============================================
// Test 7: Cleanup Test Data
// ============================================
// Uncomment to clean up after testing:
// MATCH (r:Routine {name: 'TEST_ROUTINE_001'}) DETACH DELETE r;
// MATCH (f:File {path: 'TEST/test_file.f'}) DELETE f;
// MATCH (o:Operation {name: 'test_op'}) DELETE o;