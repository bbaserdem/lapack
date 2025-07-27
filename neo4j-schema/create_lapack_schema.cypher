// LAPACK Graph Database Schema Creation Script
// This script creates the initial database schema for LAPACK dependencies

// ============================================
// STEP 1: Create Uniqueness Constraints
// ============================================
// These constraints ensure data integrity and automatically create indexes

// Constraint for Routine nodes
CREATE CONSTRAINT routine_name_unique IF NOT EXISTS 
FOR (r:Routine) REQUIRE r.name IS UNIQUE;

// Constraint for File nodes
CREATE CONSTRAINT file_path_unique IF NOT EXISTS 
FOR (f:File) REQUIRE f.path IS UNIQUE;

// Constraint for Operation nodes
CREATE CONSTRAINT operation_name_unique IF NOT EXISTS 
FOR (o:Operation) REQUIRE o.name IS UNIQUE;

// Constraint for Precision nodes
CREATE CONSTRAINT precision_symbol_unique IF NOT EXISTS 
FOR (p:Precision) REQUIRE p.symbol IS UNIQUE;

// ============================================
// STEP 2: Create Additional Indexes
// ============================================
// These indexes improve query performance for common access patterns

// Index for routine precision queries
CREATE INDEX routine_precision_index IF NOT EXISTS 
FOR (r:Routine) ON (r.precision);

// Index for routine operation queries
CREATE INDEX routine_operation_index IF NOT EXISTS 
FOR (r:Routine) ON (r.operation);

// Index for file library queries
CREATE INDEX file_library_index IF NOT EXISTS 
FOR (f:File) ON (f.library);

// Index for file directory queries
CREATE INDEX file_directory_index IF NOT EXISTS 
FOR (f:File) ON (f.directory);

// ============================================
// STEP 3: Create Initial Precision Nodes
// ============================================
// Pre-populate the four standard LAPACK precision types

MERGE (s:Precision {symbol: 's'})
ON CREATE SET s.name = 'single';

MERGE (d:Precision {symbol: 'd'})
ON CREATE SET d.name = 'double';

MERGE (c:Precision {symbol: 'c'})
ON CREATE SET c.name = 'complex';

MERGE (z:Precision {symbol: 'z'})
ON CREATE SET z.name = 'double complex';

// ============================================
// STEP 4: Verify Schema Creation
// ============================================
// Return counts to verify successful creation

MATCH (p:Precision) 
WITH count(p) as precisionCount
RETURN {
  message: "Schema created successfully",
  precisionNodes: precisionCount,
  note: "Run verify_schema.cypher separately to check constraints and indexes"
} as result;