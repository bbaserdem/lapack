// LAPACK Graph Database Schema with Error Tracking
// This enhanced schema includes nodes for tracking parsing errors and warnings

// ============================================
// STEP 1: Create Uniqueness Constraints
// ============================================

// Original constraints
CREATE CONSTRAINT routine_name_unique IF NOT EXISTS 
FOR (r:Routine) REQUIRE r.name IS UNIQUE;

CREATE CONSTRAINT file_path_unique IF NOT EXISTS 
FOR (f:File) REQUIRE f.path IS UNIQUE;

CREATE CONSTRAINT operation_name_unique IF NOT EXISTS 
FOR (o:Operation) REQUIRE o.name IS UNIQUE;

CREATE CONSTRAINT precision_symbol_unique IF NOT EXISTS 
FOR (p:Precision) REQUIRE p.symbol IS UNIQUE;

// New constraints for error tracking
CREATE CONSTRAINT error_id_unique IF NOT EXISTS
FOR (e:ParseError) REQUIRE e.id IS UNIQUE;

// ============================================
// STEP 2: Create Additional Indexes
// ============================================

// Original indexes
CREATE INDEX routine_precision_index IF NOT EXISTS 
FOR (r:Routine) ON (r.precision);

CREATE INDEX routine_operation_index IF NOT EXISTS 
FOR (r:Routine) ON (r.operation);

CREATE INDEX file_library_index IF NOT EXISTS 
FOR (f:File) ON (f.library);

CREATE INDEX file_directory_index IF NOT EXISTS 
FOR (f:File) ON (f.directory);

// New indexes for error tracking
CREATE INDEX error_severity_index IF NOT EXISTS
FOR (e:ParseError) ON (e.severity);

CREATE INDEX error_type_index IF NOT EXISTS
FOR (e:ParseError) ON (e.error_type);

CREATE INDEX error_timestamp_index IF NOT EXISTS
FOR (e:ParseError) ON (e.timestamp);

CREATE INDEX file_has_errors_index IF NOT EXISTS
FOR (f:File) ON (f.has_errors);

// ============================================
// STEP 3: Create Initial Precision Nodes
// ============================================

MERGE (s:Precision {symbol: 's'})
ON CREATE SET s.name = 'single';

MERGE (d:Precision {symbol: 'd'})
ON CREATE SET d.name = 'double';

MERGE (c:Precision {symbol: 'c'})
ON CREATE SET c.name = 'complex';

MERGE (z:Precision {symbol: 'z'})
ON CREATE SET z.name = 'double complex';

// ============================================
// STEP 4: Schema Documentation
// ============================================

// New Node Type: ParseError
// Represents errors encountered during parsing
// Properties:
// - id: Unique identifier (e.g., "error_<timestamp>_<hash>")
// - error_type: Type of error (e.g., "syntax_error", "file_not_found", "encoding_error")
// - severity: Error severity ("error", "warning", "info")
// - message: Detailed error message
// - line_number: Line number where error occurred (if applicable)
// - column_number: Column number where error occurred (if applicable)
// - context: Code context around the error
// - timestamp: When the error was encountered
// - parser_version: Version of the parser that generated the error

// New Relationship: HAS_ERROR
// Links a file to its parsing errors
// From: File
// To: ParseError
// Properties:
// - position: Order of error in file (1st, 2nd, etc.)

// New Relationship: PARTIAL_PARSE
// Links a file to routines that were partially parsed despite errors
// From: File
// To: Routine
// Properties:
// - confidence: Confidence level of the parse (0.0-1.0)
// - warnings: Array of warning messages

// ============================================
// STEP 5: Example Error Queries
// ============================================

// Find all files with parsing errors:
// MATCH (f:File)-[:HAS_ERROR]->(e:ParseError)
// RETURN f.path, collect(e.message) as errors
// ORDER BY f.path

// Find most common error types:
// MATCH (e:ParseError)
// RETURN e.error_type, count(*) as count
// ORDER BY count DESC

// Find files with syntax errors:
// MATCH (f:File)-[:HAS_ERROR]->(e:ParseError {error_type: 'syntax_error'})
// RETURN f.path, e.line_number, e.message

// Find partially parsed routines:
// MATCH (f:File)-[r:PARTIAL_PARSE]->(routine:Routine)
// WHERE r.confidence < 1.0
// RETURN f.path, routine.name, r.confidence, r.warnings

// Get error statistics by severity:
// MATCH (e:ParseError)
// RETURN e.severity, count(*) as count

// Find files that need manual review:
// MATCH (f:File)
// WHERE f.has_errors = true
// OPTIONAL MATCH (f)-[:HAS_ERROR]->(e:ParseError)
// RETURN f.path, f.parse_status, collect(e.message) as errors
// ORDER BY size(errors) DESC

// ============================================
// STEP 6: Verify Schema Creation
// ============================================

MATCH (p:Precision) 
WITH count(p) as precisionCount
RETURN {
  message: "Enhanced schema with error tracking created successfully",
  precisionNodes: precisionCount,
  features: ["Error tracking", "Partial parse support", "Parse confidence metrics"],
  note: "Run verify_schema_with_errors.cypher to check all constraints and indexes"
} as result;