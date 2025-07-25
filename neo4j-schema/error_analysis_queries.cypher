// Error Analysis Queries for LAPACK Neo4j Database
// These queries help analyze parsing errors and identify problematic files

// ============================================
// 1. BASIC ERROR QUERIES
// ============================================

// Find all files with parsing errors
MATCH (f:File)-[:HAS_ERROR]->(e:ParseError)
RETURN f.path as file, 
       f.library as library,
       collect(e.message) as errors,
       count(e) as error_count
ORDER BY error_count DESC;

// Count errors by type
MATCH (e:ParseError)
RETURN e.error_type as error_type, 
       count(*) as count
ORDER BY count DESC;

// Count errors by severity
MATCH (e:ParseError)
RETURN e.severity as severity, 
       count(*) as count
ORDER BY count DESC;

// Find most recent errors
MATCH (e:ParseError)
RETURN e.timestamp as timestamp,
       datetime({epochSeconds: e.timestamp}) as error_time,
       e.error_type as type,
       e.message as message
ORDER BY e.timestamp DESC
LIMIT 20;

// ============================================
// 2. FILE-LEVEL ERROR ANALYSIS
// ============================================

// Find files that failed to parse completely (no routines extracted)
MATCH (f:File {has_errors: true})
WHERE NOT (f)-[:PARTIAL_PARSE]->(:Routine) 
  AND NOT (f)<-[:DEFINED_IN]-(:Routine)
RETURN f.path as file, 
       f.library as library,
       size((f)-[:HAS_ERROR]->()) as error_count
ORDER BY f.path;

// Find files with partial parses
MATCH (f:File)-[pp:PARTIAL_PARSE]->(r:Routine)
RETURN f.path as file,
       count(r) as routines_found,
       avg(pp.confidence) as avg_confidence,
       collect(DISTINCT pp.warnings) as warnings
ORDER BY avg_confidence ASC;

// Compare error rates by library
MATCH (f:File)
OPTIONAL MATCH (f)-[:HAS_ERROR]->(e:ParseError)
WITH f.library as library, 
     count(DISTINCT f) as total_files,
     count(DISTINCT CASE WHEN f.has_errors THEN f END) as files_with_errors,
     count(e) as total_errors
RETURN library,
       total_files,
       files_with_errors,
       toFloat(files_with_errors) / toFloat(total_files) * 100 as error_rate_percent,
       total_errors
ORDER BY error_rate_percent DESC;

// ============================================
// 3. ERROR PATTERNS
// ============================================

// Find syntax errors with line numbers
MATCH (f:File)-[:HAS_ERROR]->(e:ParseError {error_type: 'syntax_error'})
WHERE e.line_number IS NOT NULL
RETURN f.path as file,
       e.line_number as line,
       e.message as error
ORDER BY f.path, e.line_number;

// Find common error messages
MATCH (e:ParseError)
WITH e.message as message, count(*) as occurrences
WHERE occurrences > 1
RETURN message, occurrences
ORDER BY occurrences DESC
LIMIT 20;

// Find files with multiple error types
MATCH (f:File)-[:HAS_ERROR]->(e:ParseError)
WITH f, collect(DISTINCT e.error_type) as error_types
WHERE size(error_types) > 1
RETURN f.path as file,
       error_types,
       size(error_types) as different_error_types
ORDER BY different_error_types DESC;

// ============================================
// 4. IMPACT ANALYSIS
// ============================================

// Find routines that might be affected by parsing errors
MATCH (f:File {has_errors: true})-[pp:PARTIAL_PARSE]->(r:Routine)
OPTIONAL MATCH (r)-[:CALLS]->(called:Routine)
RETURN r.name as routine,
       pp.confidence as confidence,
       count(called) as calls_count,
       collect(called.name)[..5] as sample_calls
ORDER BY pp.confidence ASC, calls_count DESC;

// Find high-value routines in error-prone files
MATCH (f:File {has_errors: true})-[:PARTIAL_PARSE]->(r:Routine)
MATCH (caller:Routine)-[:CALLS]->(r)
WITH r, f, count(DISTINCT caller) as callers_count
WHERE callers_count > 5
RETURN r.name as important_routine,
       f.path as problematic_file,
       callers_count
ORDER BY callers_count DESC;

// ============================================
// 5. ERROR TIMELINE
// ============================================

// Group errors by date
MATCH (e:ParseError)
WITH date(datetime({epochSeconds: e.timestamp})) as error_date,
     count(*) as daily_errors
RETURN error_date, daily_errors
ORDER BY error_date DESC;

// Find parser version issues
MATCH (e:ParseError)
RETURN e.parser_version as parser_version,
       count(*) as error_count,
       collect(DISTINCT e.error_type) as error_types
ORDER BY error_count DESC;

// ============================================
// 6. RECOVERY SUGGESTIONS
// ============================================

// Find files that need manual review (high error count, low confidence)
MATCH (f:File)-[:HAS_ERROR]->(e:ParseError)
WITH f, count(e) as error_count
MATCH (f)-[pp:PARTIAL_PARSE]->(r:Routine)
WITH f, error_count, avg(pp.confidence) as avg_confidence, count(r) as routine_count
WHERE error_count > 2 OR avg_confidence < 0.7
RETURN f.path as needs_review,
       f.library as library,
       error_count,
       avg_confidence,
       routine_count
ORDER BY error_count DESC, avg_confidence ASC;

// Find potentially fixable syntax errors
MATCH (f:File)-[:HAS_ERROR]->(e:ParseError)
WHERE e.error_type = 'syntax_error' 
  AND e.line_number IS NOT NULL
RETURN f.path as file,
       collect({line: e.line_number, message: e.message}) as syntax_errors
ORDER BY size(syntax_errors) DESC
LIMIT 20;

// ============================================
// 7. EXPORT ERROR REPORT
// ============================================

// Generate comprehensive error report
MATCH (f:File {has_errors: true})
OPTIONAL MATCH (f)-[:HAS_ERROR]->(e:ParseError)
OPTIONAL MATCH (f)-[pp:PARTIAL_PARSE]->(r:Routine)
WITH f, 
     collect(DISTINCT {
       type: e.error_type, 
       severity: e.severity,
       message: e.message,
       line: e.line_number
     }) as errors,
     count(DISTINCT r) as partial_routines,
     avg(pp.confidence) as avg_confidence
RETURN f.path as file_path,
       f.library as library,
       f.parse_status as status,
       size(errors) as error_count,
       errors[..3] as first_three_errors,
       partial_routines,
       avg_confidence
ORDER BY error_count DESC;