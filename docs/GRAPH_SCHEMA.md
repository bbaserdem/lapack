# LAPACK Graph Database Schema

This document describes the graph database schema used to represent the LAPACK/BLAS codebase structure and dependencies.

## Overview

The graph database captures:
- Fortran routines (subroutines and functions)
- Source files and their organization
- Call relationships between routines
- Mathematical operations implemented
- Precision variants of routines

## Node Types

### 1. Routine Node
Represents a Fortran subroutine or function.

**Label:** `Routine`

**Properties:**
- `name` (string): Full routine name (e.g., "DGEMM", "ZGETRF")
- `type` (string): Either "subroutine" or "function"
- `precision` (string): Precision indicator from LAPACK naming convention
  - 's' = single precision real
  - 'd' = double precision real
  - 'c' = single precision complex
  - 'z' = double precision complex
- `operation` (string): Base operation name without precision prefix (e.g., "gemm", "getrf")
- `line_start` (integer): Starting line number in source file
- `line_end` (integer): Ending line number in source file

**Example:**
```cypher
(:Routine {
  name: "DGEMM",
  type: "subroutine",
  precision: "d",
  operation: "gemm",
  line_start: 150,
  line_end: 450
})
```

### 2. File Node
Represents a Fortran source file.

**Label:** `File`

**Properties:**
- `path` (string): Full file path relative to LAPACK root
- `name` (string): File name (e.g., "dgemm.f")
- `library` (string): Which library the file belongs to ("BLAS", "LAPACK", "UNKNOWN")
- `directory` (string): Parent directory path

**Example:**
```cypher
(:File {
  path: "BLAS/SRC/dgemm.f",
  name: "dgemm.f",
  library: "BLAS",
  directory: "BLAS/SRC"
})
```

### 3. Operation Node
Represents a mathematical operation or algorithm.

**Label:** `Operation`

**Properties:**
- `name` (string): Operation identifier (e.g., "gemm" for matrix multiply, "getrf" for LU factorization)

**Example:**
```cypher
(:Operation {name: "gemm"})
```

### 4. Precision Node
Represents floating-point precision types.

**Label:** `Precision`

**Properties:**
- `symbol` (string): Single character precision code ('s', 'd', 'c', 'z')
- `name` (string): Full precision name ("single", "double", "complex", "double complex")

**Example:**
```cypher
(:Precision {symbol: "d", name: "double"})
```

## Relationship Types

### 1. CALLS
Indicates that one routine calls another routine.

**From:** Routine  
**To:** Routine  
**Properties:**
- `direct` (boolean): Always true for direct calls

**Example:**
```cypher
(dgetrf:Routine {name: "DGETRF"})-[:CALLS {direct: true}]->(dgemm:Routine {name: "DGEMM"})
```

### 2. DEFINED_IN
Links a routine to the file where it's defined.

**From:** Routine  
**To:** File  
**Properties:** None

**Example:**
```cypher
(dgemm:Routine {name: "DGEMM"})-[:DEFINED_IN]->(file:File {path: "BLAS/SRC/dgemm.f"})
```

### 3. IMPLEMENTS
Links a routine to the mathematical operation it implements.

**From:** Routine  
**To:** Operation  
**Properties:** None

**Example:**
```cypher
(dgemm:Routine {name: "DGEMM"})-[:IMPLEMENTS]->(op:Operation {name: "gemm"})
```

### 4. HAS_PRECISION
Links a routine to its precision type.

**From:** Routine  
**To:** Precision  
**Properties:** None

**Example:**
```cypher
(dgemm:Routine {name: "DGEMM"})-[:HAS_PRECISION]->(p:Precision {symbol: "d"})
```

## Indexes and Constraints

The following indexes and constraints are created for performance and data integrity:

```cypher
-- Uniqueness constraints
CREATE CONSTRAINT FOR (r:Routine) REQUIRE r.name IS UNIQUE;
CREATE CONSTRAINT FOR (f:File) REQUIRE f.path IS UNIQUE;
CREATE CONSTRAINT FOR (o:Operation) REQUIRE o.name IS UNIQUE;
CREATE CONSTRAINT FOR (p:Precision) REQUIRE p.symbol IS UNIQUE;

-- Additional indexes for common queries
CREATE INDEX FOR (r:Routine) ON (r.precision);
CREATE INDEX FOR (r:Routine) ON (r.operation);
CREATE INDEX FOR (f:File) ON (f.library);
```

## Common Query Patterns

### 1. Find all callers of a specific routine
```cypher
MATCH (caller:Routine)-[:CALLS]->(callee:Routine {name: 'DGEMM'})
RETURN caller.name, caller.precision
ORDER BY caller.name
```

### 2. Find all precision variants of an operation
```cypher
MATCH (r:Routine)-[:IMPLEMENTS]->(op:Operation {name: 'gemm'})
RETURN r.name, r.precision
ORDER BY r.precision, r.name
```

### 3. Find dependency chain for a routine
```cypher
MATCH path = (r:Routine {name: 'DGETRF'})-[:CALLS*1..5]->(dep:Routine)
RETURN path
```

### 4. Find most frequently called routines
```cypher
MATCH (r:Routine)<-[:CALLS]-(caller)
RETURN r.name, count(caller) as call_count
ORDER BY call_count DESC
LIMIT 20
```

### 5. Find all routines in a specific file
```cypher
MATCH (r:Routine)-[:DEFINED_IN]->(f:File {name: 'dgemm.f'})
RETURN r.name, r.type
```

### 6. Find files with most routines
```cypher
MATCH (r:Routine)-[:DEFINED_IN]->(f:File)
RETURN f.path, count(r) as routine_count
ORDER BY routine_count DESC
LIMIT 10
```

### 7. Find circular dependencies
```cypher
MATCH path = (r:Routine)-[:CALLS*2..10]->(r)
RETURN path
LIMIT 10
```

### 8. Compare routine counts by precision
```cypher
MATCH (r:Routine)-[:HAS_PRECISION]->(p:Precision)
RETURN p.name, p.symbol, count(r) as routine_count
ORDER BY p.symbol
```

## Data Import

The schema supports multiple import methods:

1. **Direct API**: Using Neo4j Python driver
2. **CSV Import**: Using neo4j-admin import tool
3. **Cypher Scripts**: Direct execution of CREATE statements

## Visualization

The graph structure enables various visualizations:
- Call dependency graphs
- File organization trees
- Operation clustering by precision
- Critical path analysis

## Future Extensions

Potential schema extensions:
- Module nodes for Fortran 90+ modules
- Common block relationships
- Parameter dependencies
- Performance metrics
- Test coverage links