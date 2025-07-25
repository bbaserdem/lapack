#!/usr/bin/env python3
"""
Conceptual design for LAPACK -> Neo4j code graph using fortran-src

Neo4j Schema:
- Nodes:
  - (:Routine {name, file, type, precision, operation})
  - (:File {path, module})
  - (:Library {name}) // BLAS, LAPACK, etc.
  
- Relationships:
  - (:Routine)-[:CALLS]->(:Routine)
  - (:Routine)-[:DEFINED_IN]->(:File)
  - (:File)-[:BELONGS_TO]->(:Library)
  - (:Routine)-[:DEPENDS_ON]->(:Routine)
"""

# Example Cypher queries that would be useful:

# 1. Create routine nodes
create_routine = """
CREATE (r:Routine {
    name: $name,
    file: $file,
    type: $type,  // 'subroutine' or 'function'
    precision: $precision,  // 's', 'd', 'c', 'z'
    operation: $operation   // 'gemm', 'getrf', etc.
})
"""

# 2. Create call relationships
create_call = """
MATCH (caller:Routine {name: $caller_name}),
      (callee:Routine {name: $callee_name})
CREATE (caller)-[:CALLS]->(callee)
"""

# 3. Useful queries once populated:

# Find all routines that call DGEMM
find_dgemm_callers = """
MATCH (r:Routine)-[:CALLS]->(gemm:Routine {name: 'DGEMM'})
RETURN r.name, r.file
"""

# Find dependency chain for a routine
dependency_chain = """
MATCH path = (r:Routine {name: 'DGETRF'})-[:CALLS*1..5]->(dep:Routine)
RETURN path
"""

# Find all double precision GEMM variants
precision_variants = """
MATCH (r:Routine)
WHERE r.operation = 'gemm' AND r.precision = 'd'
RETURN r.name, r.file
"""

# Workflow:
def process_lapack_to_neo4j():
    """
    1. Parse all LAPACK/BLAS files with fortran-src
    2. Extract:
       - Procedure definitions (name, type)
       - Call statements
       - File locations
    3. Parse LAPACK naming conventions:
       - First letter = precision (S/D/C/Z)
       - Rest = operation (GEMM, GETRF, etc.)
    4. Create Neo4j import files or direct connection
    5. Build the graph with nodes and relationships
    """
    
    # Example data structure after parsing
    routines = {
        'DGEMM': {
            'file': 'BLAS/SRC/dgemm.f',
            'type': 'subroutine',
            'precision': 'd',
            'operation': 'gemm',
            'calls': []
        },
        'DGETRF': {
            'file': 'SRC/dgetrf.f', 
            'type': 'subroutine',
            'precision': 'd',
            'operation': 'getrf',
            'calls': ['DGEMM', 'DTRSM', 'DLASWP']
        },
        'DGESV': {
            'file': 'SRC/dgesv.f',
            'type': 'subroutine', 
            'precision': 'd',
            'operation': 'gesv',
            'calls': ['DGETRF', 'DGETRS']
        }
    }
    
    # This would generate CSV files for neo4j-admin import
    # or use py2neo/neo4j-driver to insert directly
    
    return routines

# Benefits of Neo4j for LAPACK:
# 1. Query call chains easily
# 2. Find all dependencies of a routine
# 3. Identify heavily-used core routines
# 4. Analyze precision variants
# 5. Visualize the entire call graph
# 6. Find unused routines
# 7. Identify circular dependencies