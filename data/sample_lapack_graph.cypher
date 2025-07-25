// Sample LAPACK dependency graph data
// This is a small subset for testing and demonstration

// Create constraints (run once)
CREATE CONSTRAINT routine_name IF NOT EXISTS ON (r:Routine) ASSERT r.name IS UNIQUE;
CREATE CONSTRAINT file_path IF NOT EXISTS ON (f:File) ASSERT f.path IS UNIQUE;

// Create sample routines
CREATE (dgetrf:Routine {name: 'DGETRF', type: 'factorization', description: 'LU factorization'})
CREATE (dgetrs:Routine {name: 'DGETRS', type: 'solver', description: 'Solve using LU factors'})
CREATE (dgemm:Routine {name: 'DGEMM', type: 'blas3', description: 'Matrix-matrix multiply'})
CREATE (dgemv:Routine {name: 'DGEMV', type: 'blas2', description: 'Matrix-vector multiply'})
CREATE (dscal:Routine {name: 'DSCAL', type: 'blas1', description: 'Vector scaling'})
CREATE (dswap:Routine {name: 'DSWAP', type: 'blas1', description: 'Vector swap'})
CREATE (idamax:Routine {name: 'IDAMAX', type: 'blas1', description: 'Find max element'})

// Create sample files
CREATE (f1:File {path: 'SRC/dgetrf.f', type: 'fortran'})
CREATE (f2:File {path: 'SRC/dgetrs.f', type: 'fortran'})
CREATE (f3:File {path: 'BLAS/SRC/dgemm.f', type: 'fortran'})

// Create DEFINED_IN relationships
CREATE (dgetrf)-[:DEFINED_IN]->(f1)
CREATE (dgetrs)-[:DEFINED_IN]->(f2)
CREATE (dgemm)-[:DEFINED_IN]->(f3)

// Create CALLS relationships
CREATE (dgetrf)-[:CALLS {line: 123}]->(dgemm)
CREATE (dgetrf)-[:CALLS {line: 145}]->(dscal)
CREATE (dgetrf)-[:CALLS {line: 167}]->(dswap)
CREATE (dgetrf)-[:CALLS {line: 189}]->(idamax)
CREATE (dgetrs)-[:CALLS {line: 78}]->(dgemv)
CREATE (dgetrs)-[:CALLS {line: 92}]->(dswap)

// Add some metadata
MATCH (r:Routine)
SET r.analyzed = true, r.timestamp = datetime();