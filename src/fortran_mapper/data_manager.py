"""Data management utilities for LAPACK Neo4j graphs."""

import os
from datetime import datetime
from pathlib import Path
from typing import Optional
from .neo4j_client import Neo4jClient

class DataManager:
    """Manage Neo4j LAPACK graph data - backup, restore, and share."""
    
    def __init__(self, uri: str = "bolt://localhost:7687",
                 username: str = "", password: str = ""):
        """Initialize data manager."""
        self.uri = uri
        self.username = username
        self.password = password
    
    def create_backup(self, backup_dir: str = "backups") -> Path:
        """Create a backup of the current Neo4j data."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_path = Path(backup_dir) / f"backup_{timestamp}"
        backup_path.mkdir(parents=True, exist_ok=True)
        
        print(f"Creating backup in: {backup_path}")
        
        with Neo4jClient(self.uri, self.username, self.password) as client:
            # Export all formats
            client.export_to_cypher(str(backup_path / f"lapack_graph_{timestamp}.cypher"))
            client.export_to_json(str(backup_path / f"lapack_graph_{timestamp}.json"))
            client.export_statistics(str(backup_path / f"lapack_stats_{timestamp}.json"))
        
        # Create README
        readme_path = backup_path / "README.md"
        with open(readme_path, 'w') as f:
            f.write(f"# LAPACK Graph Backup\n\n")
            f.write(f"Created: {datetime.now().isoformat()}\n\n")
            f.write("## Files\n\n")
            f.write("- `*.cypher` - Cypher statements to recreate the graph\n")
            f.write("- `*.json` - JSON export of nodes and relationships\n")
            f.write("- `*_stats.json` - Graph statistics and summary\n\n")
            f.write("## Restore\n\n")
            f.write("```bash\n")
            f.write("# Using Cypher file\n")
            f.write(f"fortran-mapper import {backup_path}/lapack_graph_*.cypher\n\n")
            f.write("# Using JSON file\n")
            f.write(f"fortran-mapper import {backup_path}/lapack_graph_*.json\n")
            f.write("```\n")
        
        print(f"\nBackup created successfully: {backup_path}")
        return backup_path
    
    def restore_backup(self, backup_path: str) -> bool:
        """Restore from a backup."""
        backup_path = Path(backup_path)
        
        if not backup_path.exists():
            print(f"Backup not found: {backup_path}")
            return False
        
        # Find the appropriate file
        if backup_path.is_dir():
            # Look for cypher file first, then json
            cypher_files = list(backup_path.glob("*.cypher"))
            json_files = list(backup_path.glob("*.json"))
            
            if cypher_files:
                backup_file = cypher_files[0]
            elif json_files:
                # Skip stats files
                json_files = [f for f in json_files if "stats" not in f.name]
                if json_files:
                    backup_file = json_files[0]
                else:
                    print(f"No valid backup files found in: {backup_path}")
                    return False
            else:
                print(f"No valid backup files found in: {backup_path}")
                return False
        else:
            backup_file = backup_path
        
        print(f"Restoring from: {backup_file}")
        
        with Neo4jClient(self.uri, self.username, self.password) as client:
            if str(backup_file).endswith('.cypher'):
                client.import_from_cypher(str(backup_file))
            else:
                client.import_from_json(str(backup_file))
            
            # Verify
            stats = client.verify_data()
            print(f"\nData restored successfully:")
            print(f"  Total nodes: {stats['total_nodes']}")
            print(f"  Total relationships: {stats['total_relationships']}")
        
        return True
    
    def list_backups(self, backup_dir: str = "backups") -> None:
        """List available backups."""
        backup_dir = Path(backup_dir)
        
        if not backup_dir.exists():
            print(f"No backup directory found: {backup_dir}")
            return
        
        backups = sorted([d for d in backup_dir.iterdir() 
                         if d.is_dir() and d.name.startswith("backup_")])
        
        if not backups:
            print("No backups found.")
            return
        
        print(f"Available backups in {backup_dir}:\n")
        for backup in backups:
            # Get size
            size = sum(f.stat().st_size for f in backup.rglob("*") if f.is_file())
            size_mb = size / (1024 * 1024)
            
            # Count files
            file_count = len(list(backup.glob("*")))
            
            print(f"  {backup.name}")
            print(f"    Files: {file_count}, Size: {size_mb:.2f} MB")
            
            # Check for specific file types
            if list(backup.glob("*.cypher")):
                print("    ✓ Cypher export")
            if list(backup.glob("*.json")):
                print("    ✓ JSON export")
            if list(backup.glob("*_stats.json")):
                print("    ✓ Statistics")
            print()
    
    def create_sample_data(self, output_file: str = "data/sample_lapack_graph.cypher") -> None:
        """Create a sample dataset for testing."""
        output_path = Path(output_file)
        
        print(f"Creating sample data: {output_path}")
        
        sample_cypher = """// Sample LAPACK dependency graph for testing
// This represents a small subset of LAPACK routines

// Create constraints
CREATE CONSTRAINT routine_name IF NOT EXISTS FOR (r:Routine) REQUIRE r.name IS UNIQUE;
CREATE CONSTRAINT file_path IF NOT EXISTS FOR (f:File) REQUIRE f.path IS UNIQUE;

// Core BLAS Level 1 routines
CREATE (ddot:Routine {name: 'DDOT', type: 'blas1', description: 'Dot product of vectors'});
CREATE (dscal:Routine {name: 'DSCAL', type: 'blas1', description: 'Scale vector by constant'});
CREATE (daxpy:Routine {name: 'DAXPY', type: 'blas1', description: 'y = a*x + y'});
CREATE (dnrm2:Routine {name: 'DNRM2', type: 'blas1', description: '2-norm of vector'});

// BLAS Level 2 routines
CREATE (dgemv:Routine {name: 'DGEMV', type: 'blas2', description: 'Matrix-vector multiply'});
CREATE (dger:Routine {name: 'DGER', type: 'blas2', description: 'Rank-1 update'});
CREATE (dtrsv:Routine {name: 'DTRSV', type: 'blas2', description: 'Triangular solve'});

// BLAS Level 3 routines
CREATE (dgemm:Routine {name: 'DGEMM', type: 'blas3', description: 'Matrix-matrix multiply'});
CREATE (dtrsm:Routine {name: 'DTRSM', type: 'blas3', description: 'Triangular matrix solve'});

// LAPACK routines
CREATE (dgetrf:Routine {name: 'DGETRF', type: 'factorization', description: 'LU factorization'});
CREATE (dgetrs:Routine {name: 'DGETRS', type: 'solver', description: 'Solve using LU factors'});
CREATE (dgeqrf:Routine {name: 'DGEQRF', type: 'factorization', description: 'QR factorization'});
CREATE (dpotrf:Routine {name: 'DPOTRF', type: 'factorization', description: 'Cholesky factorization'});

// Create file nodes
CREATE (f1:File {path: 'BLAS/SRC/ddot.f', type: 'fortran'});
CREATE (f2:File {path: 'BLAS/SRC/dgemm.f', type: 'fortran'});
CREATE (f3:File {path: 'SRC/dgetrf.f', type: 'fortran'});
CREATE (f4:File {path: 'SRC/dgetrs.f', type: 'fortran'});

// Define where routines are implemented
MATCH (r:Routine {name: 'DDOT'}), (f:File {path: 'BLAS/SRC/ddot.f'})
CREATE (r)-[:DEFINED_IN]->(f);

MATCH (r:Routine {name: 'DGEMM'}), (f:File {path: 'BLAS/SRC/dgemm.f'})
CREATE (r)-[:DEFINED_IN]->(f);

MATCH (r:Routine {name: 'DGETRF'}), (f:File {path: 'SRC/dgetrf.f'})
CREATE (r)-[:DEFINED_IN]->(f);

MATCH (r:Routine {name: 'DGETRS'}), (f:File {path: 'SRC/dgetrs.f'})
CREATE (r)-[:DEFINED_IN]->(f);

// Create call relationships
// DGETRF calls multiple BLAS routines
MATCH (caller:Routine {name: 'DGETRF'}), (callee:Routine {name: 'DGEMM'})
CREATE (caller)-[:CALLS {line: 234}]->(callee);

MATCH (caller:Routine {name: 'DGETRF'}), (callee:Routine {name: 'DTRSM'})
CREATE (caller)-[:CALLS {line: 267}]->(callee);

MATCH (caller:Routine {name: 'DGETRF'}), (callee:Routine {name: 'DSCAL'})
CREATE (caller)-[:CALLS {line: 189}]->(callee);

// DGETRS calls BLAS routines
MATCH (caller:Routine {name: 'DGETRS'}), (callee:Routine {name: 'DTRSV'})
CREATE (caller)-[:CALLS {line: 123}]->(callee);

MATCH (caller:Routine {name: 'DGETRS'}), (callee:Routine {name: 'DGEMV'})
CREATE (caller)-[:CALLS {line: 145}]->(callee);

// DGEMM calls lower level BLAS
MATCH (caller:Routine {name: 'DGEMM'}), (callee:Routine {name: 'DDOT'})
CREATE (caller)-[:CALLS {line: 567}]->(callee);

// Add metadata
MATCH (r:Routine)
SET r.analyzed = true, 
    r.created_at = datetime(),
    r.sample_data = true;
"""
        
        output_path.parent.mkdir(parents=True, exist_ok=True)
        with open(output_path, 'w') as f:
            f.write(sample_cypher)
        
        print(f"Sample data created: {output_path}")
        print(f"\nTo import this sample data:")
        print(f"  fortran-mapper import {output_path}")