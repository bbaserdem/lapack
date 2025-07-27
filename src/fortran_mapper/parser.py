"""
LAPACK Parser - Parse and analyze LAPACK/BLAS Fortran source code

This module provides functionality for parsing LAPACK and BLAS
Fortran source files, building dependency graphs, and exporting the results
in various formats including Neo4j graph database.
"""

import re
import json
from pathlib import Path
from typing import List, Dict, Set, Optional, Any, Tuple
from dataclasses import dataclass, field

@dataclass
class Subroutine:
    """Represents a Fortran subroutine with its dependencies."""
    name: str
    file_path: str
    line_number: int
    calls: Set[str] = field(default_factory=set)
    called_by: Set[str] = field(default_factory=set)
    arguments: List[str] = field(default_factory=list)
    external_routines: Set[str] = field(default_factory=set)
    
    def to_dict(self) -> dict:
        """Convert to dictionary for JSON serialization."""
        return {
            'name': self.name,
            'file_path': self.file_path,
            'line_number': self.line_number,
            'calls': sorted(list(self.calls)),
            'called_by': sorted(list(self.called_by)),
            'arguments': self.arguments,
            'external_routines': sorted(list(self.external_routines))
        }


class LapackParser:
    """Parser for LAPACK/BLAS Fortran source code."""
    
    def __init__(self):
        self.subroutines: Dict[str, Subroutine] = {}
        self.files_parsed: Set[str] = set()
        self.errors: List[str] = []
        
        # Common LAPACK/BLAS routine patterns
        self.lapack_patterns = {
            'blas1': re.compile(r'[SDCZ](DOT|AXPY|SCAL|COPY|SWAP|NRM2|ASUM|AMAX)', re.IGNORECASE),
            'blas2': re.compile(r'[SDCZ](GEMV|GBMV|HEMV|HBMV|HPMV|SYMV|SBMV|SPMV|TRMV|TBMV|TPMV|TRSV|TBSV|TPSV|GER|GERU|GERC|HER|HPR|HER2|HPR2|SYR|SPR|SYR2|SPR2)', re.IGNORECASE),
            'blas3': re.compile(r'[SDCZ](GEMM|SYMM|HEMM|SYRK|HERK|SYR2K|HER2K|TRMM|TRSM)', re.IGNORECASE),
            'lapack': re.compile(r'[SDCZ](GETRF|GETRS|GETRI|POTRF|POTRS|POTRI|GEQRF|GEQP3|ORGQR|ORMQR|GELQF|ORGLQ|ORMLQ|GESVD|GESDD|GBTRF|GBTRS|GTTRF|GTTRS)', re.IGNORECASE)
        }
    
    def parse_file(self, file_path: Path) -> None:
        """Parse a single Fortran file."""
        try:
            with open(file_path, 'r', encoding='latin-1') as f:
                content = f.read()
            
            self.files_parsed.add(str(file_path))
            
            # Find all subroutines/functions
            subroutine_pattern = re.compile(
                r'^\s*(SUBROUTINE|FUNCTION)\s+(\w+)\s*\((.*?)\)',
                re.IGNORECASE | re.MULTILINE | re.DOTALL
            )
            
            for match in subroutine_pattern.finditer(content):
                routine_type = match.group(1).upper()
                routine_name = match.group(2).upper()
                arguments = match.group(3)
                
                # Get line number
                line_number = content[:match.start()].count('\n') + 1
                
                # Parse arguments
                arg_list = []
                if arguments.strip():
                    # Remove comments and split by comma
                    args_clean = re.sub(r'!.*$', '', arguments, flags=re.MULTILINE)
                    arg_list = [arg.strip() for arg in args_clean.split(',') if arg.strip()]
                
                # Create subroutine object
                sub = Subroutine(
                    name=routine_name,
                    file_path=str(file_path),
                    line_number=line_number,
                    arguments=arg_list
                )
                
                # Find the end of this subroutine
                end_pattern = re.compile(
                    rf'^\s*END\s+{routine_type}\s+{routine_name}|^\s*END\s*$',
                    re.IGNORECASE | re.MULTILINE
                )
                
                routine_start = match.end()
                end_match = end_pattern.search(content, routine_start)
                routine_end = end_match.start() if end_match else len(content)
                
                routine_content = content[routine_start:routine_end]
                
                # Parse EXTERNAL declarations
                external_pattern = re.compile(
                    r'^\s*EXTERNAL\s+(.*?)$',
                    re.IGNORECASE | re.MULTILINE
                )
                
                for ext_match in external_pattern.finditer(routine_content):
                    externals = ext_match.group(1)
                    # Split by comma and clean
                    for ext in externals.split(','):
                        ext_name = ext.strip().upper()
                        if ext_name:
                            sub.external_routines.add(ext_name)
                
                # Parse CALL statements
                call_pattern = re.compile(
                    r'CALL\s+(\w+)\s*\(',
                    re.IGNORECASE
                )
                
                for call_match in call_pattern.finditer(routine_content):
                    called_routine = call_match.group(1).upper()
                    sub.calls.add(called_routine)
                
                # Parse function calls (common patterns)
                # Look for known BLAS/LAPACK functions
                for category, pattern in self.lapack_patterns.items():
                    for func_match in pattern.finditer(routine_content):
                        func_name = func_match.group(0).upper()
                        # Make sure it's actually a function call
                        if re.search(rf'\b{func_name}\s*\(', routine_content, re.IGNORECASE):
                            sub.calls.add(func_name)
                
                # Store the subroutine
                self.subroutines[routine_name] = sub
                
        except Exception as e:
            self.errors.append(f"Error parsing {file_path}: {e}")
    
    def parse_directory(self, directory: Path, patterns: Optional[List[str]] = None,
                       exclude_patterns: Optional[List[str]] = None) -> None:
        """Parse all Fortran files in a directory recursively."""
        if patterns is None:
            patterns = ['*.f', '*.f90', '*.F', '*.F90', '*.for']
        
        if exclude_patterns is None:
            exclude_patterns = []
        
        for pattern in patterns:
            for file_path in Path(directory).rglob(pattern):
                # Check exclusions
                if any(exc in str(file_path) for exc in exclude_patterns):
                    continue
                
                print(f"Parsing {file_path}")
                self.parse_file(file_path)
        
        # Build reverse dependencies
        self._build_reverse_dependencies()
    
    def _build_reverse_dependencies(self) -> None:
        """Build the called_by relationships."""
        for name, sub in self.subroutines.items():
            for called in sub.calls:
                if called in self.subroutines:
                    self.subroutines[called].called_by.add(name)
    
    def export_to_json(self, output_path: Path) -> None:
        """Export the parsed data to JSON."""
        data = {
            'subroutines': {
                name: sub.to_dict() 
                for name, sub in self.subroutines.items()
            },
            'statistics': {
                'total_subroutines': len(self.subroutines),
                'total_calls': sum(len(sub.calls) for sub in self.subroutines.values()),
                'files_parsed': len(self.files_parsed),
                'errors': self.errors
            }
        }
        
        with open(output_path, 'w') as f:
            json.dump(data, f, indent=2)
        
        print(f"Exported to {output_path}")
    
    def _extract_precision(self, name: str) -> Optional[str]:
        """Extract precision from routine name."""
        if not name:
            return None
        
        first_char = name[0].upper()
        precision_map = {
            'S': 'single',
            'D': 'double',
            'C': 'complex',
            'Z': 'double_complex'
        }
        return precision_map.get(first_char)
    
    def _extract_category(self, name: str) -> Optional[str]:
        """Extract category from routine name based on LAPACK/BLAS patterns."""
        if not name or len(name) < 2:
            return None
        
        # Check for common patterns in the name
        name_upper = name.upper()
        
        # Check for specific operation patterns first (more specific than type patterns)
        if self._extract_precision(name):
            if 'COPY' in name_upper or 'SWAP' in name_upper or 'SCAL' in name_upper:
                return 'vector_operation'
            elif 'GEMM' in name_upper or 'GEMV' in name_upper:
                return 'matrix_multiplication'
            elif 'DOT' in name_upper:
                return 'inner_product'
            elif 'NRM' in name_upper or 'NORM' in name_upper:
                return 'norm'
            elif 'SUM' in name_upper or 'ASUM' in name_upper:
                return 'sum'
            elif 'MAX' in name_upper or 'MIN' in name_upper:
                return 'extrema'
        
        # Matrix types (2nd and 3rd characters)
        if len(name_upper) >= 3:
            matrix_type = name_upper[1:3]
            matrix_categories = {
                'GE': 'general',
                'GB': 'general_band',
                'GT': 'general_tridiagonal',
                'PO': 'positive_definite',
                'PP': 'positive_definite_packed',
                'PB': 'positive_definite_band',
                'PT': 'positive_definite_tridiagonal',
                'SY': 'symmetric',
                'SP': 'symmetric_packed',
                'SB': 'symmetric_band',
                'ST': 'symmetric_tridiagonal',
                'HE': 'hermitian',
                'HP': 'hermitian_packed',
                'HB': 'hermitian_band',
                'TR': 'triangular',
                'TP': 'triangular_packed',
                'TB': 'triangular_band',
                'OR': 'orthogonal',
                'UN': 'unitary',
                'OP': 'orthogonal_packed',
                'UP': 'unitary_packed'
            }
            
            if matrix_type in matrix_categories:
                # Check for common operations
                if 'TRF' in name_upper:
                    return f"{matrix_categories[matrix_type]}_factorization"
                elif 'TRS' in name_upper:
                    return f"{matrix_categories[matrix_type]}_solve"
                elif 'TRI' in name_upper:
                    return f"{matrix_categories[matrix_type]}_inverse"
                elif 'EQR' in name_upper:
                    return f"{matrix_categories[matrix_type]}_qr"
                elif 'SVD' in name_upper:
                    return f"{matrix_categories[matrix_type]}_svd"
                elif 'EIG' in name_upper or 'EV' in name_upper:
                    return f"{matrix_categories[matrix_type]}_eigenvalue"
                else:
                    return matrix_categories[matrix_type]
        
        # Special routines
        if name_upper in ['XERBLA', 'LSAME', 'XERBLA_ARRAY']:
            return 'utility'
        
        # Check for BLAS/LAPACK general patterns as fallback
        for category, pattern in self.lapack_patterns.items():
            if pattern.match(name):
                return category
        
        return 'other'
    
    def export_to_neo4j(self, driver) -> None:
        """Export the parsed data to Neo4j."""
        with driver.session() as session:
            # Clear existing data
            print("Clearing existing data...")
            session.run("MATCH (n) DETACH DELETE n")
            
            # Create constraints
            print("Creating constraints...")
            session.run("CREATE CONSTRAINT routine_name IF NOT EXISTS FOR (r:Routine) REQUIRE r.name IS UNIQUE")
            session.run("CREATE CONSTRAINT file_path IF NOT EXISTS FOR (f:File) REQUIRE f.path IS UNIQUE")
            
            # Create file nodes
            print("Creating file nodes...")
            files = set(sub.file_path for sub in self.subroutines.values())
            for file_path in files:
                session.run(
                    "CREATE (f:File {path: $path, name: $name})",
                    path=file_path,
                    name=Path(file_path).name
                )
            
            # Create routine nodes
            print("Creating routine nodes...")
            for name, sub in self.subroutines.items():
                # Determine routine type
                routine_type = 'unknown'
                for category, pattern in self.lapack_patterns.items():
                    if pattern.match(name):
                        routine_type = category
                        break
                
                # Extract precision and category
                precision = self._extract_precision(name)
                category = self._extract_category(name)
                
                session.run("""
                    CREATE (r:Routine {
                        name: $name,
                        type: $type,
                        precision: $precision,
                        category: $category,
                        file_path: $file_path,
                        line_number: $line,
                        argument_count: $arg_count
                    })
                """, name=name, type=routine_type, precision=precision,
                    category=category, file_path=sub.file_path, 
                    line=sub.line_number, arg_count=len(sub.arguments))
            
            # Create DEFINED_IN relationships
            print("Creating DEFINED_IN relationships...")
            for name, sub in self.subroutines.items():
                session.run("""
                    MATCH (r:Routine {name: $name})
                    MATCH (f:File {path: $path})
                    CREATE (r)-[:DEFINED_IN {line: $line}]->(f)
                """, name=name, path=sub.file_path, line=sub.line_number)
            
            # Create CALLS relationships
            print("Creating CALLS relationships...")
            for name, sub in self.subroutines.items():
                for called in sub.calls:
                    if called in self.subroutines:
                        session.run("""
                            MATCH (caller:Routine {name: $caller})
                            MATCH (callee:Routine {name: $callee})
                            CREATE (caller)-[:CALLS]->(callee)
                        """, caller=name, callee=called)
            
            print("Export to Neo4j complete!")
    
    def export_to_dot(self, output_path: Path, include_files: bool = False) -> None:
        """Export to GraphViz DOT format."""
        with open(output_path, 'w') as f:
            f.write('digraph LAPACK {\n')
            f.write('  rankdir=LR;\n')
            f.write('  node [shape=box];\n\n')
            
            # Add nodes
            for name, sub in self.subroutines.items():
                # Color by type
                color = 'lightblue'
                for category, pattern in self.lapack_patterns.items():
                    if pattern.match(name):
                        colors = {
                            'blas1': 'lightgreen',
                            'blas2': 'lightcoral',
                            'blas3': 'lightyellow',
                            'lapack': 'lightblue'
                        }
                        color = colors.get(category, 'lightgray')
                        break
                
                f.write(f'  "{name}" [fillcolor={color}, style=filled];\n')
            
            # Add edges
            f.write('\n')
            for name, sub in self.subroutines.items():
                for called in sub.calls:
                    if called in self.subroutines:
                        f.write(f'  "{name}" -> "{called}";\n')
            
            f.write('}\n')
        
        print(f"Exported to {output_path}")
    
    def export_to_graphml(self, output_path: Path) -> None:
        """Export to GraphML format."""
        import xml.etree.ElementTree as ET
        
        graphml = ET.Element('graphml', xmlns="http://graphml.graphdrawing.org/xmlns")
        
        # Define keys
        key_name = ET.SubElement(graphml, 'key', id="name", **{'for': 'node', 'attr.name': 'name', 'attr.type': 'string'})
        key_type = ET.SubElement(graphml, 'key', id="type", **{'for': 'node', 'attr.name': 'type', 'attr.type': 'string'})
        key_file = ET.SubElement(graphml, 'key', id="file", **{'for': 'node', 'attr.name': 'file', 'attr.type': 'string'})
        
        # Create graph
        graph = ET.SubElement(graphml, 'graph', id="G", edgedefault="directed")
        
        # Add nodes
        for name, sub in self.subroutines.items():
            node = ET.SubElement(graph, 'node', id=name)
            
            data_name = ET.SubElement(node, 'data', key="name")
            data_name.text = name
            
            # Determine type
            routine_type = 'unknown'
            for category, pattern in self.lapack_patterns.items():
                if pattern.match(name):
                    routine_type = category
                    break
            
            data_type = ET.SubElement(node, 'data', key="type")
            data_type.text = routine_type
            
            data_file = ET.SubElement(node, 'data', key="file")
            data_file.text = Path(sub.file_path).name
        
        # Add edges
        edge_id = 0
        for name, sub in self.subroutines.items():
            for called in sub.calls:
                if called in self.subroutines:
                    edge = ET.SubElement(graph, 'edge', id=f"e{edge_id}", source=name, target=called)
                    edge_id += 1
        
        # Write to file
        tree = ET.ElementTree(graphml)
        tree.write(output_path, encoding='UTF-8', xml_declaration=True)
        
        print(f"Exported to {output_path}")
    
    def import_from_neo4j(self, driver) -> None:
        """Import data from Neo4j database."""
        with driver.session() as session:
            # Get all routines
            result = session.run("""
                MATCH (r:Routine)-[:DEFINED_IN]->(f:File)
                RETURN r.name as name, f.path as file_path, 
                       r.line_number as line_number
            """)
            
            for record in result:
                sub = Subroutine(
                    name=record['name'],
                    file_path=record['file_path'],
                    line_number=record['line_number'] or 0
                )
                self.subroutines[record['name']] = sub
            
            # Get all calls
            calls_result = session.run("""
                MATCH (caller:Routine)-[:CALLS]->(callee:Routine)
                RETURN caller.name as caller, callee.name as callee
            """)
            
            for record in calls_result:
                if record['caller'] in self.subroutines:
                    self.subroutines[record['caller']].calls.add(record['callee'])
                if record['callee'] in self.subroutines:
                    self.subroutines[record['callee']].called_by.add(record['caller'])