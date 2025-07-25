#!/usr/bin/env python3
"""
LAPACK Parser CLI - Parse and analyze LAPACK/BLAS Fortran source code

This tool provides a command-line interface for parsing LAPACK and BLAS
Fortran source files, building dependency graphs, and exporting the results
in various formats including Neo4j graph database.
"""

import argparse
import json
import sys
from pathlib import Path
from typing import List, Optional

from src.lapack_util.fortran_parser import FortranParser
from src.lapack_util.lapack_to_neo4j import LapackToNeo4j
from src.lapack_util.exporter import GraphExporter


def parse_directory(directory: Path, patterns: List[str], parser: FortranParser) -> dict:
    """Parse all Fortran files in a directory matching the given patterns."""
    results = {
        'files_parsed': 0,
        'routines_found': 0,
        'errors': [],
        'files': {}
    }
    
    for pattern in patterns:
        for file_path in directory.rglob(pattern):
            print(f"Parsing {file_path}...")
            result = parser.parse_file(file_path)
            
            results['files_parsed'] += 1
            results['files'][str(file_path)] = {
                'routines': len(result.routines),
                'error': result.error
            }
            
            if result.error:
                results['errors'].append(f"{file_path}: {result.error}")
            else:
                results['routines_found'] += len(result.routines)
    
    return results


def main():
    parser = argparse.ArgumentParser(
        description='Parse LAPACK/BLAS Fortran source code and build dependency graphs',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Parse a single file
  python lapack_parser.py --file BLAS/SRC/dgemm.f

  # Parse all BLAS source files
  python lapack_parser.py --directory BLAS/SRC --pattern "*.f"

  # Parse LAPACK and export to Neo4j
  python lapack_parser.py --directory . --neo4j --neo4j-uri bolt://localhost:7687

  # Export parsing results to JSON
  python lapack_parser.py --directory . --output lapack_graph.json

  # Parse with specific patterns
  python lapack_parser.py --directory . --pattern "d*.f" --pattern "z*.f"
        """
    )
    
    # Input options
    input_group = parser.add_mutually_exclusive_group(required=True)
    input_group.add_argument('--file', '-f', type=Path,
                           help='Parse a single Fortran file')
    input_group.add_argument('--directory', '-d', type=Path,
                           help='Parse all Fortran files in directory (recursive)')
    
    # Pattern options
    parser.add_argument('--pattern', '-p', action='append', default=[],
                      help='File patterns to match (default: *.f, *.f90, *.F, *.F90)')
    
    # Output options
    parser.add_argument('--output', '-o', type=Path,
                      help='Output file for results (JSON format)')
    parser.add_argument('--format', choices=['json', 'csv', 'graphml'], default='json',
                      help='Output format (default: json)')
    
    # Neo4j options
    parser.add_argument('--neo4j', action='store_true',
                      help='Export to Neo4j database')
    parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                      help='Neo4j connection URI (default: bolt://localhost:7687)')
    parser.add_argument('--neo4j-user', default='neo4j',
                      help='Neo4j username (default: neo4j)')
    parser.add_argument('--neo4j-password', default='password',
                      help='Neo4j password (default: password)')
    parser.add_argument('--neo4j-clear', action='store_true',
                      help='Clear existing data in Neo4j before import')
    
    # Analysis options
    parser.add_argument('--stats', action='store_true',
                      help='Show statistics about parsed code')
    parser.add_argument('--verbose', '-v', action='store_true',
                      help='Enable verbose output')
    
    args = parser.parse_args()
    
    # Set default patterns if none provided
    if not args.pattern:
        args.pattern = ['*.f', '*.f90', '*.F', '*.F90']
    
    # Initialize parser
    fortran_parser = FortranParser()
    
    # Parse files
    if args.file:
        # Single file mode
        print(f"Parsing {args.file}...")
        result = fortran_parser.parse_file(args.file)
        
        if result.error:
            print(f"Error: {result.error}", file=sys.stderr)
            sys.exit(1)
        
        if args.verbose:
            print(f"Found {len(result.routines)} routines:")
            for routine in result.routines:
                print(f"  - {routine.name} ({routine.routine_type})")
                if routine.calls:
                    print(f"    Calls: {', '.join(sorted(routine.calls))}")
    
    else:
        # Directory mode
        results = parse_directory(args.directory, args.pattern, fortran_parser)
        
        print(f"\nParsing Summary:")
        print(f"  Files parsed: {results['files_parsed']}")
        print(f"  Routines found: {results['routines_found']}")
        print(f"  Errors: {len(results['errors'])}")
        
        if args.verbose and results['errors']:
            print("\nErrors encountered:")
            for error in results['errors']:
                print(f"  - {error}")
        
        if args.stats:
            print("\nStatistics:")
            # Get all parsed data for statistics
            all_routines = []
            precision_counts = {'s': 0, 'd': 0, 'c': 0, 'z': 0}
            
            for file_path in fortran_parser._parsed_files:
                file_data = fortran_parser._parsed_files[file_path]
                for routine in file_data.routines:
                    all_routines.append(routine)
                    if routine.precision in precision_counts:
                        precision_counts[routine.precision] += 1
            
            print(f"  Total routines: {len(all_routines)}")
            print(f"  Precision distribution:")
            for prec, count in sorted(precision_counts.items()):
                if count > 0:
                    print(f"    {prec}: {count} ({count/len(all_routines)*100:.1f}%)")
    
    # Export to Neo4j if requested
    if args.neo4j:
        print(f"\nExporting to Neo4j at {args.neo4j_uri}...")
        try:
            neo4j_exporter = LapackToNeo4j(
                uri=args.neo4j_uri,
                user=args.neo4j_user,
                password=args.neo4j_password
            )
            
            if args.neo4j_clear:
                print("Clearing existing data...")
                neo4j_exporter.clear_database()
            
            # Export parsed data
            neo4j_exporter.process_parsed_data(fortran_parser._parsed_files)
            
            print("Export completed successfully!")
            
        except Exception as e:
            print(f"Error exporting to Neo4j: {e}", file=sys.stderr)
            sys.exit(1)
    
    # Export to file if requested
    if args.output:
        print(f"\nExporting to {args.output}...")
        exporter = GraphExporter()
        
        if args.format == 'json':
            # Prepare data for JSON export
            export_data = {
                'metadata': {
                    'total_files': len(fortran_parser._parsed_files),
                    'total_routines': sum(len(f.routines) for f in fortran_parser._parsed_files.values())
                },
                'files': {}
            }
            
            for file_path, file_data in fortran_parser._parsed_files.items():
                export_data['files'][str(file_path)] = {
                    'routines': [
                        {
                            'name': r.name,
                            'type': r.routine_type,
                            'precision': r.precision,
                            'operation': r.operation,
                            'calls': list(r.calls) if r.calls else [],
                            'lines': [r.line_start, r.line_end]
                        }
                        for r in file_data.routines
                    ]
                }
            
            with open(args.output, 'w') as f:
                json.dump(export_data, f, indent=2)
            
            print(f"Exported to {args.output}")
        
        elif args.format in ['csv', 'graphml']:
            # These formats would need implementation in GraphExporter
            print(f"Export format {args.format} not yet implemented", file=sys.stderr)
            sys.exit(1)


if __name__ == '__main__':
    main()