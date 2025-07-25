#!/usr/bin/env python3
"""
Example: Build complete LAPACK/BLAS graph and export to Neo4j

This script demonstrates how to:
1. Parse all LAPACK and BLAS source files
2. Build a complete dependency graph
3. Export to Neo4j with proper error handling
4. Generate statistics and reports
"""

from pathlib import Path
import sys
import time
from datetime import datetime
from collections import defaultdict

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.lapack_util.fortran_parser import FortranParser
from src.lapack_util.lapack_to_neo4j import LapackToNeo4j


def parse_with_progress(parser, directory, pattern="*.f", library_name=""):
    """Parse files with progress reporting."""
    files_parsed = 0
    errors = []
    start_time = time.time()
    
    files = list(directory.glob(pattern))
    total_files = len(files)
    
    print(f"\nParsing {library_name} ({total_files} files)...")
    
    for i, file_path in enumerate(files):
        try:
            result = parser.parse_file(file_path)
            if result.error:
                errors.append((file_path, result.error))
            files_parsed += 1
            
            # Progress report every 50 files
            if (i + 1) % 50 == 0:
                elapsed = time.time() - start_time
                rate = files_parsed / elapsed
                remaining = (total_files - files_parsed) / rate
                print(f"  Progress: {i+1}/{total_files} files "
                      f"({(i+1)/total_files*100:.1f}%) "
                      f"ETA: {remaining:.0f}s")
                
        except Exception as e:
            errors.append((file_path, str(e)))
    
    elapsed = time.time() - start_time
    print(f"  Completed: {files_parsed} files in {elapsed:.1f}s "
          f"({files_parsed/elapsed:.1f} files/sec)")
    
    return files_parsed, errors


def generate_statistics(parser):
    """Generate comprehensive statistics from parsed data."""
    stats = {
        'total_files': len(parser._parsed_files),
        'total_routines': 0,
        'by_type': defaultdict(int),
        'by_precision': defaultdict(int),
        'by_library': defaultdict(int),
        'call_graph_stats': {
            'total_calls': 0,
            'unique_callers': set(),
            'unique_callees': set()
        },
        'top_operations': defaultdict(int),
        'files_with_errors': 0
    }
    
    # Analyze parsed data
    for file_path, file_data in parser._parsed_files.items():
        if file_data.error:
            stats['files_with_errors'] += 1
            continue
            
        # Determine library
        path_str = str(file_path)
        if 'BLAS' in path_str:
            library = 'BLAS'
        elif 'LAPACK' in path_str or 'SRC' in path_str:
            library = 'LAPACK'
        else:
            library = 'OTHER'
        
        for routine in file_data.routines:
            stats['total_routines'] += 1
            stats['by_type'][routine.routine_type] += 1
            stats['by_library'][library] += 1
            
            if routine.precision:
                stats['by_precision'][routine.precision] += 1
            
            if routine.operation:
                stats['top_operations'][routine.operation] += 1
            
            if routine.calls:
                stats['call_graph_stats']['total_calls'] += len(routine.calls)
                stats['call_graph_stats']['unique_callers'].add(routine.name)
                stats['call_graph_stats']['unique_callees'].update(routine.calls)
    
    return stats


def print_statistics(stats):
    """Print formatted statistics."""
    print("\n" + "="*60)
    print("LAPACK/BLAS Parsing Statistics")
    print("="*60)
    
    print(f"\nFiles and Routines:")
    print(f"  Total files parsed: {stats['total_files']}")
    print(f"  Files with errors: {stats['files_with_errors']}")
    print(f"  Total routines found: {stats['total_routines']}")
    
    print(f"\nRoutines by Type:")
    for rtype, count in sorted(stats['by_type'].items()):
        print(f"  {rtype}: {count}")
    
    print(f"\nRoutines by Precision:")
    precision_names = {'s': 'Single', 'd': 'Double', 'c': 'Complex', 'z': 'Double Complex'}
    for prec in ['s', 'd', 'c', 'z']:
        if prec in stats['by_precision']:
            count = stats['by_precision'][prec]
            pct = count / stats['total_routines'] * 100
            print(f"  {prec} ({precision_names[prec]}): {count} ({pct:.1f}%)")
    
    print(f"\nRoutines by Library:")
    for lib, count in sorted(stats['by_library'].items()):
        print(f"  {lib}: {count}")
    
    print(f"\nCall Graph Statistics:")
    cg = stats['call_graph_stats']
    print(f"  Total function calls: {cg['total_calls']}")
    print(f"  Unique callers: {len(cg['unique_callers'])}")
    print(f"  Unique callees: {len(cg['unique_callees'])}")
    
    print(f"\nTop 10 Operations:")
    top_ops = sorted(stats['top_operations'].items(), key=lambda x: x[1], reverse=True)[:10]
    for op, count in top_ops:
        print(f"  {op}: {count} routines")


def export_to_neo4j_with_retry(parser, uri, user, password, max_retries=3):
    """Export to Neo4j with retry logic."""
    for attempt in range(max_retries):
        try:
            print(f"\nConnecting to Neo4j (attempt {attempt + 1}/{max_retries})...")
            neo4j = LapackToNeo4j(uri=uri, user=user, password=password)
            
            print("Clearing existing data...")
            neo4j.clear_database()
            
            print("Creating schema...")
            # Schema creation is handled in the LapackToNeo4j class
            
            print("Exporting data...")
            start_time = time.time()
            neo4j.process_parsed_data(parser._parsed_files)
            elapsed = time.time() - start_time
            
            print(f"Export completed in {elapsed:.1f}s")
            
            # Verify export
            with neo4j.driver.session() as session:
                result = session.run("""
                    MATCH (r:Routine) RETURN count(r) as routineCount
                    UNION ALL
                    MATCH (f:File) RETURN count(f) as fileCount
                    UNION ALL
                    MATCH ()-[c:CALLS]->() RETURN count(c) as callCount
                """)
                
                counts = list(result)
                print(f"\nVerification:")
                print(f"  Routines in Neo4j: {counts[0]['routineCount']}")
                print(f"  Files in Neo4j: {counts[1]['fileCount']}")
                print(f"  Call relationships: {counts[2]['callCount']}")
            
            neo4j.close()
            return True
            
        except Exception as e:
            print(f"Error on attempt {attempt + 1}: {e}")
            if attempt < max_retries - 1:
                print("Retrying in 5 seconds...")
                time.sleep(5)
            else:
                print("Export failed after all retries")
                return False


def main():
    # Configuration
    NEO4J_URI = "bolt://localhost:7687"
    NEO4J_USER = "neo4j"
    NEO4J_PASSWORD = "password"
    
    # Parse command line arguments
    import argparse
    parser_args = argparse.ArgumentParser(description='Build complete LAPACK/BLAS graph')
    parser_args.add_argument('--neo4j-uri', default=NEO4J_URI, help='Neo4j URI')
    parser_args.add_argument('--neo4j-user', default=NEO4J_USER, help='Neo4j username')
    parser_args.add_argument('--neo4j-password', default=NEO4J_PASSWORD, help='Neo4j password')
    parser_args.add_argument('--skip-neo4j', action='store_true', help='Skip Neo4j export')
    parser_args.add_argument('--blas-only', action='store_true', help='Parse only BLAS')
    parser_args.add_argument('--lapack-only', action='store_true', help='Parse only LAPACK')
    parser_args.add_argument('--output-stats', help='Save statistics to file')
    
    args = parser_args.parse_args()
    
    # Initialize parser
    parser = FortranParser()
    total_files = 0
    all_errors = []
    
    print(f"Starting LAPACK/BLAS parsing at {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    # Parse BLAS
    if not args.lapack_only:
        blas_path = Path("BLAS/SRC")
        if blas_path.exists():
            files_parsed, errors = parse_with_progress(parser, blas_path, "*.f", "BLAS")
            total_files += files_parsed
            all_errors.extend(errors)
        else:
            print(f"Warning: BLAS directory not found at {blas_path}")
    
    # Parse LAPACK
    if not args.blas_only:
        lapack_path = Path("SRC")
        if lapack_path.exists():
            files_parsed, errors = parse_with_progress(parser, lapack_path, "*.f", "LAPACK")
            total_files += files_parsed
            all_errors.extend(errors)
        else:
            print(f"Warning: LAPACK directory not found at {lapack_path}")
    
    # Report errors
    if all_errors:
        print(f"\nParsing errors ({len(all_errors)} files):")
        for file_path, error in all_errors[:10]:  # Show first 10
            print(f"  {file_path}: {error}")
        if len(all_errors) > 10:
            print(f"  ... and {len(all_errors) - 10} more")
    
    # Generate and print statistics
    stats = generate_statistics(parser)
    print_statistics(stats)
    
    # Save statistics if requested
    if args.output_stats:
        import json
        with open(args.output_stats, 'w') as f:
            # Convert sets to lists for JSON serialization
            stats_json = stats.copy()
            stats_json['call_graph_stats']['unique_callers'] = len(stats['call_graph_stats']['unique_callers'])
            stats_json['call_graph_stats']['unique_callees'] = len(stats['call_graph_stats']['unique_callees'])
            
            json.dump(stats_json, f, indent=2)
            print(f"\nStatistics saved to {args.output_stats}")
    
    # Export to Neo4j
    if not args.skip_neo4j:
        success = export_to_neo4j_with_retry(
            parser,
            args.neo4j_uri,
            args.neo4j_user,
            args.neo4j_password
        )
        
        if success:
            print("\n✓ Graph successfully built and exported to Neo4j!")
            print(f"\nNext steps:")
            print(f"1. Open Neo4j Browser at http://localhost:7474")
            print(f"2. Login with username '{args.neo4j_user}'")
            print(f"3. Try this query: MATCH (r:Routine) RETURN r LIMIT 25")
            print(f"4. See docs/NEO4J_QUERIES.md for more query examples")
        else:
            print("\n✗ Failed to export to Neo4j")
            sys.exit(1)
    
    print(f"\nCompleted at {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")


if __name__ == "__main__":
    main()