#!/usr/bin/env python3
"""
Example: Open Neo4j Browser with a specific routine visualization.

Usage:
    python visualize_routine.py DGEMM
    python visualize_routine.py XERBLA --depth 3
"""

import sys
import argparse
from pathlib import Path
sys.path.append(str(Path(__file__).parent.parent / 'src'))

from lapack_util.browser_integration import Neo4jBrowserIntegration


def visualize_routine(routine_name: str, depth: int = 2):
    """Open Neo4j Browser with visualization of a specific routine."""
    
    # Create the query
    query = f"""
// Visualization of {routine_name} and its call network (depth={depth})
MATCH path = (start:Routine {{name: '{routine_name}'}})-[:CALLS*0..{depth}]-(connected:Routine)
WITH start, connected, path
RETURN start, connected, path
LIMIT 150
"""
    
    # Open in browser
    browser = Neo4jBrowserIntegration()
    print(f"Opening visualization for routine: {routine_name}")
    print(f"Call depth: {depth}")
    print(f"\nQuery being sent to Neo4j Browser:")
    print("-" * 50)
    print(query)
    print("-" * 50)
    
    url = browser.open_with_query(query)
    print(f"\nBrowser opened with visualization!")
    print(f"URL: {url}")
    
    # Also show some example queries for this routine
    print(f"\n\nAdditional queries you can run for {routine_name}:")
    
    print("\n1. Find what this routine calls:")
    print(f"MATCH (r:Routine {{name: '{routine_name}'}})-[:CALLS]->(called)")
    print("RETURN called.name, called.precision, called.category")
    
    print("\n2. Find what calls this routine:")
    print(f"MATCH (caller)-[:CALLS]->(r:Routine {{name: '{routine_name}'}})")
    print("RETURN caller.name, caller.precision, caller.category")
    
    print("\n3. Find the file where it's defined:")
    print(f"MATCH (r:Routine {{name: '{routine_name}'}})-[:DEFINED_IN]->(f:File)")
    print("RETURN f.path")


def main():
    parser = argparse.ArgumentParser(
        description='Visualize a specific LAPACK routine in Neo4j Browser'
    )
    parser.add_argument('routine', help='Name of the routine to visualize (e.g., DGEMM)')
    parser.add_argument('--depth', type=int, default=2,
                       help='Depth of call chain to visualize (default: 2)')
    
    args = parser.parse_args()
    
    # Convert routine name to uppercase (LAPACK convention)
    routine_name = args.routine.upper()
    
    visualize_routine(routine_name, args.depth)


if __name__ == '__main__':
    main()