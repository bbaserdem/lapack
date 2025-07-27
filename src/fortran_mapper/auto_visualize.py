#!/usr/bin/env python3
"""Automated LAPACK graph visualization using Neo4j Browser integration."""

import json
import webbrowser
import urllib.parse
from pathlib import Path
from typing import Dict, List, Any, Optional
from datetime import datetime
from .browser_integration import Neo4jBrowserIntegration


class LAPACKAutoVisualizer:
    """Automated visualization for LAPACK computational graphs using Neo4j Browser."""
    
    def __init__(self, output_dir: str = "visualizations", 
                 neo4j_host: str = "localhost", neo4j_port: int = 7474):
        """Initialize the visualizer.
        
        Args:
            output_dir: Directory to save visualization outputs
            neo4j_host: Neo4j server host
            neo4j_port: Neo4j browser port
        """
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.browser = Neo4jBrowserIntegration(neo4j_host, neo4j_port)
        
    def open_query_in_browser(self, query: str, query_name: str = "Query") -> str:
        """Open a Cypher query directly in Neo4j Browser.
        
        Args:
            query: Cypher query to execute
            query_name: Name of the query for logging
            
        Returns:
            URL that was opened
        """
        print(f"\nOpening '{query_name}' in Neo4j Browser...")
        url = self.browser.open_with_query(query)
        return url
    
    def create_visualization_queries(self) -> Dict[str, str]:
        """Create a suite of visualization queries."""
        return {
            "Graph Overview": """
// Overview of LAPACK computational graph structure
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WITH r1, r2, rand() as random
ORDER BY random
LIMIT 200
RETURN r1, r2
""",
            
            "Most Connected Routines (Hubs)": """
// Top 30 most connected routines - the central hubs of LAPACK
MATCH (r:Routine)
WITH r, COUNT{(r)-[:CALLS]-()} as degree
ORDER BY degree DESC
LIMIT 30
MATCH (r)-[rel:CALLS]-(connected)
RETURN r, rel, connected
""",
            
            "Precision Distribution Network": """
// How different precision routines interact
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WHERE r1.precision IS NOT NULL AND r2.precision IS NOT NULL
WITH r1, r2, rand() as random
ORDER BY random
LIMIT 150
RETURN r1, r2
""",
            
            "Category Interaction Pattern": """
// How different categories of routines call each other
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WHERE r1.category <> 'other' AND r2.category <> 'other'
  AND r1.category IS NOT NULL AND r2.category IS NOT NULL
WITH r1, r2, rand() as random
ORDER BY random
LIMIT 150
RETURN r1, r2
""",
            
            "XERBLA Error Handling Network": """
// Error handling network - routines that call XERBLA
MATCH (xerbla:Routine {name: 'XERBLA'})
MATCH (caller)-[:CALLS]->(xerbla)
WITH xerbla, caller
LIMIT 100
RETURN xerbla, caller
""",
            
            "BLAS Level 3 Operations": """
// BLAS Level 3 (matrix-matrix) operations and their connections
MATCH (r:Routine {type: 'blas3'})
OPTIONAL MATCH (r)-[c:CALLS]-(connected:Routine)
WITH r, c, connected
LIMIT 150
RETURN r, c, connected
""",
            
            "Factorization Routines Network": """
// All factorization routines and their dependencies
MATCH (r:Routine)
WHERE r.category CONTAINS 'factorization'
OPTIONAL MATCH (r)-[c:CALLS]-(connected:Routine)
WITH r, c, connected
LIMIT 200
RETURN r, c, connected
""",
            
            "Eigenvalue Solver Dependencies": """
// Eigenvalue solvers and what they depend on
MATCH (r:Routine)
WHERE r.category CONTAINS 'eigenvalue' OR r.name CONTAINS 'EV'
OPTIONAL MATCH (r)-[:CALLS]->(dep:Routine)
WITH r, dep
WHERE dep IS NOT NULL
RETURN r, dep
LIMIT 150
""",
            
            "File-based Clustering": """
// Routines clustered by their source files
MATCH (f:File)<-[:DEFINED_IN]-(r:Routine)
WITH f, collect(r) as routines
WHERE size(routines) > 5 AND size(routines) < 25
LIMIT 5
UNWIND routines as r
OPTIONAL MATCH (r)-[c:CALLS]-(other:Routine)
WHERE other IN routines
RETURN f, r, c, other
""",
            
            "Isolated Routine Groups": """
// Find isolated groups of interconnected routines
MATCH (r:Routine)
WHERE NOT (r)-[:CALLS]-()
RETURN r
LIMIT 50
"""
        }
    
    def open_all_visualizations(self, delay_seconds: int = 2) -> None:
        """Open all visualization queries in Neo4j Browser tabs.
        
        Args:
            delay_seconds: Delay between opening each tab to avoid overwhelming the browser
        """
        import time
        
        queries = self.create_visualization_queries()
        
        print(f"\nOpening {len(queries)} visualization queries in Neo4j Browser...")
        print("Each query will open in a new browser tab.\n")
        
        for i, (name, query) in enumerate(queries.items(), 1):
            print(f"{i}. {name}")
            self.open_query_in_browser(query, name)
            
            if i < len(queries):  # Don't delay after the last query
                time.sleep(delay_seconds)
    
    def visualize_routine_network(self, routine_name: str, depth: int = 2) -> None:
        """Open visualization of the call network around a specific routine."""
        query = f"""
// Network visualization for routine: {routine_name} (depth={depth})
MATCH path = (start:Routine {{name: '{routine_name}'}})-[:CALLS*0..{depth}]-(connected:Routine)
WITH start, connected, path
RETURN start, connected, path
LIMIT 100
"""
        print(f"\nOpening network visualization for '{routine_name}' (depth={depth})...")
        self.open_query_in_browser(query, f"{routine_name} Network (depth {depth})")
    
    def create_interactive_guide(self) -> Path:
        """Create an interactive HTML guide with all visualization queries."""
        queries = self.create_visualization_queries()
        
        # Add some analysis queries
        analysis_queries = {
            "Routine Statistics": """
// Basic statistics about routines
MATCH (r:Routine)
WITH count(r) as total,
     count(CASE WHEN r.precision = 'single' THEN 1 END) as single_count,
     count(CASE WHEN r.precision = 'double' THEN 1 END) as double_count,
     count(CASE WHEN r.precision = 'complex' THEN 1 END) as complex_count,
     count(CASE WHEN r.precision = 'double_complex' THEN 1 END) as double_complex_count
RETURN total, single_count, double_count, complex_count, double_complex_count
""",
            
            "Most Called Routines": """
// Top 20 most frequently called routines
MATCH (caller)-[:CALLS]->(r:Routine)
RETURN r.name as routine, r.precision as precision, r.category as category,
       count(caller) as times_called
ORDER BY times_called DESC
LIMIT 20
""",
            
            "Routines with Most Dependencies": """
// Routines that call the most other routines
MATCH (r:Routine)-[:CALLS]->(dependency)
RETURN r.name as routine, r.precision as precision,
       count(DISTINCT dependency) as dependency_count
ORDER BY dependency_count DESC
LIMIT 20
""",
            
            "Cross-Precision Calls": """
// Where different precisions interact
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WHERE r1.precision <> r2.precision
  AND r1.precision IS NOT NULL 
  AND r2.precision IS NOT NULL
RETURN r1.name as caller, r1.precision as caller_precision,
       r2.name as callee, r2.precision as callee_precision
LIMIT 50
"""
        }
        
        all_queries = {**queries, **analysis_queries}
        
        guide_path = self.browser.create_guide(
            all_queries, 
            str(self.output_dir / f"lapack_visualization_guide_{datetime.now():%Y%m%d_%H%M%S}.html")
        )
        
        return guide_path


def main():
    """Run automated visualizations in Neo4j Browser."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description='Automated LAPACK graph visualization in Neo4j Browser',
        epilog='Examples:\n'
               '  %(prog)s --guide          # Create interactive HTML guide\n'
               '  %(prog)s --all            # Open all visualizations in browser\n'
               '  %(prog)s -r DGEMM         # Visualize DGEMM network\n'
               '  %(prog)s -q "MATCH..."    # Open custom query in browser',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument('-o', '--output', default='visualizations', 
                       help='Output directory for guides and reports')
    parser.add_argument('--host', default='localhost',
                       help='Neo4j server host (default: localhost)')
    parser.add_argument('--port', type=int, default=7474,
                       help='Neo4j browser port (default: 7474)')
    
    # Visualization options
    parser.add_argument('-r', '--routine', 
                       help='Visualize specific routine network')
    parser.add_argument('-d', '--depth', type=int, default=2,
                       help='Depth for routine network analysis (default: 2)')
    parser.add_argument('-q', '--query',
                       help='Open custom Cypher query in browser')
    
    # Actions
    parser.add_argument('--all', action='store_true',
                       help='Open all visualization queries in browser tabs')
    parser.add_argument('--guide', action='store_true',
                       help='Create interactive HTML guide with all queries')
    parser.add_argument('--list', action='store_true',
                       help='List available visualization queries')
    
    args = parser.parse_args()
    
    visualizer = LAPACKAutoVisualizer(args.output, args.host, args.port)
    
    if args.list:
        # List available queries
        print("\nAvailable Visualization Queries:")
        print("=" * 50)
        for i, name in enumerate(visualizer.create_visualization_queries().keys(), 1):
            print(f"{i}. {name}")
        print("\nUse --all to open all queries or -q to run a specific query")
        
    elif args.guide:
        # Create interactive guide
        print("Creating interactive visualization guide...")
        guide_path = visualizer.create_interactive_guide()
        print(f"\nGuide created: {guide_path}")
        print("\nTo use the guide:")
        print("1. Open Neo4j Browser")
        print(f"2. Drag and drop the HTML file into the browser window")
        print("3. Click on any query to run it")
        
    elif args.all:
        # Open all visualizations
        visualizer.open_all_visualizations()
        print("\nAll visualization queries opened in browser!")
        
    elif args.routine:
        # Visualize specific routine
        visualizer.visualize_routine_network(args.routine, args.depth)
        
    elif args.query:
        # Open custom query
        visualizer.open_query_in_browser(args.query, "Custom Query")
        
    else:
        # Default: show usage and create guide
        print("\nLAPACK Graph Visualization Tool")
        print("=" * 50)
        print("\nThis tool opens Cypher queries directly in Neo4j Browser for visualization.")
        print("\nQuick start:")
        print("  1. Run with --guide to create an interactive HTML guide")
        print("  2. Run with --all to open all visualizations in browser tabs")
        print("  3. Run with -r ROUTINE_NAME to visualize a specific routine")
        print("\nCreating guide by default...")
        
        guide_path = visualizer.create_interactive_guide()
        print(f"\nGuide created: {guide_path}")
        
    print(f"\nNeo4j Browser: http://{args.host}:{args.port}/")


if __name__ == '__main__':
    main()