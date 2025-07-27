#!/usr/bin/env python3
"""
Demo script showing how to use MCP Neo4j tools with LAPACK graph.

This example demonstrates how to:
1. Execute Cypher queries via MCP
2. Process and visualize results
3. Generate insights from the LAPACK computational graph

Note: This script assumes you have:
- Neo4j running with LAPACK data loaded
- MCP Neo4j tools configured in Claude
"""

import json
import sys
from pathlib import Path
sys.path.append(str(Path(__file__).parent.parent / 'src'))

from lapack_util.query_templates import QUERY_TEMPLATES


def print_query_results(name: str, results: list):
    """Pretty print query results."""
    print(f"\n=== {name} ===")
    if not results:
        print("No results found.")
        return
        
    if isinstance(results[0], dict):
        # Get column names
        columns = list(results[0].keys())
        
        # Calculate column widths
        widths = {}
        for col in columns:
            max_width = len(col)
            for row in results[:20]:  # Check first 20 rows
                val = str(row.get(col, ''))
                max_width = max(max_width, len(val))
            widths[col] = min(max_width + 2, 40)  # Cap at 40 chars
        
        # Print header
        header = "|".join(f" {col:<{widths[col]-2}} " for col in columns)
        print(header)
        print("-" * len(header))
        
        # Print rows
        for i, row in enumerate(results):
            if i >= 10:  # Limit output
                print(f"... and {len(results) - 10} more rows")
                break
            row_str = "|".join(f" {str(row.get(col, '')):<{widths[col]-2}} " for col in columns)
            print(row_str)
    else:
        for i, result in enumerate(results[:10]):
            print(f"{i+1}. {result}")


def demo_browser_integration():
    """Demo: Open queries directly in Neo4j Browser."""
    print("\n" + "="*60)
    print("NEO4J BROWSER INTEGRATION DEMO")
    print("="*60)
    
    from lapack_util.browser_integration import Neo4jBrowserIntegration
    
    browser = Neo4jBrowserIntegration()
    
    # Example queries to open in browser
    queries = {
        "Graph Overview": """
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WITH r1, r2, rand() as random
ORDER BY random
LIMIT 150
RETURN r1, r2
""",
        
        "Most Connected Hubs": """
MATCH (r:Routine)
WITH r, COUNT{(r)-[:CALLS]-()} as degree
ORDER BY degree DESC
LIMIT 25
MATCH (r)-[rel:CALLS]-(connected)
RETURN r, rel, connected
""",
        
        "XERBLA Error Network": """
MATCH (xerbla:Routine {name: 'XERBLA'})
MATCH (caller)-[:CALLS]->(xerbla)
WITH xerbla, caller
LIMIT 50
RETURN xerbla, caller
"""
    }
    
    print("\nYou can open these queries in Neo4j Browser:")
    for i, (name, query) in enumerate(queries.items(), 1):
        print(f"\n{i}. {name}")
        url = browser.open_with_query(query, open_browser=False)
        print(f"   URL: {url}")
    
    print("\nTo open a query, copy the URL to your browser or run:")
    print("python -m lapack_util.auto_visualize --all")


def demo_connectivity_analysis():
    """Demo: Analyze connectivity patterns."""
    print("\n" + "="*60)
    print("CONNECTIVITY ANALYSIS DEMO")
    print("="*60)
    
    queries = [
        ("Most Called Routines", QUERY_TEMPLATES["most_called_routines"]),
        ("Routines with Most Dependencies", QUERY_TEMPLATES["routines_with_most_dependencies"]),
        ("Isolated Routines (first 5)", QUERY_TEMPLATES["isolated_routines"] + " LIMIT 5"),
    ]
    
    for name, query in queries:
        print(f"\n--- {name} ---")
        print(f"Query preview: {query[:100]}...")
        print("(Execute via mcp__neo4j-lapack__read_neo4j_cypher)")


def demo_specific_routine_analysis(routine_name: str = "DGEMM"):
    """Demo: Analyze a specific routine."""
    print("\n" + "="*60)
    print(f"ROUTINE ANALYSIS DEMO: {routine_name}")
    print("="*60)
    
    # Get routine info
    info_query = f"""
        MATCH (r:Routine {{name: '{routine_name}'}})
        OPTIONAL MATCH (r)-[:DEFINED_IN]->(f:File)
        OPTIONAL MATCH (r)-[:CALLS]->(dep)
        OPTIONAL MATCH (caller)-[:CALLS]->(r)
        RETURN r.name as name, r.precision as precision, r.category as category,
               f.path as file_path,
               count(DISTINCT dep) as dependencies,
               count(DISTINCT caller) as callers
    """
    
    print(f"\nRoutine Information Query:")
    print(info_query)
    
    # Get dependencies
    from lapack_util.query_templates import get_routine_dependencies
    dep_query = get_routine_dependencies(routine_name, depth=2)
    
    print(f"\nDependencies Query (depth=2):")
    print(dep_query[:150] + "...")
    
    # Get dependents
    from lapack_util.query_templates import get_routine_dependents
    dependent_query = get_routine_dependents(routine_name, depth=2)
    
    print(f"\nDependents Query (depth=2):")
    print(dependent_query[:150] + "...")


def demo_precision_analysis():
    """Demo: Analyze precision patterns."""
    print("\n" + "="*60)
    print("PRECISION ANALYSIS DEMO")
    print("="*60)
    
    # Precision call matrix
    print("\nPrecision Call Matrix Query:")
    print(QUERY_TEMPLATES["precision_call_matrix"])
    
    # Find precision migration candidates
    from lapack_util.query_templates import get_precision_migration_candidates
    print("\nPrecision Migration Candidates Query:")
    print(get_precision_migration_candidates())


def generate_example_queries():
    """Generate a file with example queries for manual execution."""
    output_file = Path("examples/lapack_cypher_queries.txt")
    output_file.parent.mkdir(exist_ok=True)
    
    with open(output_file, 'w') as f:
        f.write("# LAPACK Graph Analysis - Example Cypher Queries\n")
        f.write("# Execute these using: mcp__neo4j-lapack__read_neo4j_cypher\n\n")
        
        for name, query in QUERY_TEMPLATES.items():
            f.write(f"## {name.replace('_', ' ').title()}\n")
            f.write(f"{query.strip()}\n\n")
        
        f.write("\n## Specific Routine Analysis Examples\n\n")
        
        for routine in ["DGEMM", "DGETRF", "DSYEV", "DGESVD"]:
            f.write(f"### {routine} Dependencies\n")
            from lapack_util.query_templates import get_routine_dependencies
            f.write(get_routine_dependencies(routine, 2) + "\n\n")
    
    print(f"\nExample queries saved to: {output_file}")


def main():
    """Run all demos."""
    print("LAPACK Neo4j Integration Demo")
    print("=============================")
    print("\nThis demo shows two ways to work with LAPACK graph queries:")
    print("1. Execute via MCP tools: mcp__neo4j-lapack__read_neo4j_cypher(query)")
    print("2. Open directly in Neo4j Browser for visualization")
    
    # Run demos
    demo_browser_integration()
    demo_connectivity_analysis()
    demo_specific_routine_analysis("DGEMM")
    demo_precision_analysis()
    
    # Generate query file
    generate_example_queries()
    
    print("\n" + "="*60)
    print("DEMO COMPLETE")
    print("="*60)
    print("\nNext steps:")
    print("1. For visualization: python -m lapack_util.auto_visualize --guide")
    print("2. For MCP queries: Use mcp__neo4j-lapack__read_neo4j_cypher(query)")
    print("3. For browser: python -m lapack_util.browser_integration --help")
    print("\nSee examples/lapack_cypher_queries.txt for all queries")


if __name__ == "__main__":
    main()