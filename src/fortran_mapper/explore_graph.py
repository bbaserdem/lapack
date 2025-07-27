#!/usr/bin/env python3
"""Interactive tool to explore the LAPACK computational graph in Neo4j."""

import argparse
import json
from pathlib import Path
from typing import Optional
from .neo4j_client import Neo4jClient
from .graph_visualizer import GraphVisualizer


def print_summary(visualizer: GraphVisualizer):
    """Print a comprehensive summary of the graph."""
    print("\n=== LAPACK Computational Graph Summary ===\n")
    
    summary = visualizer.get_graph_summary()
    
    print(f"Total Nodes: {summary['total_nodes']:,}")
    print(f"Total Relationships: {summary['total_relationships']:,}\n")
    
    print("Node Distribution:")
    for label, count in summary['nodes_by_label'].items():
        print(f"  {label}: {count:,}")
    
    print("\nRelationship Types:")
    for rel_type, count in summary['relationships_by_type'].items():
        print(f"  {rel_type}: {count:,}")
    
    print("\nFile Statistics:")
    file_stats = summary['file_statistics']
    print(f"  Total Files: {file_stats['total_files']:,}")
    print(f"  Avg Routines per File: {file_stats['avg_routines_per_file']:.1f}")
    print(f"  Max Routines per File: {file_stats['max_routines_per_file']}")
    
    print("\nConnectivity Metrics:")
    conn = summary['connectivity_metrics']
    print(f"  Average Outgoing Calls: {conn['avg_out_calls']:.1f}")
    print(f"  Maximum Outgoing Calls: {conn['max_out_calls']}")
    print(f"  Average Incoming Calls: {conn['avg_in_calls']:.1f}")
    print(f"  Maximum Incoming Calls: {conn['max_in_calls']}")
    print(f"  Isolated Routines: {conn['isolated_routines']:,}")
    print(f"  Leaf Routines (no outgoing): {conn['leaf_routines']:,}")
    print(f"  Root Routines (no incoming): {conn['root_routines']:,}")


def print_top_hubs(visualizer: GraphVisualizer, limit: int = 20):
    """Print the most connected routines."""
    print(f"\n=== Top {limit} Most Connected Routines ===\n")
    
    hubs = visualizer.get_top_hubs(limit)
    
    print(f"{'Routine':<20} {'Precision':<10} {'Category':<25} {'In':<8} {'Out':<8} {'Total':<8}")
    print("-" * 90)
    
    for hub in hubs:
        category = hub['category'] or 'N/A'
        if len(category) > 23:
            category = category[:20] + '...'
        print(f"{hub['routine_name']:<20} {hub['precision'] or 'N/A':<10} "
              f"{category:<25} {hub['in_calls']:<8} "
              f"{hub['out_calls']:<8} {hub['total_calls']:<8}")


def analyze_routine(visualizer: GraphVisualizer, routine_name: str, depth: int = 3):
    """Analyze a specific routine and its call chains."""
    print(f"\n=== Analysis of Routine: {routine_name} ===\n")
    
    # Get basic info
    info_query = """
        MATCH (r:Routine {name: $name})
        OPTIONAL MATCH (r)-[:DEFINED_IN]->(f:File)
        RETURN r.name as name, r.precision as precision, 
               r.category as category, r.purpose as purpose,
               f.path as file_path,
               COUNT{(r)-[:CALLS]->()} as out_calls,
               COUNT{(r)<-[:CALLS]-()} as in_calls
    """
    
    with visualizer.client.driver.session() as session:
        info_result = session.run(info_query, name=routine_name)
        info = [dict(record) for record in info_result]
    
    if not info:
        print(f"Routine '{routine_name}' not found!")
        return
    
    r = info[0]
    print(f"Name: {r['name']}")
    print(f"Precision: {r['precision'] or 'N/A'}")
    print(f"Category: {r['category'] or 'N/A'}")
    print(f"Purpose: {r['purpose'] or 'N/A'}")
    print(f"File: {r['file_path'] or 'N/A'}")
    print(f"Outgoing Calls: {r['out_calls']}")
    print(f"Incoming Calls: {r['in_calls']}")
    
    # Get call chains
    chains = visualizer.get_call_chain(routine_name, "both", depth)
    
    if chains['chains'].get('callers'):
        print(f"\nRoutines that call {routine_name} (up to depth {depth}):")
        for i, caller in enumerate(chains['chains']['callers'][:10], 1):
            chain = " -> ".join(caller['chain'])
            print(f"  {i}. {chain}")
    
    if chains['chains'].get('callees'):
        print(f"\nRoutines called by {routine_name} (up to depth {depth}):")
        for i, callee in enumerate(chains['chains']['callees'][:10], 1):
            chain = " -> ".join(callee['chain'])
            print(f"  {i}. {chain}")


def analyze_precision_patterns(visualizer: GraphVisualizer):
    """Analyze precision patterns in the graph."""
    print("\n=== Precision Analysis ===\n")
    
    patterns = visualizer.analyze_precision_patterns()
    
    print("Routines by Precision:")
    for p in patterns['routines_by_precision']:
        print(f"  {p['precision'] or 'Unknown'}: {p['count']:,} routines")
    
    print("\nPrecision Interaction Patterns:")
    print(f"{'From':<10} {'To':<10} {'Calls':<10}")
    print("-" * 30)
    
    for interaction in patterns['precision_interactions'][:20]:
        from_p = interaction['from_precision'] or 'Unknown'
        to_p = interaction['to_precision'] or 'Unknown'
        calls = interaction['call_count']
        print(f"{from_p:<10} {to_p:<10} {calls:<10}")


def analyze_categories(visualizer: GraphVisualizer):
    """Analyze category patterns in the graph."""
    print("\n=== Category Analysis ===\n")
    
    analysis = visualizer.analyze_categories()
    
    print("Routine Categories:")
    for cat in analysis['category_distribution'][:15]:
        print(f"  {cat['category']}: {cat['count']:,} routines")
    
    print("\nTop Category Interactions:")
    print(f"{'From Category':<30} {'To Category':<30} {'Calls':<10}")
    print("-" * 70)
    
    for interaction in analysis['category_interactions'][:20]:
        from_cat = interaction['from_category'][:28]
        to_cat = interaction['to_category'][:28]
        calls = interaction['call_count']
        print(f"{from_cat:<30} {to_cat:<30} {calls:<10}")


def find_file_coupling(visualizer: GraphVisualizer, limit: int = 20):
    """Analyze coupling between files."""
    print(f"\n=== Top {limit} File Couplings ===\n")
    
    couplings = visualizer.get_file_coupling(limit)
    
    print(f"{'File 1':<40} {'File 2':<40} {'Cross-file Calls':<15}")
    print("-" * 95)
    
    for coupling in couplings:
        file1 = Path(coupling['file1']).name
        file2 = Path(coupling['file2']).name
        calls = coupling['cross_file_calls']
        print(f"{file1:<40} {file2:<40} {calls:<15}")


def export_visualization_data(visualizer: GraphVisualizer, output_file: str, 
                            max_nodes: int = 500, routine_filter: Optional[str] = None):
    """Export graph data for external visualization."""
    print(f"\nExporting visualization data to {output_file}...")
    visualizer.export_for_visualization(output_file, max_nodes, routine_filter)
    print("Export complete!")


def print_cypher_queries(visualizer: GraphVisualizer):
    """Print useful Cypher queries for Neo4j Browser."""
    print("\n=== Useful Cypher Queries for Neo4j Browser ===\n")
    
    queries = visualizer.generate_visualization_queries()
    
    for name, query in queries.items():
        print(f"-- {name.replace('_', ' ').title()} --")
        print(query.strip())
        print("\n")


def main():
    parser = argparse.ArgumentParser(
        description="Explore the LAPACK computational graph in Neo4j"
    )
    
    parser.add_argument('--uri', default='bolt://localhost:7687',
                       help='Neo4j connection URI')
    parser.add_argument('--username', default='',
                       help='Neo4j username')
    parser.add_argument('--password', default='',
                       help='Neo4j password')
    
    subparsers = parser.add_subparsers(dest='command', help='Commands')
    
    # Summary command
    subparsers.add_parser('summary', help='Show graph summary statistics')
    
    # Top hubs command
    hubs_parser = subparsers.add_parser('hubs', help='Show most connected routines')
    hubs_parser.add_argument('--limit', type=int, default=20,
                            help='Number of hubs to show')
    
    # Routine analysis command
    routine_parser = subparsers.add_parser('routine', help='Analyze a specific routine')
    routine_parser.add_argument('name', help='Routine name to analyze')
    routine_parser.add_argument('--depth', type=int, default=3,
                               help='Maximum depth for call chains')
    
    # Precision analysis
    subparsers.add_parser('precision', help='Analyze precision patterns')
    
    # Category analysis
    subparsers.add_parser('categories', help='Analyze routine categories')
    
    # File coupling
    coupling_parser = subparsers.add_parser('coupling', help='Analyze file coupling')
    coupling_parser.add_argument('--limit', type=int, default=20,
                                help='Number of couplings to show')
    
    # Export for visualization
    export_parser = subparsers.add_parser('export', help='Export data for visualization')
    export_parser.add_argument('output', help='Output file path')
    export_parser.add_argument('--max-nodes', type=int, default=500,
                              help='Maximum nodes to export')
    export_parser.add_argument('--filter', help='Filter routines by name')
    
    # Cypher queries
    subparsers.add_parser('queries', help='Show useful Cypher queries')
    
    args = parser.parse_args()
    
    # Connect to Neo4j
    with Neo4jClient(args.uri, args.username, args.password) as client:
        visualizer = GraphVisualizer(client)
        
        if args.command == 'summary' or args.command is None:
            print_summary(visualizer)
        
        elif args.command == 'hubs':
            print_top_hubs(visualizer, args.limit)
        
        elif args.command == 'routine':
            analyze_routine(visualizer, args.name, args.depth)
        
        elif args.command == 'precision':
            analyze_precision_patterns(visualizer)
        
        elif args.command == 'categories':
            analyze_categories(visualizer)
        
        elif args.command == 'coupling':
            find_file_coupling(visualizer, args.limit)
        
        elif args.command == 'export':
            export_visualization_data(visualizer, args.output, 
                                    args.max_nodes, args.filter)
        
        elif args.command == 'queries':
            print_cypher_queries(visualizer)
        
        else:
            print_summary(visualizer)


if __name__ == '__main__':
    main()