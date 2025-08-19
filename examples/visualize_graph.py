#!/usr/bin/env python3
"""
Example: Interactive graph visualization with fortran-mapper.

This example demonstrates how to use the visualization features of fortran-mapper
to create interactive force-directed graph visualizations of Fortran code structure.

Usage:
    # Start interactive server
    python visualize_graph.py serve
    
    # Export static visualization
    python visualize_graph.py export output.html
    
    # Export centered on specific routine
    python visualize_graph.py export output.html --routine DGEMM --depth 3
"""

import sys
import argparse
from pathlib import Path
sys.path.append(str(Path(__file__).parent.parent / 'fortran-mapper' / 'src'))

from neo4j import GraphDatabase
from fortran_mapper.visualization import VisualizationServer, GraphDataGenerator


def visualize_serve(args):
    """Start the interactive visualization server."""
    # Create Neo4j connection
    auth = None
    if args.neo4j_user and args.neo4j_password:
        auth = (args.neo4j_user, args.neo4j_password)
    
    driver = GraphDatabase.driver(args.neo4j_uri, auth=auth)
    
    try:
        # Create and start server
        server = VisualizationServer(driver, host=args.host, port=args.port)
        
        print(f"🌐 Starting visualization server at http://{args.host}:{args.port}")
        print("\nFeatures:")
        print("  • Force-directed graph layout with physics simulation")
        print("  • Interactive controls for filtering nodes and relationships")
        print("  • Multiple visualization modes: Overview, Centered, Hierarchy")
        print("  • Real-time graph manipulation and exploration")
        print("  • Export to SVG or JSON formats")
        print("\nPress Ctrl+C to stop the server")
        
        server.start(auto_open=not args.no_browser)
        
    finally:
        driver.close()


def visualize_export(args):
    """Export a static visualization."""
    # Create Neo4j connection
    auth = None
    if args.neo4j_user and args.neo4j_password:
        auth = (args.neo4j_user, args.neo4j_password)
    
    driver = GraphDatabase.driver(args.neo4j_uri, auth=auth)
    
    try:
        generator = GraphDataGenerator(driver)
        server = VisualizationServer(driver)
        
        # Generate graph data based on options
        if args.routine:
            print(f"📊 Generating call hierarchy for routine: {args.routine}")
            graph_data = generator.generate_call_hierarchy(
                routine_name=args.routine,
                direction='both',
                max_depth=args.depth
            )
        elif args.center:
            print(f"📊 Generating centered graph around: {args.center}")
            try:
                center_id = int(args.center)
                graph_data = generator.generate_centered_graph(
                    center_node_id=center_id,
                    depth=args.depth
                )
            except ValueError:
                graph_data = generator.generate_centered_graph(
                    center_node_name=args.center,
                    depth=args.depth
                )
        else:
            print(f"📊 Generating overview graph (limit: {args.limit} nodes)")
            graph_data = generator.generate_overview_graph(node_limit=args.limit)
        
        # Export to file
        output_path = Path(args.output)
        server.export_static_visualization(output_path, graph_data)
        
        print(f"✅ Exported visualization to: {output_path}")
        print(f"   Nodes: {len(graph_data.nodes)}")
        print(f"   Edges: {len(graph_data.edges)}")
        print(f"\n📁 Open {output_path} in a web browser to view the visualization")
        
    finally:
        driver.close()


def main():
    parser = argparse.ArgumentParser(
        description='Interactive graph visualization for Fortran code'
    )
    
    # Neo4j connection options
    parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                       help='Neo4j URI (default: bolt://localhost:7687)')
    parser.add_argument('--neo4j-user', help='Neo4j username')
    parser.add_argument('--neo4j-password', help='Neo4j password')
    
    subparsers = parser.add_subparsers(dest='command', help='Commands')
    
    # Serve command
    serve_parser = subparsers.add_parser('serve', help='Start interactive visualization server')
    serve_parser.add_argument('--host', default='127.0.0.1', 
                             help='Host to bind to (default: 127.0.0.1)')
    serve_parser.add_argument('--port', type=int, default=8080,
                             help='Port to listen on (default: 8080)')
    serve_parser.add_argument('--no-browser', action='store_true',
                             help='Do not open browser automatically')
    
    # Export command
    export_parser = subparsers.add_parser('export', help='Export static visualization')
    export_parser.add_argument('output', help='Output HTML file')
    export_parser.add_argument('--limit', type=int, default=100,
                              help='Maximum nodes for overview (default: 100)')
    export_parser.add_argument('--routine', help='Center on specific routine (hierarchy mode)')
    export_parser.add_argument('--center', help='Center on specific node (centered mode)')
    export_parser.add_argument('--depth', type=int, default=2,
                              help='Graph depth for centered/hierarchy modes (default: 2)')
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    if args.command == 'serve':
        visualize_serve(args)
    elif args.command == 'export':
        visualize_export(args)


if __name__ == '__main__':
    main()