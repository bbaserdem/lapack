"""
Connectome CLI - Command-line interface for LAPACK connectome analysis.
"""

import argparse
import sys
from pathlib import Path

# Import fortran-mapper functionality
from fortran_mapper import FortranMapper
from fortran_mapper.adapters.lapack import LapackNodeEnricher, LapackNodeCreator


def main():
    """Main CLI entry point for connectome."""
    parser = argparse.ArgumentParser(
        description="LAPACK computational connectome analysis",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Analyze LAPACK source and export to JSON
  connectome analyze /path/to/lapack/SRC --json output.json
  
  # Generate connectome visualization
  connectome visualize /path/to/lapack/SRC --neo4j
  
  # Export computational graph
  connectome export /path/to/lapack/SRC --dot graph.dot
        """
    )
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # Analyze command
    analyze_parser = subparsers.add_parser('analyze', help='Analyze LAPACK computational structure')
    analyze_parser.add_argument('directory', help='LAPACK source directory')
    analyze_parser.add_argument('--json', help='Export to JSON file')
    analyze_parser.add_argument('--dot', help='Export to DOT file')
    analyze_parser.add_argument('--graphml', help='Export to GraphML file')
    analyze_parser.add_argument('--verbose', '-v', action='store_true', help='Verbose output')
    
    # Visualize command
    viz_parser = subparsers.add_parser('visualize', help='Generate connectome visualizations')
    viz_parser.add_argument('directory', help='LAPACK source directory')
    viz_parser.add_argument('--neo4j', action='store_true', help='Launch Neo4j visualization')
    viz_parser.add_argument('--neo4j-uri', default='bolt://localhost:7687', help='Neo4j URI')
    
    # Export command
    export_parser = subparsers.add_parser('export', help='Export computational graphs')
    export_parser.add_argument('directory', help='LAPACK source directory')
    export_parser.add_argument('--dot', help='Export to DOT format')
    export_parser.add_argument('--json', help='Export to JSON format')
    export_parser.add_argument('--graphml', help='Export to GraphML format')
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return 1
        
    # Set up fortran-mapper with LAPACK hooks
    mapper = FortranMapper()
    mapper.register_hook("node_enricher", LapackNodeEnricher())
    mapper.register_hook("node_creator", LapackNodeCreator())
    
    try:
        if args.command == 'analyze':
            return handle_analyze(mapper, args)
        elif args.command == 'visualize':
            return handle_visualize(mapper, args)
        elif args.command == 'export':
            return handle_export(mapper, args)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    
    return 0


def handle_analyze(mapper, args):
    """Handle the analyze command."""
    print(f"🔍 Analyzing LAPACK computational structure in {args.directory}")
    
    graph = mapper.parse_directory(args.directory)
    
    # Print basic statistics
    routine_nodes = graph.get_nodes_by_type("Routine")
    file_nodes = graph.get_nodes_by_type("File")
    relationships = graph.get_all_relationships()
    
    print(f"📊 Found {len(routine_nodes)} routines")
    print(f"📁 Found {len(file_nodes)} files") 
    print(f"🔗 Created {len(relationships)} relationships")
    
    # Export if requested
    if args.json:
        mapper.export_to_json(graph, args.json)
        print(f"📄 Exported to JSON: {args.json}")
        
    if args.dot:
        mapper.export_to_dot(graph, args.dot)
        print(f"🎯 Exported to DOT: {args.dot}")
        
    if args.graphml:
        mapper.export_to_graphml(graph, args.graphml)
        print(f"📈 Exported to GraphML: {args.graphml}")
    
    return 0


def handle_visualize(mapper, args):
    """Handle the visualize command."""
    print(f"🎨 Generating connectome visualization for {args.directory}")
    
    graph = mapper.parse_directory(args.directory)
    
    if args.neo4j:
        print(f"🔗 Connecting to Neo4j at {args.neo4j_uri}")
        # TODO: Implement Neo4j visualization
        print("Neo4j visualization not yet implemented")
    
    return 0


def handle_export(mapper, args):
    """Handle the export command."""
    print(f"📤 Exporting computational graph from {args.directory}")
    
    graph = mapper.parse_directory(args.directory)
    
    if args.json:
        mapper.export_to_json(graph, args.json)
        print(f"📄 Exported to JSON: {args.json}")
        
    if args.dot:
        mapper.export_to_dot(graph, args.dot)
        print(f"🎯 Exported to DOT: {args.dot}")
        
    if args.graphml:
        mapper.export_to_graphml(graph, args.graphml)
        print(f"📈 Exported to GraphML: {args.graphml}")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
