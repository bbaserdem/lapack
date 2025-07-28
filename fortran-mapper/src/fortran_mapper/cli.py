#!/usr/bin/env python3
"""Command-line interface for fortran-mapper."""

import argparse
import sys
from pathlib import Path
import logging

from .core.parser import FortranMapper
# LAPACK hooks are now in a separate package
try:
    from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator
    LAPACK_HOOKS_AVAILABLE = True
except ImportError:
    LAPACK_HOOKS_AVAILABLE = False
from .core.nodes import NodeType


def setup_logging(verbose: bool = False):
    """Setup logging configuration."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )


def create_parser():
    """Create command-line argument parser."""
    parser = argparse.ArgumentParser(
        description="Generic Fortran code mapper with extensible hook system",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Parse directory with LAPACK hooks and export to JSON
  fortran-mapper parse /path/to/fortran --lapack --json output.json
  
  # Export only call graph to DOT format
  fortran-mapper parse /path/to/fortran --dot output.dot --call-graph-only
  
  # Parse with custom extensions
  fortran-mapper parse /path/to/fortran --extensions .f95,.F95 --json output.json
        """
    )
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # Parse command
    parse_parser = subparsers.add_parser('parse', help='Parse Fortran code')
    parse_parser.add_argument('directory', type=Path, help='Directory to parse')
    parse_parser.add_argument('--extensions', default='.f,.f90,.f77,.F,.F90',
                             help='Comma-separated file extensions (default: .f,.f90,.f77,.F,.F90)')
    parse_parser.add_argument('--lapack', action='store_true',
                             help='Enable LAPACK-specific hooks')
    parse_parser.add_argument('--verbose', '-v', action='store_true',
                             help='Enable verbose logging')
    
    # Output format options
    output_group = parse_parser.add_argument_group('Output formats')
    output_group.add_argument('--json', type=Path, help='Export to JSON file')
    output_group.add_argument('--dot', type=Path, help='Export to DOT file')
    output_group.add_argument('--graphml', type=Path, help='Export to GraphML file')
    output_group.add_argument('--call-graph-only', action='store_true',
                             help='Export only call graph (for DOT format)')
    
    # Neo4j options
    neo4j_group = parse_parser.add_argument_group('Neo4j export')
    neo4j_group.add_argument('--neo4j-uri', help='Neo4j database URI')
    neo4j_group.add_argument('--neo4j-user', help='Neo4j username')
    neo4j_group.add_argument('--neo4j-password', help='Neo4j password')
    neo4j_group.add_argument('--neo4j-clear', action='store_true',
                            help='Clear existing data before import')
    
    # Stats command
    stats_parser = subparsers.add_parser('stats', help='Show statistics about parsed code')
    stats_parser.add_argument('json_file', type=Path, help='JSON file to analyze')
    
    # Test command
    test_parser = subparsers.add_parser('test', help='Run built-in tests')
    test_parser.add_argument('--verbose', '-v', action='store_true',
                           help='Enable verbose output')
    
    # Neo4j server management commands
    neo4j_parser = subparsers.add_parser('neo4j', help='Neo4j server management')
    neo4j_subparsers = neo4j_parser.add_subparsers(dest='neo4j_action', help='Neo4j actions')
    
    # Start command
    start_parser = neo4j_subparsers.add_parser('start', help='Start Neo4j server')
    start_parser.add_argument('--data-dir', help='Neo4j data directory (default: ./neo4j-data)')
    
    # Stop command
    stop_parser = neo4j_subparsers.add_parser('stop', help='Stop Neo4j server')
    stop_parser.add_argument('--data-dir', help='Neo4j data directory (default: ./neo4j-data)')
    
    # Status command
    status_parser = neo4j_subparsers.add_parser('status', help='Check Neo4j server status')
    status_parser.add_argument('--data-dir', help='Neo4j data directory (default: ./neo4j-data)')
    status_parser.add_argument('--verbose', '-v', action='store_true',
                             help='Show detailed process information')
    
    # Console command
    console_parser = neo4j_subparsers.add_parser('console', help='Start Neo4j in console mode (foreground)')
    console_parser.add_argument('--data-dir', help='Neo4j data directory (default: ./neo4j-data)')
    
    return parser


def cmd_parse(args):
    """Handle parse command."""
    setup_logging(args.verbose)
    
    # Create mapper
    mapper = FortranMapper()
    
    # Register LAPACK hooks if requested
    if args.lapack:
        if not LAPACK_HOOKS_AVAILABLE:
            print("❌ LAPACK hooks not available. Install with: uv pip install fortran-mapper-hooks-lapack")
            print("   Or for development: cd hooks/lapack && uv pip install -e .")
            return 1
        mapper.register_hook("node_enricher", LapackNodeEnricher())
        mapper.register_hook("node_creator", LapackNodeCreator())
        print("✅ Enabled LAPACK-specific hooks")
    
    # Parse extensions
    extensions = [ext.strip() for ext in args.extensions.split(',')]
    
    print(f"🔍 Parsing {args.directory} with extensions: {extensions}")
    
    try:
        # Parse directory
        graph = mapper.parse_directory(args.directory, extensions)
        
        # Show statistics
        routines = graph.get_nodes_by_type(NodeType.ROUTINE)
        files = graph.get_nodes_by_type(NodeType.FILE)
        custom_nodes = graph.get_nodes_by_type(NodeType.CUSTOM)
        
        print(f"📊 Found {len(routines)} routines, {len(files)} files, {len(custom_nodes)} custom nodes")
        print(f"🔗 Created {len(graph.relationships)} relationships")
        
        # Export to requested formats
        exported = False
        
        if args.json:
            mapper.export_to_json(graph, args.json)
            print(f"📁 Exported to JSON: {args.json}")
            exported = True
        
        if args.dot:
            if args.call_graph_only:
                from .exporters.dot import DOTExporter
                exporter = DOTExporter()
                exporter.export_call_graph_only(graph, args.dot)
                print(f"📁 Exported call graph to DOT: {args.dot}")
            else:
                mapper.export_to_dot(graph, args.dot)
                print(f"📁 Exported to DOT: {args.dot}")
            exported = True
        
        if args.graphml:
            mapper.export_to_graphml(graph, args.graphml)
            print(f"📁 Exported to GraphML: {args.graphml}")
            exported = True
        
        if args.neo4j_uri:
            try:
                from neo4j import GraphDatabase
                driver = GraphDatabase.driver(
                    args.neo4j_uri,
                    auth=(args.neo4j_user, args.neo4j_password) if args.neo4j_user else None
                )
                mapper.export_to_neo4j(graph, driver, args.neo4j_clear)
                print(f"📁 Exported to Neo4j: {args.neo4j_uri}")
                driver.close()
                exported = True
            except ImportError:
                print("❌ Neo4j driver not available. Install with: pip install neo4j")
                return 1
        
        if not exported:
            print("ℹ️  No export format specified. Use --json, --dot, --graphml, or --neo4j-uri")
            print("   Example: fortran-mapper parse /path/to/fortran --json output.json")
        
        return 0
        
    except Exception as e:
        print(f"❌ Parse failed: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


def cmd_stats(args):
    """Handle stats command."""
    try:
        import json
        
        with open(args.json_file, 'r') as f:
            data = json.load(f)
        
        stats = data.get('statistics', {})
        nodes = data.get('nodes', [])
        relationships = data.get('relationships', [])
        
        print(f"📊 Statistics for {args.json_file}")
        print("=" * 50)
        
        # Node statistics
        node_types = {}
        for node in nodes:
            node_type = node.get('type', 'unknown')
            node_types[node_type] = node_types.get(node_type, 0) + 1
        
        print("Nodes by type:")
        for node_type, count in sorted(node_types.items()):
            print(f"  {node_type}: {count}")
        
        # Relationship statistics
        rel_types = {}
        for rel in relationships:
            rel_type = rel.get('type', 'unknown')
            rel_types[rel_type] = rel_types.get(rel_type, 0) + 1
        
        print("\nRelationships by type:")
        for rel_type, count in sorted(rel_types.items()):
            print(f"  {rel_type}: {count}")
        
        # Custom statistics
        if stats:
            print(f"\nCustom statistics:")
            for key, value in stats.items():
                print(f"  {key}: {value}")
        
        return 0
        
    except Exception as e:
        print(f"❌ Stats failed: {e}")
        return 1


def cmd_test(args):
    """Handle test command."""
    try:
        # Import and run the test script
        import sys
        from pathlib import Path
        
        # Add the test script to path
        test_script_path = Path(__file__).parent.parent.parent / "test_fortran_mapper.py"
        
        if not test_script_path.exists():
            print(f"❌ Test script not found: {test_script_path}")
            return 1
        
        # Import and run test
        sys.path.insert(0, str(test_script_path.parent))
        import test_fortran_mapper
        
        success = test_fortran_mapper.main()
        return 0 if success else 1
        
    except Exception as e:
        print(f"❌ Test failed: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


def cmd_neo4j(args):
    """Handle neo4j server management commands."""
    try:
        # Import neo4j server management module
        from .neo4j_server import neo4j_server_command
        
        if not args.neo4j_action:
            print("❌ No action specified. Use: start, stop, status, or console")
            return 1
        
        return neo4j_server_command(
            args.neo4j_action,
            data_dir=args.data_dir,
            verbose=getattr(args, 'verbose', False)
        )
        
    except ImportError as e:
        print(f"❌ Failed to import neo4j_server module: {e}")
        print("   Make sure psutil is installed: uv pip install psutil")
        return 1
    except Exception as e:
        print(f"❌ Neo4j command failed: {e}")
        return 1


def main():
    """Main entry point."""
    parser = create_parser()
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return 1
    
    # Dispatch to command handlers
    if args.command == 'parse':
        return cmd_parse(args)
    elif args.command == 'stats':
        return cmd_stats(args)
    elif args.command == 'test':
        return cmd_test(args)
    elif args.command == 'neo4j':
        return cmd_neo4j(args)
    else:
        print(f"❌ Unknown command: {args.command}")
        return 1


if __name__ == '__main__':
    sys.exit(main())