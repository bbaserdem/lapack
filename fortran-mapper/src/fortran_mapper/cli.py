#!/usr/bin/env python3
"""Command-line interface for fortran-mapper."""

import argparse
import sys
from pathlib import Path
import logging
import json
from typing import Optional, Any

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


def get_neo4j_driver(uri: str, user: Optional[str] = None, password: Optional[str] = None):
    """Get Neo4j driver instance."""
    try:
        from neo4j import GraphDatabase
        auth = (user, password) if user else None
        return GraphDatabase.driver(uri, auth=auth)
    except ImportError:
        print("❌ Neo4j driver not available. Install with: uv pip install neo4j")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Failed to connect to Neo4j: {e}")
        sys.exit(1)


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
    
    # Global options
    parser.add_argument('--debug', action='store_true',
                       help='Enable debug logging')
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # Parse command
    parse_parser = subparsers.add_parser('parse', help='Parse Fortran code')
    parse_parser.add_argument('directories', nargs='+', type=Path, 
                             help='Directory or directories to parse')
    parse_parser.add_argument('--extensions', default='.f,.f90,.f77,.F,.F90',
                             help='Comma-separated file extensions (default: .f,.f90,.f77,.F,.F90)')
    parse_parser.add_argument('--exclude', nargs='*', default=[],
                             help='Patterns to exclude from parsing')
    parse_parser.add_argument('--lapack', action='store_true',
                             help='Enable LAPACK-specific hooks (deprecated, use --hooks lapack)')
    parse_parser.add_argument('--hooks', nargs='*', default=[],
                             help='Hook packages to load (e.g., --hooks lapack custom)')
    parse_parser.add_argument('--verbose', '-v', action='store_true',
                             help='Enable verbose logging')
    
    # Output format options
    output_group = parse_parser.add_argument_group('Output formats')
    output_group.add_argument('--json', type=Path, help='Export to JSON file')
    output_group.add_argument('--dot', type=Path, help='Export to DOT file')
    output_group.add_argument('--graphml', type=Path, help='Export to GraphML file')
    output_group.add_argument('--call-graph-only', action='store_true',
                             help='Export only call graph (for DOT format)')
    output_group.add_argument('-o', '--output', choices=['neo4j'], 
                             help='Export directly to output format')
    
    # Neo4j options
    neo4j_group = parse_parser.add_argument_group('Neo4j export')
    neo4j_group.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                            help='Neo4j database URI (default: bolt://localhost:7687)')
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
    
    # List hooks command
    list_hooks_parser = subparsers.add_parser('list-hooks', help='List available hook packages')
    list_hooks_parser.add_argument('--installed', action='store_true',
                                  help='Show only installed hooks')
    
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
    
    # Analyze command
    analyze_parser = subparsers.add_parser('analyze', help='Analyze routine dependencies')
    analyze_parser.add_argument('routine', help='Routine name to analyze')
    analyze_parser.add_argument('-d', '--depth', type=int, default=2,
                               help='Maximum depth to traverse (default: 2)')
    analyze_parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                               help='Neo4j database URI (default: bolt://localhost:7687)')
    analyze_parser.add_argument('--neo4j-user', help='Neo4j username')
    analyze_parser.add_argument('--neo4j-password', help='Neo4j password')
    
    # Query command
    query_parser = subparsers.add_parser('query', help='Execute Cypher query')
    query_parser.add_argument('query', help='Cypher query to execute')
    query_parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                             help='Neo4j database URI (default: bolt://localhost:7687)')
    query_parser.add_argument('--neo4j-user', help='Neo4j username')
    query_parser.add_argument('--neo4j-password', help='Neo4j password')
    query_parser.add_argument('--format', choices=['table', 'json', 'csv'], default='table',
                             help='Output format (default: table)')
    
    # Export command
    export_parser = subparsers.add_parser('export', help='Export data from Neo4j')
    export_parser.add_argument('format', choices=['json', 'cypher', 'graphml', 'stats'],
                              help='Export format')
    export_parser.add_argument('output', type=Path, help='Output file path')
    export_parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                              help='Neo4j database URI (default: bolt://localhost:7687)')
    export_parser.add_argument('--neo4j-user', help='Neo4j username')
    export_parser.add_argument('--neo4j-password', help='Neo4j password')
    
    # Import command
    import_parser = subparsers.add_parser('import', help='Import data to Neo4j')
    import_parser.add_argument('file', type=Path, help='File to import')
    import_parser.add_argument('--clear', action='store_true',
                              help='Clear existing data before import')
    import_parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                              help='Neo4j database URI (default: bolt://localhost:7687)')
    import_parser.add_argument('--neo4j-user', help='Neo4j username')
    import_parser.add_argument('--neo4j-password', help='Neo4j password')
    
    # Explore command
    explore_parser = subparsers.add_parser('explore', help='Explore graph data')
    explore_subparsers = explore_parser.add_subparsers(dest='explore_action', help='Explore actions')
    
    # Summary subcommand
    summary_parser = explore_subparsers.add_parser('summary', help='Show graph summary')
    summary_parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                               help='Neo4j database URI')
    summary_parser.add_argument('--neo4j-user', help='Neo4j username')
    summary_parser.add_argument('--neo4j-password', help='Neo4j password')
    
    # Hubs subcommand
    hubs_parser = explore_subparsers.add_parser('hubs', help='Find hub routines')
    hubs_parser.add_argument('--limit', type=int, default=10,
                            help='Number of hubs to show (default: 10)')
    hubs_parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                            help='Neo4j database URI')
    hubs_parser.add_argument('--neo4j-user', help='Neo4j username')
    hubs_parser.add_argument('--neo4j-password', help='Neo4j password')
    
    # Routine subcommand
    routine_parser = explore_subparsers.add_parser('routine', help='Explore specific routine')
    routine_parser.add_argument('name', help='Routine name')
    routine_parser.add_argument('--depth', type=int, default=2,
                               help='Exploration depth (default: 2)')
    routine_parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                               help='Neo4j database URI')
    routine_parser.add_argument('--neo4j-user', help='Neo4j username')
    routine_parser.add_argument('--neo4j-password', help='Neo4j password')
    
    # Precision subcommand
    precision_parser = explore_subparsers.add_parser('precision', help='Analyze precision distribution')
    precision_parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                                 help='Neo4j database URI')
    precision_parser.add_argument('--neo4j-user', help='Neo4j username')
    precision_parser.add_argument('--neo4j-password', help='Neo4j password')
    
    # Categories subcommand
    categories_parser = explore_subparsers.add_parser('categories', help='Analyze operation categories')
    categories_parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                                  help='Neo4j database URI')
    categories_parser.add_argument('--neo4j-user', help='Neo4j username')
    categories_parser.add_argument('--neo4j-password', help='Neo4j password')
    
    return parser


def discover_available_hooks() -> list[str]:
    """Discover available hooks in the hooks/ directory.
    
    Returns:
        List of available hook names
    """
    hooks_dir = Path(__file__).parent.parent.parent / 'hooks'
    available_hooks = []
    
    if hooks_dir.exists():
        for hook_dir in hooks_dir.iterdir():
            if hook_dir.is_dir() and (hook_dir / 'pyproject.toml').exists():
                available_hooks.append(hook_dir.name)
    
    return available_hooks


def load_hook_package(hook_name: str, mapper: FortranMapper) -> bool:
    """Load a hook package dynamically.
    
    Args:
        hook_name: Name of the hook (e.g., 'lapack')
        mapper: FortranMapper instance to register hooks with
        
    Returns:
        True if hook loaded successfully, False otherwise
    """
    # Map of short names to package names
    hook_packages = {
        'lapack': 'fortran_mapper_hooks_lapack',
        # Add more mappings as needed
    }
    
    package_name = hook_packages.get(hook_name, f'fortran_mapper_hooks_{hook_name}')
    
    try:
        # Try to import the hook package
        hook_module = __import__(package_name, fromlist=['*'])
        
        # Look for enricher and creator classes
        enricher_found = False
        creator_found = False
        
        for attr_name in dir(hook_module):
            attr = getattr(hook_module, attr_name)
            if (isinstance(attr, type) and 
                hasattr(attr, '__name__') and 
                'Enricher' in attr.__name__):
                mapper.register_hook("node_enricher", attr())
                enricher_found = True
            elif (isinstance(attr, type) and 
                  hasattr(attr, '__name__') and 
                  'Creator' in attr.__name__):
                mapper.register_hook("node_creator", attr())
                creator_found = True
        
        if enricher_found or creator_found:
            print(f"✅ Loaded {hook_name} hooks")
            return True
        else:
            print(f"⚠️  No hooks found in {package_name}")
            return False
            
    except ImportError as e:
        print(f"❌ Failed to load {hook_name} hooks: {e}")
        print(f"   Install with: uv pip install {package_name}")
        print(f"   Or for development: cd hooks/{hook_name} && uv pip install -e .")
        return False


def cmd_parse(args):
    """Handle parse command."""
    setup_logging(args.verbose or args.debug)
    
    # Create mapper
    mapper = FortranMapper()
    
    # Ensure args.hooks is always a list
    if not hasattr(args, 'hooks') or args.hooks is None:
        args.hooks = []
    elif not isinstance(args.hooks, list):
        args.hooks = list(args.hooks)
    
    # Handle deprecated --lapack flag
    if hasattr(args, 'lapack') and args.lapack and 'lapack' not in args.hooks:
        args.hooks.append('lapack')
    
    # Load requested hooks
    if args.hooks:
        print(f"🔌 Loading hooks: {', '.join(args.hooks)}")
        for hook_name in args.hooks:
            load_hook_package(hook_name, mapper)
    
    # Parse extensions
    extensions = [ext.strip() for ext in args.extensions.split(',')]
    
    directories_str = ', '.join(str(d) for d in args.directories)
    print(f"🔍 Parsing {directories_str} with extensions: {extensions}")
    
    try:
        # Parse directories
        from .core.graph import Graph
        graph = Graph()
        
        for directory in args.directories:
            print(f"  Parsing {directory}...")
            dir_graph = mapper.parse_directory(directory, extensions)
            # Merge graphs - use add_node to properly update indices
            for node_id, node in dir_graph.nodes.items():
                graph.add_node(node)
            for rel in dir_graph.relationships:
                graph.add_relationship(rel)
        
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
        
        if args.neo4j_uri or args.output == 'neo4j':
            try:
                from neo4j import GraphDatabase
                uri = args.neo4j_uri if args.neo4j_uri else 'bolt://localhost:7687'
                driver = GraphDatabase.driver(
                    uri,
                    auth=(args.neo4j_user, args.neo4j_password) if args.neo4j_user else None
                )
                mapper.export_to_neo4j(graph, driver, args.neo4j_clear)
                print(f"📁 Exported to Neo4j: {uri}")
                driver.close()
                exported = True
            except ImportError:
                print("❌ Neo4j driver not available. Install with: uv pip install neo4j")
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


def cmd_analyze(args):
    """Handle analyze command - analyze routine dependencies."""
    driver = get_neo4j_driver(args.neo4j_uri, args.neo4j_user, args.neo4j_password)
    
    try:
        with driver.session() as session:
            # Query for routine and its dependencies
            query = """
            MATCH path = (r:Routine {name: $routine})-[:CALLS*0..%d]->(dep:Routine)
            RETURN DISTINCT dep.name as name, 
                   dep.precision as precision,
                   dep.category as category,
                   length(path) as depth
            ORDER BY depth, name
            """ % args.depth
            
            result = session.run(query, routine=args.routine)
            records = list(result)
            
            if not records:
                print(f"❌ Routine '{args.routine}' not found")
                return 1
            
            print(f"\n📊 Dependency analysis for {args.routine} (max depth: {args.depth})")
            print("=" * 70)
            
            current_depth = -1
            for record in records:
                if record['depth'] != current_depth:
                    current_depth = record['depth']
                    if current_depth > 0:
                        print(f"\n{'  ' * (current_depth-1)}Level {current_depth}:")
                
                indent = '  ' * current_depth
                precision = record['precision'] or 'N/A'
                category = record['category'] or 'N/A'
                
                if current_depth == 0:
                    print(f"{record['name']} [precision: {precision}, category: {category}]")
                else:
                    print(f"{indent}├─ {record['name']} [precision: {precision}, category: {category}]")
            
            print(f"\nTotal dependencies: {len(records) - 1}")
            
        return 0
        
    except Exception as e:
        print(f"❌ Analysis failed: {e}")
        return 1
    finally:
        driver.close()


def cmd_query(args):
    """Handle query command - execute Cypher query."""
    driver = get_neo4j_driver(args.neo4j_uri, args.neo4j_user, args.neo4j_password)
    
    try:
        with driver.session() as session:
            result = session.run(args.query)
            records = list(result)
            
            if not records:
                print("No results found")
                return 0
            
            if args.format == 'json':
                # JSON output
                data = [dict(record) for record in records]
                print(json.dumps(data, indent=2, default=str))
                
            elif args.format == 'csv':
                # CSV output
                if records:
                    keys = records[0].keys()
                    print(','.join(keys))
                    for record in records:
                        values = [str(record[k]) for k in keys]
                        print(','.join(values))
                        
            else:  # table format
                # Table output
                if records:
                    keys = records[0].keys()
                    
                    # Calculate column widths
                    widths = {}
                    for key in keys:
                        widths[key] = max(len(key), 
                                         max(len(str(record[key])) for record in records))
                    
                    # Print header
                    header = ' | '.join(key.ljust(widths[key]) for key in keys)
                    print(header)
                    print('-' * len(header))
                    
                    # Print rows
                    for record in records:
                        row = ' | '.join(str(record[key]).ljust(widths[key]) for key in keys)
                        print(row)
                    
                    print(f"\n({len(records)} rows)")
            
        return 0
        
    except Exception as e:
        print(f"❌ Query failed: {e}")
        return 1
    finally:
        driver.close()


def cmd_export(args):
    """Handle export command - export data from Neo4j."""
    driver = get_neo4j_driver(args.neo4j_uri, args.neo4j_user, args.neo4j_password)
    
    try:
        if args.format == 'json':
            # Export as JSON
            with driver.session() as session:
                # Get all nodes
                nodes_result = session.run("""
                    MATCH (n)
                    RETURN labels(n) as labels, properties(n) as props, id(n) as id
                """)
                nodes = []
                for record in nodes_result:
                    node = {
                        'id': record['id'],
                        'labels': record['labels'],
                        'properties': dict(record['props'])
                    }
                    nodes.append(node)
                
                # Get all relationships
                rels_result = session.run("""
                    MATCH (a)-[r]->(b)
                    RETURN id(a) as from_id, id(b) as to_id, 
                           type(r) as type, properties(r) as props
                """)
                relationships = []
                for record in rels_result:
                    rel = {
                        'from_id': record['from_id'],
                        'to_id': record['to_id'],
                        'type': record['type'],
                        'properties': dict(record['props'])
                    }
                    relationships.append(rel)
                
                data = {
                    'nodes': nodes,
                    'relationships': relationships
                }
                
                with open(args.output, 'w') as f:
                    json.dump(data, f, indent=2, default=str)
                    
        elif args.format == 'cypher':
            # Export as Cypher statements
            from .exporters.neo4j import Neo4jExporter
            exporter = Neo4jExporter(driver)
            
            # Get graph data from Neo4j
            from .core.graph import Graph
            graph = Graph()
            
            with driver.session() as session:
                # Fetch nodes
                nodes_result = session.run("""
                    MATCH (n:Routine)
                    RETURN n.name as name, properties(n) as props
                """)
                for record in nodes_result:
                    # Create node in graph (simplified)
                    graph.add_node(record['name'], NodeType.ROUTINE, record['props'])
                
                # Export to Cypher
                exporter.export_cypher_statements(graph, args.output)
                
        elif args.format == 'graphml':
            # Export as GraphML
            print("❌ GraphML export from Neo4j not yet implemented")
            return 1
            
        elif args.format == 'stats':
            # Export statistics
            with driver.session() as session:
                stats = {}
                
                # Count nodes by label
                labels_result = session.run("""
                    MATCH (n)
                    RETURN labels(n)[0] as label, count(n) as count
                    ORDER BY count DESC
                """)
                stats['nodes_by_label'] = {r['label']: r['count'] for r in labels_result}
                
                # Count relationships by type
                rels_result = session.run("""
                    MATCH ()-[r]->()
                    RETURN type(r) as type, count(r) as count
                    ORDER BY count DESC
                """)
                stats['relationships_by_type'] = {r['type']: r['count'] for r in rels_result}
                
                # Precision distribution
                precision_result = session.run("""
                    MATCH (r:Routine)
                    WHERE r.precision IS NOT NULL
                    RETURN r.precision as precision, count(r) as count
                    ORDER BY count DESC
                """)
                stats['precision_distribution'] = {r['precision']: r['count'] for r in precision_result}
                
                # Category distribution
                category_result = session.run("""
                    MATCH (r:Routine)
                    WHERE r.category IS NOT NULL
                    RETURN r.category as category, count(r) as count
                    ORDER BY count DESC
                    LIMIT 20
                """)
                stats['top_categories'] = {r['category']: r['count'] for r in category_result}
                
                with open(args.output, 'w') as f:
                    f.write("LAPACK Graph Statistics\n")
                    f.write("=" * 50 + "\n\n")
                    
                    f.write("Nodes by Type:\n")
                    for label, count in stats['nodes_by_label'].items():
                        f.write(f"  {label}: {count}\n")
                    
                    f.write("\nRelationships by Type:\n")
                    for rel_type, count in stats['relationships_by_type'].items():
                        f.write(f"  {rel_type}: {count}\n")
                    
                    f.write("\nPrecision Distribution:\n")
                    for precision, count in stats['precision_distribution'].items():
                        f.write(f"  {precision}: {count}\n")
                    
                    f.write("\nTop Categories:\n")
                    for category, count in stats['top_categories'].items():
                        f.write(f"  {category}: {count}\n")
        
        print(f"✅ Exported to {args.output}")
        return 0
        
    except Exception as e:
        print(f"❌ Export failed: {e}")
        return 1
    finally:
        driver.close()


def cmd_import(args):
    """Handle import command - import data to Neo4j."""
    driver = get_neo4j_driver(args.neo4j_uri, args.neo4j_user, args.neo4j_password)
    
    try:
        if args.clear:
            print("🗑️  Clearing existing data...")
            with driver.session() as session:
                session.run("MATCH (n) DETACH DELETE n")
        
        if args.file.suffix == '.cypher':
            # Import Cypher statements
            print(f"📁 Importing from {args.file}...")
            with open(args.file, 'r') as f:
                cypher_content = f.read()
            
            # Split by semicolons and execute each statement
            statements = [s.strip() for s in cypher_content.split(';') if s.strip()]
            
            with driver.session() as session:
                for i, statement in enumerate(statements):
                    if statement:
                        try:
                            session.run(statement)
                            if (i + 1) % 100 == 0:
                                print(f"  Executed {i + 1}/{len(statements)} statements...")
                        except Exception as e:
                            print(f"  Warning: Statement {i + 1} failed: {e}")
            
            print(f"✅ Imported {len(statements)} statements")
            
        elif args.file.suffix == '.json':
            # Import JSON (simplified)
            print("❌ JSON import not yet implemented")
            return 1
            
        else:
            print(f"❌ Unsupported file format: {args.file.suffix}")
            return 1
            
        return 0
        
    except Exception as e:
        print(f"❌ Import failed: {e}")
        return 1
    finally:
        driver.close()


def cmd_explore(args):
    """Handle explore command - explore graph data."""
    if not args.explore_action:
        print("❌ No explore action specified. Use: summary, hubs, routine, precision, or categories")
        return 1
    
    driver = get_neo4j_driver(args.neo4j_uri, args.neo4j_user, args.neo4j_password)
    
    try:
        with driver.session() as session:
            if args.explore_action == 'summary':
                # Show graph summary
                print("\n📊 Graph Summary")
                print("=" * 50)
                
                # Total nodes
                total_nodes = session.run("MATCH (n) RETURN count(n) as count").single()['count']
                print(f"Total nodes: {total_nodes}")
                
                # Nodes by type
                print("\nNodes by type:")
                nodes_by_type = session.run("""
                    MATCH (n)
                    RETURN labels(n)[0] as label, count(n) as count
                    ORDER BY count DESC
                """)
                for record in nodes_by_type:
                    print(f"  {record['label']}: {record['count']}")
                
                # Total relationships
                total_rels = session.run("MATCH ()-[r]->() RETURN count(r) as count").single()['count']
                print(f"\nTotal relationships: {total_rels}")
                
                # Relationships by type
                print("\nRelationships by type:")
                rels_by_type = session.run("""
                    MATCH ()-[r]->()
                    RETURN type(r) as type, count(r) as count
                    ORDER BY count DESC
                """)
                for record in rels_by_type:
                    print(f"  {record['type']}: {record['count']}")
                
            elif args.explore_action == 'hubs':
                # Find hub routines
                print(f"\n📊 Top {args.limit} Hub Routines")
                print("=" * 50)
                
                hubs = session.run("""
                    MATCH (r:Routine)
                    WITH r, size((r)<-[:CALLS]-()) as in_degree,
                         size((r)-[:CALLS]->()) as out_degree
                    RETURN r.name as name, in_degree, out_degree, 
                           in_degree + out_degree as total_degree
                    ORDER BY total_degree DESC
                    LIMIT $limit
                """, limit=args.limit)
                
                for i, record in enumerate(hubs, 1):
                    print(f"{i}. {record['name']}")
                    print(f"   In-degree: {record['in_degree']}, Out-degree: {record['out_degree']}, Total: {record['total_degree']}")
                
            elif args.explore_action == 'routine':
                # Explore specific routine
                print(f"\n📊 Exploring routine: {args.name}")
                print("=" * 50)
                
                # Get routine info
                info = session.run("""
                    MATCH (r:Routine {name: $name})
                    RETURN r
                """, name=args.name).single()
                
                if not info:
                    print(f"❌ Routine '{args.name}' not found")
                    return 1
                
                routine = info['r']
                print(f"Name: {routine['name']}")
                print(f"Precision: {routine.get('precision', 'N/A')}")
                print(f"Category: {routine.get('category', 'N/A')}")
                print(f"File: {routine.get('file_path', 'N/A')}")
                print(f"Line: {routine.get('line_number', 'N/A')}")
                
                # Get calls
                print(f"\nCalls (depth {args.depth}):")
                calls = session.run("""
                    MATCH path = (r:Routine {name: $name})-[:CALLS*1..%d]->(dep:Routine)
                    RETURN DISTINCT dep.name as name, length(path) as depth
                    ORDER BY depth, name
                """ % args.depth, name=args.name)
                
                for record in calls:
                    print(f"  {'  ' * (record['depth']-1)}├─ {record['name']}")
                
                # Get called by
                print(f"\nCalled by:")
                called_by = session.run("""
                    MATCH (caller:Routine)-[:CALLS]->(r:Routine {name: $name})
                    RETURN caller.name as name
                    ORDER BY name
                """, name=args.name)
                
                for record in called_by:
                    print(f"  ├─ {record['name']}")
                
            elif args.explore_action == 'precision':
                # Analyze precision distribution
                print("\n📊 Precision Distribution")
                print("=" * 50)
                
                precision_dist = session.run("""
                    MATCH (r:Routine)
                    RETURN r.precision as precision, count(r) as count
                    ORDER BY count DESC
                """)
                
                total = 0
                for record in precision_dist:
                    precision = record['precision'] or 'None'
                    count = record['count']
                    total += count
                    print(f"{precision}: {count}")
                
                print(f"\nTotal: {total}")
                
            elif args.explore_action == 'categories':
                # Analyze operation categories
                print("\n📊 Operation Categories")
                print("=" * 50)
                
                categories = session.run("""
                    MATCH (r:Routine)
                    WHERE r.category IS NOT NULL
                    RETURN r.category as category, count(r) as count
                    ORDER BY count DESC
                    LIMIT 30
                """)
                
                for record in categories:
                    print(f"{record['category']}: {record['count']}")
                    
        return 0
        
    except Exception as e:
        print(f"❌ Explore failed: {e}")
        return 1
    finally:
        driver.close()


def cmd_list_hooks(args):
    """Handle list-hooks command - list available hook packages."""
    print("🔌 Available Hook Packages")
    print("=" * 50)
    
    # Discover hooks in hooks/ directory
    available_hooks = discover_available_hooks()
    
    # Known hook mappings
    known_hooks = {
        'lapack': {
            'package': 'fortran_mapper_hooks_lapack',
            'description': 'LAPACK-specific analysis hooks'
        }
    }
    
    for hook_name in available_hooks:
        hook_info = known_hooks.get(hook_name, {
            'package': f'fortran_mapper_hooks_{hook_name}',
            'description': f'{hook_name.title()} hooks'
        })
        
        # Check if installed
        try:
            __import__(hook_info['package'])
            status = "✅ Installed"
        except ImportError:
            status = "❌ Not installed"
            if args.installed:
                continue
        
        print(f"\n{hook_name}:")
        print(f"  Package: {hook_info['package']}")
        print(f"  Description: {hook_info['description']}")
        print(f"  Status: {status}")
        
        if status == "❌ Not installed":
            print(f"  Install: cd hooks/{hook_name} && uv pip install -e .")
    
    print(f"\n💡 Usage: fortran-mapper parse <dir> --hooks {' '.join(available_hooks)}")
    return 0


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
    elif args.command == 'list-hooks':
        return cmd_list_hooks(args)
    elif args.command == 'neo4j':
        return cmd_neo4j(args)
    elif args.command == 'analyze':
        return cmd_analyze(args)
    elif args.command == 'query':
        return cmd_query(args)
    elif args.command == 'export':
        return cmd_export(args)
    elif args.command == 'import':
        return cmd_import(args)
    elif args.command == 'explore':
        return cmd_explore(args)
    else:
        print(f"❌ Unknown command: {args.command}")
        return 1


if __name__ == '__main__':
    sys.exit(main())