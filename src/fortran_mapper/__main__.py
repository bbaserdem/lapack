"""Command-line interface for Fortran Mapper."""

import argparse
import sys
import logging
from . import commands

def main():
    """Main entry point for fortran-mapper CLI."""
    parser = argparse.ArgumentParser(
        prog='fortran-mapper',
        description='A flexible, extensible Fortran code mapper with hook-based customization'
    )
    
    # Global options
    parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                       help='Neo4j connection URI')
    parser.add_argument('--username', default='',
                       help='Neo4j username')
    parser.add_argument('--password', default='',
                       help='Neo4j password')
    parser.add_argument('--debug', action='store_true',
                       help='Enable debug logging')
    parser.add_argument('--log-file',
                       help='Log to file instead of stderr')
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # Parse command
    parse_parser = subparsers.add_parser('parse', 
                                        help='Parse LAPACK source files')
    parse_parser.add_argument('source', nargs='+',
                             help='Source directories or files to parse')
    parse_parser.add_argument('-o', '--output',
                             help='Output destination (neo4j, *.json, *.dot, *.graphml)')
    parse_parser.add_argument('--exclude', nargs='*',
                             help='Patterns to exclude from parsing')
    
    # Analyze command
    analyze_parser = subparsers.add_parser('analyze',
                                          help='Analyze routine dependencies')
    analyze_parser.add_argument('routine',
                               help='Routine name to analyze')
    analyze_parser.add_argument('-d', '--depth', type=int, default=2,
                               help='Dependency depth to analyze')
    
    # Export command
    export_parser = subparsers.add_parser('export',
                                         help='Export graph data')
    export_parser.add_argument('format', 
                              choices=['cypher', 'json', 'stats', 'graphml', 'dot'],
                              help='Export format')
    export_parser.add_argument('output',
                              help='Output file path')
    
    # Import command
    import_parser = subparsers.add_parser('import',
                                         help='Import graph data')
    import_parser.add_argument('input',
                              help='Input file (*.cypher or *.json)')
    import_parser.add_argument('--clear', action='store_true',
                              help='Clear existing data before import')
    
    # Backup command
    backup_parser = subparsers.add_parser('backup',
                                         help='Create data backup')
    backup_parser.add_argument('--backup-dir', default='backups',
                              help='Backup directory')
    
    # Restore command
    restore_parser = subparsers.add_parser('restore',
                                          help='Restore from backup')
    restore_parser.add_argument('backup',
                               help='Backup path or directory')
    
    # List backups command
    list_parser = subparsers.add_parser('list-backups',
                                       help='List available backups')
    list_parser.add_argument('--backup-dir', default='backups',
                            help='Backup directory')
    
    # Sample data command
    sample_parser = subparsers.add_parser('sample',
                                         help='Create sample data')
    sample_parser.add_argument('-o', '--output',
                              default='data/sample_lapack_graph.cypher',
                              help='Output file')
    
    # Query command
    query_parser = subparsers.add_parser('query',
                                        help='Execute Cypher query')
    query_parser.add_argument('query',
                             help='Cypher query to execute')
    query_parser.add_argument('-o', '--output',
                             help='Save results to file')
    
    # Stats command
    stats_parser = subparsers.add_parser('stats',
                                        help='Show database statistics')
    
    # Explore command
    explore_parser = subparsers.add_parser('explore',
                                          help='Interactive graph exploration')
    explore_subparsers = explore_parser.add_subparsers(dest='explore_command', 
                                                       help='Exploration commands')
    
    # Explore subcommands
    explore_subparsers.add_parser('summary', help='Show graph summary')
    
    hubs_parser = explore_subparsers.add_parser('hubs', help='Show most connected routines')
    hubs_parser.add_argument('--limit', type=int, default=20, help='Number of hubs to show')
    
    routine_parser = explore_subparsers.add_parser('routine', help='Analyze specific routine')
    routine_parser.add_argument('name', help='Routine name')
    routine_parser.add_argument('--depth', type=int, default=3, help='Call chain depth')
    
    explore_subparsers.add_parser('precision', help='Analyze precision patterns')
    explore_subparsers.add_parser('categories', help='Analyze routine categories')
    
    coupling_parser = explore_subparsers.add_parser('coupling', help='Analyze file coupling')
    coupling_parser.add_argument('--limit', type=int, default=20, help='Number of couplings')
    
    viz_parser = explore_subparsers.add_parser('visualize', help='Export for visualization')
    viz_parser.add_argument('output', help='Output file path')
    viz_parser.add_argument('--max-nodes', type=int, default=500, help='Maximum nodes')
    viz_parser.add_argument('--filter', help='Filter routines by name')
    
    explore_subparsers.add_parser('queries', help='Show useful Cypher queries')
    
    # Neo4j server management commands
    neo4j_parser = subparsers.add_parser('neo4j',
                                        help='Manage Neo4j server')
    neo4j_parser.add_argument('action',
                             choices=['start', 'stop', 'status', 'console'],
                             help='Neo4j server action')
    neo4j_parser.add_argument('--data-dir',
                             help='Neo4j data directory (default: ./neo4j-data)')
    
    # Visualize command with subcommands
    visualize_parser = subparsers.add_parser('visualize',
                                           help='Interactive graph visualization')
    visualize_subparsers = visualize_parser.add_subparsers(dest='visualize_command',
                                                          help='Visualization commands')
    
    # Visualize serve subcommand
    serve_parser = visualize_subparsers.add_parser('serve',
                                                  help='Start visualization web server')
    serve_parser.add_argument('-p', '--port', type=int, default=8080,
                             help='Port to run server on (default: 8080)')
    serve_parser.add_argument('--no-open', action='store_true',
                             help='Do not automatically open browser')
    
    # Visualize export subcommand
    export_viz_parser = visualize_subparsers.add_parser('export',
                                                       help='Export visualization data')
    export_viz_parser.add_argument('output_dir',
                                  help='Directory to export visualization data')
    
    # Visualize schema subcommand
    schema_parser = visualize_subparsers.add_parser('schema',
                                                   help='Display or export graph schema')
    schema_parser.add_argument('-o', '--output',
                              help='Export schema to file (JSON format)')
    
    args = parser.parse_args()
    
    # Configure logging
    log_level = logging.DEBUG if args.debug else logging.INFO
    log_format = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    
    # Suppress Neo4j notification warnings for cleaner output
    neo4j_logger = logging.getLogger('neo4j.notifications')
    neo4j_logger.setLevel(logging.ERROR)
    
    if args.log_file:
        logging.basicConfig(
            level=log_level,
            format=log_format,
            filename=args.log_file,
            filemode='a'
        )
    else:
        logging.basicConfig(
            level=log_level,
            format=log_format,
            stream=sys.stderr
        )
    
    if not args.command:
        parser.print_help()
        return 0
    
    # Common Neo4j arguments
    neo4j_args = {
        'neo4j_uri': args.neo4j_uri,
        'username': args.username,
        'password': args.password
    }
    
    # Execute commands
    try:
        if args.command == 'parse':
            commands.parse_command(
                args.source, args.output,
                exclude_patterns=args.exclude,
                **neo4j_args
            )
        
        elif args.command == 'analyze':
            commands.analyze_command(
                args.routine, args.depth,
                **neo4j_args
            )
        
        elif args.command == 'export':
            commands.export_command(
                args.format, args.output,
                **neo4j_args
            )
        
        elif args.command == 'import':
            commands.import_command(
                args.input, clear=args.clear,
                **neo4j_args
            )
        
        elif args.command == 'backup':
            commands.backup_command(
                args.backup_dir,
                **neo4j_args
            )
        
        elif args.command == 'restore':
            commands.restore_command(
                args.backup,
                **neo4j_args
            )
        
        elif args.command == 'list-backups':
            commands.list_backups_command(args.backup_dir)
        
        elif args.command == 'sample':
            commands.sample_data_command(args.output)
        
        elif args.command == 'query':
            commands.query_command(
                args.query,
                output=args.output,
                **neo4j_args
            )
        
        elif args.command == 'stats':
            commands.stats_command(**neo4j_args)
        
        elif args.command == 'explore':
            from .graph_visualizer import GraphVisualizer
            from .neo4j_client import Neo4jClient
            
            with Neo4jClient(args.neo4j_uri, args.username, args.password) as client:
                visualizer = GraphVisualizer(client)
                
                if args.explore_command == 'summary' or args.explore_command is None:
                    from .explore_graph import print_summary
                    print_summary(visualizer)
                
                elif args.explore_command == 'hubs':
                    from .explore_graph import print_top_hubs
                    print_top_hubs(visualizer, args.limit)
                
                elif args.explore_command == 'routine':
                    from .explore_graph import analyze_routine
                    analyze_routine(visualizer, args.name, args.depth)
                
                elif args.explore_command == 'precision':
                    from .explore_graph import analyze_precision_patterns
                    analyze_precision_patterns(visualizer)
                
                elif args.explore_command == 'categories':
                    from .explore_graph import analyze_categories
                    analyze_categories(visualizer)
                
                elif args.explore_command == 'coupling':
                    from .explore_graph import find_file_coupling
                    find_file_coupling(visualizer, args.limit)
                
                elif args.explore_command == 'visualize':
                    from .explore_graph import export_visualization_data
                    export_visualization_data(visualizer, args.output, 
                                            args.max_nodes, args.filter)
                
                elif args.explore_command == 'queries':
                    from .explore_graph import print_cypher_queries
                    print_cypher_queries(visualizer)
        
        elif args.command == 'neo4j':
            from .neo4j_server import neo4j_server_command
            return neo4j_server_command(args.action, args.data_dir, verbose=args.debug)
        
        elif args.command == 'visualize':
            if args.visualize_command == 'serve' or args.visualize_command is None:
                commands.visualize_serve_command(
                    port=args.port if hasattr(args, 'port') else 8080,
                    auto_open=not getattr(args, 'no_open', False),
                    **neo4j_args
                )
            
            elif args.visualize_command == 'export':
                commands.visualize_export_command(
                    args.output_dir,
                    **neo4j_args
                )
            
            elif args.visualize_command == 'schema':
                commands.visualize_schema_command(
                    output=args.output if hasattr(args, 'output') else None,
                    **neo4j_args
                )
        
    except KeyboardInterrupt:
        print("\nOperation cancelled.")
        return 1
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    
    return 0

if __name__ == '__main__':
    sys.exit(main())