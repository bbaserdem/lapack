"""Command-line interface for LAPACK utilities."""

import argparse
import sys
from . import commands

def main():
    """Main entry point for lapack-util CLI."""
    parser = argparse.ArgumentParser(
        prog='lapack-util',
        description='LAPACK computational graph analysis utilities'
    )
    
    # Global options
    parser.add_argument('--neo4j-uri', default='bolt://localhost:7687',
                       help='Neo4j connection URI')
    parser.add_argument('--username', default='',
                       help='Neo4j username')
    parser.add_argument('--password', default='',
                       help='Neo4j password')
    
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
    
    args = parser.parse_args()
    
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
        
    except KeyboardInterrupt:
        print("\nOperation cancelled.")
        return 1
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    
    return 0

if __name__ == '__main__':
    sys.exit(main())