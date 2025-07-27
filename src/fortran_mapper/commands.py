"""Command implementations for fortran-mapper CLI."""

import os
import webbrowser
from pathlib import Path
from typing import List, Optional
from .parser import LapackParser
from .neo4j_client import Neo4jClient
from .data_manager import DataManager
from .visualization import VisualizationServer, DynamicGraphGenerator

def parse_command(source_paths: List[str], output: Optional[str] = None,
                 neo4j_uri: str = "bolt://localhost:7687",
                 username: str = "", password: str = "",
                 exclude_patterns: Optional[List[str]] = None) -> None:
    """Parse LAPACK source files and optionally export to Neo4j."""
    parser = LapackParser()
    
    # Parse all source paths
    for source_path in source_paths:
        print(f"Parsing {source_path}...")
        parser.parse_directory(source_path, exclude_patterns=exclude_patterns)
    
    # Show summary
    print(f"\nParsed {len(parser.subroutines)} subroutines")
    print(f"Found {sum(len(s.calls) for s in parser.subroutines.values())} call relationships")
    
    # Export to Neo4j if requested
    if output and output.lower() == "neo4j":
        print("\nExporting to Neo4j...")
        with Neo4jClient(neo4j_uri, username, password) as client:
            parser.export_to_neo4j(client.driver)
    elif output:
        # Export to file
        if output.endswith('.json'):
            parser.export_to_json(output)
        elif output.endswith('.dot'):
            parser.export_to_dot(output)
        elif output.endswith('.graphml'):
            parser.export_to_graphml(output)
        else:
            print(f"Unknown output format: {output}")

def analyze_command(routine_name: str, depth: int = 2,
                   neo4j_uri: str = "bolt://localhost:7687",
                   username: str = "", password: str = "") -> None:
    """Analyze dependencies for a specific routine."""
    with Neo4jClient(neo4j_uri, username, password) as client:
        # Get direct dependencies
        query = f"""
            MATCH (r:Routine {{name: '{routine_name}'}})
            OPTIONAL MATCH (r)-[:CALLS]->(called:Routine)
            OPTIONAL MATCH (caller:Routine)-[:CALLS]->(r)
            RETURN r.name as name,
                   collect(DISTINCT called.name) as calls,
                   collect(DISTINCT caller.name) as called_by
        """
        
        results = client.execute_query(query)
        if not results:
            print(f"Routine '{routine_name}' not found in database")
            return
        
        result = results[0]
        print(f"\nAnalysis for {routine_name}:")
        print(f"  Calls: {', '.join(result['calls']) if result['calls'] and result['calls'][0] else 'None'}")
        print(f"  Called by: {', '.join(result['called_by']) if result['called_by'] and result['called_by'][0] else 'None'}")
        
        # Get recursive dependencies if depth > 1
        if depth > 1:
            query = f"""
                MATCH path = (r:Routine {{name: '{routine_name}'}})-[:CALLS*1..{depth}]->(dep:Routine)
                RETURN DISTINCT dep.name as dependency, length(path) as distance
                ORDER BY distance, dependency
            """
            deps = client.execute_query(query)
            
            if deps:
                print(f"\nDependencies (up to depth {depth}):")
                current_level = 0
                for dep in deps:
                    if dep['distance'] != current_level:
                        current_level = dep['distance']
                        print(f"\n  Level {current_level}:")
                    print(f"    - {dep['dependency']}")

def export_command(format: str, output: str,
                  neo4j_uri: str = "bolt://localhost:7687",
                  username: str = "", password: str = "") -> None:
    """Export Neo4j graph to various formats."""
    with Neo4jClient(neo4j_uri, username, password) as client:
        if format == "cypher":
            client.export_to_cypher(output)
        elif format == "json":
            client.export_to_json(output)
        elif format == "stats":
            client.export_statistics(output)
        elif format == "graphml":
            # Export to GraphML using parser
            parser = LapackParser()
            parser.import_from_neo4j(client.driver)
            parser.export_to_graphml(output)
        elif format == "dot":
            # Export to DOT using parser
            parser = LapackParser()
            parser.import_from_neo4j(client.driver)
            parser.export_to_dot(output)
        else:
            print(f"Unknown export format: {format}")

def import_command(input_file: str,
                  neo4j_uri: str = "bolt://localhost:7687",
                  username: str = "", password: str = "",
                  clear: bool = False) -> None:
    """Import data into Neo4j from file."""
    with Neo4jClient(neo4j_uri, username, password) as client:
        if clear:
            print("Clearing existing data...")
            client.clear_database()
        
        if input_file.endswith('.cypher'):
            client.import_from_cypher(input_file)
        elif input_file.endswith('.json'):
            client.import_from_json(input_file)
        else:
            print(f"Unknown input format: {input_file}")
            return
        
        # Verify
        stats = client.verify_data()
        print(f"\nImport complete:")
        print(f"  Total nodes: {stats['total_nodes']}")
        print(f"  Total relationships: {stats['total_relationships']}")

def backup_command(backup_dir: str = "backups",
                  neo4j_uri: str = "bolt://localhost:7687",
                  username: str = "", password: str = "") -> None:
    """Create a backup of Neo4j data."""
    manager = DataManager(neo4j_uri, username, password)
    manager.create_backup(backup_dir)

def restore_command(backup_path: str,
                   neo4j_uri: str = "bolt://localhost:7687",
                   username: str = "", password: str = "") -> None:
    """Restore Neo4j data from backup."""
    manager = DataManager(neo4j_uri, username, password)
    success = manager.restore_backup(backup_path)
    if not success:
        exit(1)

def list_backups_command(backup_dir: str = "backups") -> None:
    """List available backups."""
    manager = DataManager()
    manager.list_backups(backup_dir)

def sample_data_command(output_file: str = "data/sample_lapack_graph.cypher") -> None:
    """Create sample LAPACK graph data."""
    manager = DataManager()
    manager.create_sample_data(output_file)

def query_command(query: str,
                 neo4j_uri: str = "bolt://localhost:7687",
                 username: str = "", password: str = "",
                 output: Optional[str] = None) -> None:
    """Execute a Cypher query."""
    with Neo4jClient(neo4j_uri, username, password) as client:
        results = client.execute_query(query)
        
        if output:
            # Save to file
            import json
            with open(output, 'w') as f:
                json.dump(results, f, indent=2, default=str)
            print(f"Results saved to: {output}")
        else:
            # Print to console
            for i, record in enumerate(results):
                print(f"\nRecord {i + 1}:")
                for key, value in record.items():
                    print(f"  {key}: {value}")

def stats_command(neo4j_uri: str = "bolt://localhost:7687",
                 username: str = "", password: str = "") -> None:
    """Show database statistics."""
    with Neo4jClient(neo4j_uri, username, password) as client:
        stats = client.verify_data()
        
        print("\nDatabase Statistics:")
        print(f"  Total nodes: {stats['total_nodes']}")
        print(f"  Total relationships: {stats['total_relationships']}")
        
        print("\nNodes by label:")
        for label, count in stats['nodes_by_label'].items():
            print(f"  {label}: {count}")
        
        print("\nRelationships by type:")
        for rel_type, count in stats['relationships_by_type'].items():
            print(f"  {rel_type}: {count}")

def visualize_serve_command(port: int = 8080,
                           neo4j_uri: str = "bolt://localhost:7687",
                           username: str = "", password: str = "",
                           auto_open: bool = True) -> None:
    """Start the interactive visualization web server."""
    print(f"Connecting to Neo4j at {neo4j_uri}...")
    
    with Neo4jClient(neo4j_uri, username, password) as client:
        # Test connection
        try:
            stats = client.verify_data()
            print(f"Connected! Found {stats['total_nodes']} nodes and {stats['total_relationships']} relationships")
        except Exception as e:
            print(f"Warning: Could not verify Neo4j connection: {e}")
        
        server = VisualizationServer(client.driver, port=port)
        
        if auto_open:
            # Open browser after a short delay
            import threading
            def open_browser():
                import time
                time.sleep(1)
                webbrowser.open(f"http://localhost:{port}/graph_viewer.html")
            
            threading.Thread(target=open_browser, daemon=True).start()
        
        try:
            server.start()
        except KeyboardInterrupt:
            print("\nVisualization server stopped.")

def visualize_export_command(output_dir: str,
                            neo4j_uri: str = "bolt://localhost:7687",
                            username: str = "", password: str = "") -> None:
    """Export visualization data to JSON files."""
    with Neo4jClient(neo4j_uri, username, password) as client:
        generator = DynamicGraphGenerator(client.driver)
        
        print(f"Exporting visualization data to {output_dir}/...")
        generator.export_all_graphs(output_dir)
        
        print("\nExported files:")
        output_path = Path(output_dir)
        for file in output_path.glob("*.json"):
            print(f"  - {file.name}")
        
        print(f"\nTo view the exported data, run:")
        print(f"  fortran-mapper visualize serve")

def visualize_schema_command(neo4j_uri: str = "bolt://localhost:7687",
                           username: str = "", password: str = "",
                           output: Optional[str] = None) -> None:
    """Display or export the graph schema."""
    with Neo4jClient(neo4j_uri, username, password) as client:
        generator = DynamicGraphGenerator(client.driver)
        schema = generator.discover_schema()
        
        if output:
            # Export to file
            import json
            with open(output, 'w') as f:
                json.dump(schema, f, indent=2)
            print(f"Schema exported to {output}")
        else:
            # Display schema
            print("\nGraph Schema:")
            print(f"\nNode Labels ({len(schema['node_labels'])}):")
            for label in schema['node_labels']:
                count = schema['statistics']['node_counts'].get(label, 0)
                props = schema['node_properties'].get(label, [])
                print(f"  - {label}: {count} nodes")
                if props:
                    print(f"    Properties: {', '.join(props[:5])}")
                    if len(props) > 5:
                        print(f"    ... and {len(props) - 5} more")
            
            print(f"\nRelationship Types ({len(schema['relationship_types'])}):")
            for rel_type in schema['relationship_types']:
                count = schema['statistics']['relationship_counts'].get(rel_type, 0)
                print(f"  - {rel_type}: {count} relationships")