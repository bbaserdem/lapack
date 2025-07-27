"""Neo4j client for LAPACK graph operations."""

import json
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any
from neo4j import GraphDatabase

class Neo4jClient:
    """Client for managing LAPACK graphs in Neo4j."""
    
    def __init__(self, uri: str = "bolt://localhost:7687", 
                 username: str = "", password: str = ""):
        """Initialize Neo4j client.
        
        Args:
            uri: Neo4j connection URI
            username: Neo4j username (empty for no auth)
            password: Neo4j password (empty for no auth)
        """
        auth = (username, password) if username else None
        self.driver = GraphDatabase.driver(uri, auth=auth)
        self.driver.verify_connectivity()
    
    def close(self):
        """Close the Neo4j connection."""
        if self.driver:
            self.driver.close()
    
    def __enter__(self):
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()
    
    def export_to_cypher(self, output_file: str) -> None:
        """Export the entire graph as Cypher statements."""
        print(f"Exporting to Cypher format: {output_file}")
        
        with self.driver.session() as session:
            # Get all nodes
            nodes_result = session.run("""
                MATCH (n)
                RETURN labels(n) as labels, properties(n) as props, id(n) as id
                ORDER BY id
            """)
            
            # Get all relationships
            rels_result = session.run("""
                MATCH ()-[r]->()
                RETURN type(r) as type, properties(r) as props, 
                       id(startNode(r)) as start_id, id(endNode(r)) as end_id
                ORDER BY id(r)
            """)
            
            with open(output_file, 'w') as f:
                f.write("// LAPACK Dependency Graph Export\n")
                f.write(f"// Exported on: {datetime.now().isoformat()}\n\n")
                
                # Write constraints
                f.write("// Create constraints\n")
                f.write("CREATE CONSTRAINT routine_name IF NOT EXISTS FOR (r:Routine) REQUIRE r.name IS UNIQUE;\n")
                f.write("CREATE CONSTRAINT file_path IF NOT EXISTS FOR (f:File) REQUIRE f.path IS UNIQUE;\n\n")
                
                # Write nodes
                f.write("// Create nodes\n")
                node_mapping = {}
                for i, record in enumerate(nodes_result):
                    labels = ":".join(record["labels"])
                    props = record["props"]
                    node_mapping[record["id"]] = f"n{i}"
                    
                    props_str = ", ".join([f"{k}: {repr(v)}" for k, v in props.items()])
                    f.write(f"CREATE (n{i}:{labels} {{{props_str}}})\n")
                
                f.write("\n// Create relationships\n")
                # Write relationships
                for record in rels_result:
                    rel_type = record["type"]
                    props = record["props"]
                    start_var = node_mapping[record["start_id"]]
                    end_var = node_mapping[record["end_id"]]
                    
                    if props:
                        props_str = " {" + ", ".join([f"{k}: {repr(v)}" for k, v in props.items()]) + "}"
                    else:
                        props_str = ""
                    
                    f.write(f"CREATE ({start_var})-[:{rel_type}{props_str}]->({end_var})\n")
        
        print(f"Export complete: {output_file}")
    
    def export_to_json(self, output_file: str) -> None:
        """Export the graph as JSON for easy processing."""
        print(f"Exporting to JSON format: {output_file}")
        
        with self.driver.session() as session:
            # Export nodes
            nodes_result = session.run("""
                MATCH (n)
                RETURN labels(n) as labels, properties(n) as props
            """)
            
            # Export relationships
            rels_result = session.run("""
                MATCH (a)-[r]->(b)
                RETURN properties(a) as source_props, labels(a) as source_labels,
                       type(r) as rel_type, properties(r) as rel_props,
                       properties(b) as target_props, labels(b) as target_labels
            """)
            
            data = {
                "metadata": {
                    "exported_at": datetime.now().isoformat(),
                    "format": "json",
                    "version": "1.0"
                },
                "nodes": [],
                "relationships": []
            }
            
            # Collect nodes
            seen_nodes = set()
            for record in nodes_result:
                node_key = (tuple(record["labels"]), tuple(sorted(record["props"].items())))
                if node_key not in seen_nodes:
                    seen_nodes.add(node_key)
                    data["nodes"].append({
                        "labels": record["labels"],
                        "properties": record["props"]
                    })
            
            # Collect relationships
            for record in rels_result:
                data["relationships"].append({
                    "source": {
                        "labels": record["source_labels"],
                        "properties": record["source_props"]
                    },
                    "relationship": {
                        "type": record["rel_type"],
                        "properties": record["rel_props"]
                    },
                    "target": {
                        "labels": record["target_labels"],
                        "properties": record["target_props"]
                    }
                })
            
            with open(output_file, 'w') as f:
                json.dump(data, f, indent=2, default=str)
        
        print(f"Export complete: {output_file}")
    
    def export_statistics(self, output_file: str) -> None:
        """Export graph statistics and summary."""
        print(f"Exporting statistics: {output_file}")
        
        with self.driver.session() as session:
            stats = {}
            
            # Count nodes by label
            node_counts = session.run("""
                MATCH (n)
                RETURN labels(n) as labels, count(*) as count
                ORDER BY count DESC
            """)
            stats["node_counts"] = {"+".join(record["labels"]): record["count"] 
                                   for record in node_counts}
            
            # Count relationships by type
            rel_counts = session.run("""
                MATCH ()-[r]->()
                RETURN type(r) as type, count(*) as count
                ORDER BY count DESC
            """)
            stats["relationship_counts"] = {record["type"]: record["count"] 
                                          for record in rel_counts}
            
            # Get top routines by connections
            top_routines = session.run("""
                MATCH (r:Routine)
                WITH r, size((r)-[:CALLS]->()) as out_degree,
                     size((r)<-[:CALLS]-()) as in_degree
                RETURN r.name as name, out_degree, in_degree, 
                       out_degree + in_degree as total_degree
                ORDER BY total_degree DESC
                LIMIT 20
            """)
            stats["top_connected_routines"] = [dict(record) for record in top_routines]
            
            # Get summary
            summary = session.run("""
                MATCH (n)
                WITH count(n) as node_count
                MATCH ()-[r]->()
                WITH node_count, count(r) as rel_count
                RETURN node_count, rel_count
            """).single()
            
            stats["summary"] = {
                "total_nodes": summary["node_count"],
                "total_relationships": summary["rel_count"],
                "exported_at": datetime.now().isoformat()
            }
            
            with open(output_file, 'w') as f:
                json.dump(stats, f, indent=2)
        
        print(f"Statistics exported: {output_file}")
    
    def import_from_cypher(self, input_file: str) -> None:
        """Import graph from Cypher statements."""
        print(f"Importing from Cypher file: {input_file}")
        
        with open(input_file, 'r') as f:
            statements = []
            current_statement = []
            
            for line in f:
                line = line.strip()
                # Skip comments and empty lines
                if not line or line.startswith("//"):
                    continue
                
                current_statement.append(line)
                # Execute when we have a complete statement
                if line.endswith(";") or line.endswith(")"):
                    statement = " ".join(current_statement)
                    statements.append(statement)
                    current_statement = []
        
        # Execute statements in batches
        with self.driver.session() as session:
            success_count = 0
            error_count = 0
            
            for i, statement in enumerate(statements):
                try:
                    session.run(statement)
                    success_count += 1
                    if (i + 1) % 100 == 0:
                        print(f"Processed {i + 1}/{len(statements)} statements...")
                except Exception as e:
                    error_count += 1
                    print(f"Error executing statement {i + 1}: {e}")
                    print(f"Statement: {statement[:100]}...")
        
        print(f"\nImport complete: {success_count} successful, {error_count} errors")
    
    def import_from_json(self, input_file: str) -> None:
        """Import graph from JSON format."""
        print(f"Importing from JSON file: {input_file}")
        
        with open(input_file, 'r') as f:
            data = json.load(f)
        
        with self.driver.session() as session:
            # Clear existing data (optional)
            print("Clearing existing data...")
            session.run("MATCH (n) DETACH DELETE n")
            
            # Create constraints
            print("Creating constraints...")
            session.run("CREATE CONSTRAINT routine_name IF NOT EXISTS FOR (r:Routine) REQUIRE r.name IS UNIQUE")
            session.run("CREATE CONSTRAINT file_path IF NOT EXISTS FOR (f:File) REQUIRE f.path IS UNIQUE")
            
            # Import nodes
            print(f"Importing {len(data['nodes'])} nodes...")
            for node in data['nodes']:
                labels = ":".join(node['labels'])
                props = node['properties']
                
                # Build the Cypher query
                props_str = ", ".join([f"{k}: ${k}" for k in props.keys()])
                query = f"CREATE (n:{labels} {{{props_str}}})"
                
                session.run(query, **props)
            
            # Import relationships
            print(f"Importing {len(data['relationships'])} relationships...")
            for rel in data['relationships']:
                source = rel['source']
                target = rel['target']
                rel_type = rel['relationship']['type']
                rel_props = rel['relationship']['properties'] or {}
                
                # Match nodes based on their unique properties
                source_label = source['labels'][0]
                target_label = target['labels'][0]
                
                # Determine matching property
                source_match = "name" if source_label == "Routine" else "path"
                target_match = "name" if target_label == "Routine" else "path"
                
                # Build relationship query
                rel_props_str = ""
                if rel_props:
                    rel_props_str = " {" + ", ".join([f"{k}: ${k}" for k in rel_props.keys()]) + "}"
                
                query = f"""
                    MATCH (a:{source_label} {{{source_match}: $source_id}})
                    MATCH (b:{target_label} {{{target_match}: $target_id}})
                    CREATE (a)-[:{rel_type}{rel_props_str}]->(b)
                """
                
                params = {
                    'source_id': source['properties'].get(source_match),
                    'target_id': target['properties'].get(target_match),
                    **rel_props
                }
                
                try:
                    session.run(query, params)
                except Exception as e:
                    print(f"Error creating relationship: {e}")
        
        print("Import from JSON complete!")
    
    def verify_data(self) -> Dict[str, Any]:
        """Verify the imported data and return statistics."""
        print("\nVerifying data...")
        
        with self.driver.session() as session:
            # Get counts
            result = session.run("""
                MATCH (n)
                WITH count(n) as node_count
                MATCH ()-[r]->()
                WITH node_count, count(r) as rel_count
                RETURN node_count, rel_count
            """).single()
            
            stats = {
                "total_nodes": result['node_count'],
                "total_relationships": result['rel_count'],
                "nodes_by_label": {},
                "relationships_by_type": {}
            }
            
            # Get label counts
            label_counts = session.run("""
                MATCH (n)
                RETURN labels(n) as labels, count(*) as count
                ORDER BY count DESC
            """)
            
            for record in label_counts:
                label_key = "+".join(record['labels'])
                stats["nodes_by_label"][label_key] = record['count']
            
            # Get relationship counts
            rel_counts = session.run("""
                MATCH ()-[r]->()
                RETURN type(r) as type, count(*) as count
                ORDER BY count DESC
            """)
            
            for record in rel_counts:
                stats["relationships_by_type"][record['type']] = record['count']
            
            return stats
    
    def clear_database(self) -> None:
        """Clear all data from the database."""
        with self.driver.session() as session:
            session.run("MATCH (n) DETACH DELETE n")
            print("Database cleared.")
    
    def execute_query(self, query: str) -> List[Dict[str, Any]]:
        """Execute a Cypher query and return results."""
        with self.driver.session() as session:
            result = session.run(query)
            return [dict(record) for record in result]