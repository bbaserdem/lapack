"""Neo4j export functionality."""

from typing import Any, Optional
from pathlib import Path

from ..core.graph import Graph


class Neo4jExporter:
    """Export graph to Neo4j database."""
    
    def __init__(self, driver: Optional[Any] = None):
        self.driver = driver
    
    def export(self, graph: Graph, driver: Optional[Any] = None, 
               clear_existing: bool = True) -> None:
        """Export graph to Neo4j database."""
        if driver is None:
            driver = self.driver
        
        if driver is None:
            raise ValueError("Neo4j driver must be provided")
        
        with driver.session() as session:
            if clear_existing:
                print("  Clearing existing database...")
                self._clear_database(session)
            
            print("  Creating constraints and indexes...")
            self._create_constraints(session)
            
            print(f"  Creating {len(graph.nodes)} nodes in batches...")
            self._create_nodes(session, graph)
            
            print(f"  Creating {len(graph.relationships)} relationships in batches...")
            self._create_relationships(session, graph)
            
            print("  Neo4j export complete!")
    
    def _clear_database(self, session: Any) -> None:
        """Clear all nodes and relationships."""
        session.run("MATCH (n) DETACH DELETE n")
    
    def _create_constraints(self, session: Any) -> None:
        """Create unique constraints and indexes."""
        constraints = [
            "CREATE CONSTRAINT routine_name IF NOT EXISTS FOR (r:Routine) REQUIRE r.name IS UNIQUE",
            "CREATE CONSTRAINT file_path IF NOT EXISTS FOR (f:File) REQUIRE f.path IS UNIQUE",
            "CREATE CONSTRAINT module_name IF NOT EXISTS FOR (m:Module) REQUIRE m.name IS UNIQUE",
        ]
        
        indexes = [
            "CREATE INDEX routine_type IF NOT EXISTS FOR (r:Routine) ON (r.routine_type)",
            "CREATE INDEX file_directory IF NOT EXISTS FOR (f:File) ON (f.directory)",
        ]
        
        for constraint in constraints:
            try:
                session.run(constraint)
            except Exception:
                pass  # Constraint might already exist
        
        for index in indexes:
            try:
                session.run(index)
            except Exception:
                pass  # Index might already exist
    
    def _create_nodes(self, session: Any, graph: Graph) -> None:
        """Create all nodes in the graph using batch transactions."""
        # Group nodes by label for batch processing
        nodes_by_label = {}
        for node in graph.nodes.values():
            label = node.node_type.value
            if hasattr(node, 'properties') and 'custom_type' in node.properties:
                label = node.properties['custom_type']
            
            if label not in nodes_by_label:
                nodes_by_label[label] = []
            
            # Prepare properties
            props = {"name": node.name}
            if hasattr(node, 'properties'):
                for key, value in node.properties.items():
                    if value is not None:
                        # Convert lists to strings for Neo4j
                        if isinstance(value, list):
                            props[key] = str(value)
                        else:
                            props[key] = value
            
            nodes_by_label[label].append(props)
        
        # Batch create nodes by label
        for label, nodes_data in nodes_by_label.items():
            # Process in batches of 1000
            batch_size = 1000
            for i in range(0, len(nodes_data), batch_size):
                batch = nodes_data[i:i + batch_size]
                cypher = f"""
                UNWIND $batch AS props
                MERGE (n:{label} {{name: props.name}})
                SET n += props
                """
                session.run(cypher, batch=batch)
    
    def _create_relationships(self, session: Any, graph: Graph) -> None:
        """Create all relationships in the graph using batch transactions."""
        # Group relationships by type for batch processing
        rels_by_type = {}
        
        for rel in graph.relationships:
            rel_type = rel.rel_type.value
            if rel_type not in rels_by_type:
                rels_by_type[rel_type] = []
            
            # Get node information
            from_node = graph.get_node(rel.from_node_id)
            to_node = graph.get_node(rel.to_node_id)
            
            if from_node and to_node:
                # Get labels
                from_label = from_node.node_type.value
                to_label = to_node.node_type.value
                
                if hasattr(from_node, 'properties') and 'custom_type' in from_node.properties:
                    from_label = from_node.properties['custom_type']
                if hasattr(to_node, 'properties') and 'custom_type' in to_node.properties:
                    to_label = to_node.properties['custom_type']
                
                rel_data = {
                    'from_name': from_node.name,
                    'to_name': to_node.name,
                    'from_label': from_label,
                    'to_label': to_label,
                    'props': rel.properties or {}
                }
                rels_by_type[rel_type].append(rel_data)
        
        # Batch create relationships by type
        for rel_type, rels_data in rels_by_type.items():
            # Process in batches of 1000
            batch_size = 1000
            for i in range(0, len(rels_data), batch_size):
                batch = rels_data[i:i + batch_size]
                
                # Group by label pairs for more efficient matching
                by_labels = {}
                for rel_data in batch:
                    key = (rel_data['from_label'], rel_data['to_label'])
                    if key not in by_labels:
                        by_labels[key] = []
                    by_labels[key].append(rel_data)
                
                # Create relationships for each label pair
                for (from_label, to_label), label_batch in by_labels.items():
                    cypher = f"""
                    UNWIND $batch AS rel
                    MATCH (from:{from_label} {{name: rel.from_name}})
                    MATCH (to:{to_label} {{name: rel.to_name}})
                    MERGE (from)-[r:{rel_type}]->(to)
                    SET r += rel.props
                    """
                    session.run(cypher, batch=label_batch)
    
    def export_cypher_statements(self, graph: Graph, output_path: Path) -> None:
        """Export as Cypher statements to a file."""
        statements = []
        
        # Clear and constraints
        statements.extend([
            "MATCH (n) DETACH DELETE n;",
            "",
            "CREATE CONSTRAINT routine_name IF NOT EXISTS FOR (r:Routine) REQUIRE r.name IS UNIQUE;",
            "CREATE CONSTRAINT file_path IF NOT EXISTS FOR (f:File) REQUIRE f.path IS UNIQUE;",
            "CREATE CONSTRAINT module_name IF NOT EXISTS FOR (m:Module) REQUIRE m.name IS UNIQUE;",
            "",
        ])
        
        # Node creation statements
        for node in graph.nodes.values():
            props = {"name": node.name}
            if hasattr(node, 'properties'):
                props.update(node.properties)
            
            # Format properties for Cypher
            props_str = ", ".join([f"{k}: {repr(v)}" for k, v in props.items() if v is not None])
            
            label = node.node_type.value
            if hasattr(node, 'properties') and 'custom_type' in node.properties:
                label = node.properties['custom_type']
            
            statements.append(f"CREATE (:{label} {{{props_str}}});")
        
        statements.append("")
        
        # Relationship creation statements
        for rel in graph.relationships:
            from_node = graph.get_node(rel.from_node_id)
            to_node = graph.get_node(rel.to_node_id)
            
            if from_node and to_node:
                props_str = ""
                if rel.properties:
                    props_str = " {" + ", ".join([f"{k}: {repr(v)}" for k, v in rel.properties.items()]) + "}"
                
                statements.append(
                    f"MATCH (from {{name: {repr(from_node.name)}}}), "
                    f"(to {{name: {repr(to_node.name)}}}) "
                    f"CREATE (from)-[:{rel.rel_type.value}{props_str}]->(to);"
                )
        
        # Write to file
        with open(output_path, 'w') as f:
            f.write("\n".join(statements))