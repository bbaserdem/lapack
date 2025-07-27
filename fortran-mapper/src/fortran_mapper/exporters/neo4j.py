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
                self._clear_database(session)
            
            self._create_constraints(session)
            self._create_nodes(session, graph)
            self._create_relationships(session, graph)
    
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
        """Create all nodes in the graph."""
        for node in graph.nodes.values():
            # Prepare properties for Cypher
            props = {"name": node.name}
            
            if hasattr(node, 'properties'):
                for key, value in node.properties.items():
                    if value is not None:
                        # Convert lists to strings for Neo4j
                        if isinstance(value, list):
                            props[key] = str(value)
                        else:
                            props[key] = value
            
            # Create node with appropriate label
            label = node.node_type.value
            if hasattr(node, 'properties') and 'custom_type' in node.properties:
                label = node.properties['custom_type']
            
            cypher = f"MERGE (n:{label} {{name: $name}}) SET n += $props"
            session.run(cypher, name=node.name, props=props)
    
    def _create_relationships(self, session: Any, graph: Graph) -> None:
        """Create all relationships in the graph."""
        for rel in graph.relationships:
            # Get node types for matching
            from_node = graph.get_node(rel.from_node_id)
            to_node = graph.get_node(rel.to_node_id)
            
            if from_node is None or to_node is None:
                # Handle forward references
                cypher = """
                MERGE (from {node_id: $from_id})
                MERGE (to {node_id: $to_id})
                MERGE (from)-[r:%s]->(to)
                SET r += $props
                """ % rel.rel_type.value
                
                session.run(cypher, 
                           from_id=rel.from_node_id,
                           to_id=rel.to_node_id,
                           props=rel.properties)
            else:
                # Use proper node matching
                from_label = from_node.node_type.value
                to_label = to_node.node_type.value
                
                if hasattr(from_node, 'properties') and 'custom_type' in from_node.properties:
                    from_label = from_node.properties['custom_type']
                if hasattr(to_node, 'properties') and 'custom_type' in to_node.properties:
                    to_label = to_node.properties['custom_type']
                
                cypher = f"""
                MATCH (from:{from_label} {{name: $from_name}})
                MATCH (to:{to_label} {{name: $to_name}})
                MERGE (from)-[r:{rel.rel_type.value}]->(to)
                SET r += $props
                """
                
                session.run(cypher,
                           from_name=from_node.name,
                           to_name=to_node.name,
                           props=rel.properties)
    
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