"""Dynamic graph data generator that adapts to any Neo4j schema."""

import json
import logging
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from neo4j import Driver

logger = logging.getLogger(__name__)


class DynamicGraphGenerator:
    """Generate graph data dynamically based on Neo4j schema."""
    
    def __init__(self, driver: Driver):
        """Initialize with Neo4j driver."""
        self.driver = driver
        self._schema_cache = None
        
    def discover_schema(self) -> Dict[str, Any]:
        """Discover the graph schema from Neo4j."""
        if self._schema_cache:
            return self._schema_cache
            
        schema = {
            "node_labels": [],
            "relationship_types": [],
            "node_properties": {},
            "relationship_properties": {},
            "statistics": {}
        }
        
        try:
            with self.driver.session() as session:
                # Get all node labels
                result = session.run("""
                    CALL db.labels() YIELD label
                    RETURN collect(label) as labels
                """)
                labels_record = result.single()
                schema["node_labels"] = labels_record["labels"] if labels_record else []
                
                # Get all relationship types
                result = session.run("""
                    CALL db.relationshipTypes() YIELD relationshipType
                    RETURN collect(relationshipType) as types
                """)
                types_record = result.single()
                schema["relationship_types"] = types_record["types"] if types_record else []
                
                # Get node properties for each label
                for label in schema["node_labels"]:
                    result = session.run(f"""
                        MATCH (n:{label})
                        WITH n LIMIT 100
                        UNWIND keys(n) as key
                        RETURN DISTINCT key
                    """)
                    schema["node_properties"][label] = [record["key"] for record in result]
                
                # Get relationship counts
                result = session.run("""
                    MATCH ()-[r]->()
                    RETURN type(r) as type, count(r) as count
                    ORDER BY count DESC
                """)
                schema["statistics"]["relationship_counts"] = {
                    record["type"]: record["count"] for record in result
                }
                
                # Get node counts
                schema["statistics"]["node_counts"] = {}
                for label in schema["node_labels"]:
                    result = session.run(f"MATCH (n:{label}) RETURN count(n) as count")
                    count_record = result.single()
                    if count_record:
                        schema["statistics"]["node_counts"][label] = count_record["count"]
                    
        except Exception as e:
            logger.error(f"Error discovering schema: {e}")
            # Return minimal schema on error
            return {
                "node_labels": [],
                "relationship_types": [],
                "node_properties": {},
                "relationship_properties": {},
                "statistics": {},
                "error": str(e)
            }
        
        self._schema_cache = schema
        return schema
    
    def generate_overview_graph(self, limit: int = 500) -> Dict[str, Any]:
        """Generate a general overview graph showing all node types and relationships."""
        schema = self.discover_schema()
        
        nodes = []
        edges = []
        node_map = {}
        
        with self.driver.session() as session:
            # For overview, prioritize connected nodes
            # First get highly connected nodes
            query = """
                MATCH (n)
                WITH n, 
                     COUNT{(n)-[:CALLS]-()} + 
                     COUNT{(n)-[:DEFINED_IN]-()} as degree
                WHERE degree > 0
                RETURN id(n) as id, n as properties, labels(n) as labels, degree
                ORDER BY degree DESC
                LIMIT $limit
            """
            
            result = session.run(query, limit=limit)
            
            for record in result:
                node_id = f"node_{record['id']}"
                properties = dict(record["properties"])
                labels = record["labels"]
                
                # Find best display property
                display_value = properties.get('name', properties.get('path', str(record['id'])))
                
                nodes.append({
                    "id": node_id,
                    "label": display_value,
                    "type": labels[0] if labels else "unknown",
                    "properties": properties,
                    "degree": record["degree"]
                })
                node_map[record['id']] = node_id
            
            # If we have room, add some isolated nodes for completeness
            remaining = limit - len(nodes)
            if remaining > 0 and False:  # Disabled for now - focus on connected components
                query = """
                    MATCH (n)
                    WHERE NOT (n)-[]-()
                    RETURN id(n) as id, n as properties, labels(n) as labels
                    LIMIT $limit
                """
                result = session.run(query, limit=remaining//2)
                
                for record in result:
                    node_id = f"node_{record['id']}"
                    properties = dict(record["properties"])
                    labels = record["labels"]
                    
                    nodes.append({
                        "id": node_id,
                        "label": properties.get('name', properties.get('path', str(record['id']))),
                        "type": labels[0] if labels else "unknown",
                        "properties": properties,
                        "degree": 0
                    })
                    node_map[record['id']] = node_id
            
            # Get relationships between sampled nodes
            node_ids = list(node_map.keys())
            if node_ids:
                query = """
                    MATCH (n)-[r]->(m)
                    WHERE id(n) IN $node_ids AND id(m) IN $node_ids
                    RETURN id(n) as source_id, id(m) as target_id, 
                           type(r) as type, properties(r) as properties
                    LIMIT $limit
                """
                
                result = session.run(query, node_ids=node_ids, limit=limit)
                for record in result:
                    if record['source_id'] in node_map and record['target_id'] in node_map:
                        edges.append({
                            "source": node_map[record['source_id']],
                            "target": node_map[record['target_id']],
                            "type": record['type'],
                            "properties": dict(record['properties']) if record['properties'] else {}
                        })
        
        return {
            "nodes": nodes,
            "edges": edges,
            "schema": schema,
            "info": {
                "total_in_db": sum(schema["statistics"]["node_counts"].values()),
                "shown": len(nodes),
                "connected_only": True
            }
        }
    
    def generate_focused_graph(self, node_label: str, relationship_type: Optional[str] = None,
                              limit: int = 500) -> Dict[str, Any]:
        """Generate a graph focused on specific node label and optionally relationship type."""
        nodes = []
        edges = []
        node_map = {}
        
        with self.driver.session() as session:
            # Get nodes of specified label
            props = self.discover_schema()["node_properties"].get(node_label, [])
            display_prop = self._find_display_property(props)
            
            if relationship_type:
                # Get nodes connected by specific relationship
                query = f"""
                    MATCH (n:{node_label})-[r:{relationship_type}]-(m)
                    WITH n, r, m
                    LIMIT $limit
                    WITH collect(DISTINCT n) + collect(DISTINCT m) as all_nodes,
                         collect(r) as relationships
                    UNWIND all_nodes as node
                    WITH node, relationships
                    RETURN DISTINCT id(node) as id, 
                           node as properties,
                           labels(node) as labels
                """
            else:
                # Get nodes with any relationships
                query = f"""
                    MATCH (n:{node_label})
                    OPTIONAL MATCH (n)-[r]-(m)
                    WITH n, collect(DISTINCT m) as connected, collect(r) as relationships
                    WHERE size(connected) > 0 OR relationships = []
                    RETURN id(n) as id, n as properties, labels(n) as labels
                    LIMIT $limit
                """
            
            result = session.run(query, limit=limit)
            for record in result:
                node_id = f"node_{record['id']}"
                properties = dict(record["properties"])
                labels = record["labels"]
                
                nodes.append({
                    "id": node_id,
                    "label": properties.get(display_prop, str(record['id'])),
                    "type": labels[0] if labels else "unknown",
                    "properties": properties
                })
                node_map[record['id']] = node_id
            
            # Get relationships
            if node_map:
                node_ids = list(node_map.keys())
                
                if relationship_type:
                    rel_query = f"""
                        MATCH (n)-[r:{relationship_type}]->(m)
                        WHERE id(n) IN $node_ids AND id(m) IN $node_ids
                        RETURN id(n) as source_id, id(m) as target_id,
                               type(r) as type, properties(r) as properties
                    """
                else:
                    rel_query = """
                        MATCH (n)-[r]->(m)
                        WHERE id(n) IN $node_ids AND id(m) IN $node_ids
                        RETURN id(n) as source_id, id(m) as target_id,
                               type(r) as type, properties(r) as properties
                    """
                
                result = session.run(rel_query, node_ids=node_ids)
                for record in result:
                    if record['source_id'] in node_map and record['target_id'] in node_map:
                        edges.append({
                            "source": node_map[record['source_id']],
                            "target": node_map[record['target_id']],
                            "type": record['type'],
                            "properties": dict(record['properties']) if record['properties'] else {}
                        })
        
        return {
            "nodes": nodes,
            "edges": edges,
            "focus": {
                "node_label": node_label,
                "relationship_type": relationship_type
            }
        }
    
    def generate_statistics_graph(self) -> Dict[str, Any]:
        """Generate graph showing statistics as a meta-graph."""
        schema = self.discover_schema()
        nodes = []
        edges = []
        
        # Create nodes for each label
        for i, label in enumerate(schema["node_labels"]):
            count = schema["statistics"]["node_counts"].get(label, 0)
            nodes.append({
                "id": label,
                "label": f"{label}\n({count} nodes)",
                "type": "node_type",
                "size": min(50, 10 + count / 100),  # Scale size by count
                "properties": {
                    "count": count,
                    "properties": schema["node_properties"].get(label, [])
                }
            })
        
        # Create edges based on relationships
        with self.driver.session() as session:
            query = """
                MATCH (n)-[r]->(m)
                WITH labels(n)[0] as source_label,
                     type(r) as rel_type,
                     labels(m)[0] as target_label,
                     count(r) as count
                WHERE source_label IS NOT NULL AND target_label IS NOT NULL
                RETURN source_label, rel_type, target_label, count
                ORDER BY count DESC
            """
            
            result = session.run(query)
            for record in result:
                edges.append({
                    "source": record["source_label"],
                    "target": record["target_label"],
                    "type": record["rel_type"],
                    "weight": record["count"],
                    "label": f"{record['rel_type']} ({record['count']})"
                })
        
        return {
            "nodes": nodes,
            "edges": edges,
            "type": "statistics"
        }
    
    def generate_neighborhood_graph(self, node_id: str, depth: int = 2, 
                                  limit: int = 100) -> Dict[str, Any]:
        """Generate graph showing neighborhood of a specific node."""
        nodes = []
        edges = []
        node_map = {}
        
        with self.driver.session() as session:
            # Parse node_id to get actual Neo4j ID
            try:
                actual_id = int(node_id.split('_')[-1])
            except:
                # Try to find node by property value
                query = """
                    MATCH (n)
                    WHERE any(key in keys(n) WHERE n[key] = $node_id)
                    RETURN id(n) as id
                    LIMIT 1
                """
                result = session.run(query, node_id=node_id)
                record = result.single()
                if not record:
                    return {"nodes": [], "edges": [], "error": "Node not found"}
                actual_id = record["id"]
            
            # Get neighborhood
            query = """
                MATCH path = (center)-[*0..$depth]-(neighbor)
                WHERE id(center) = $node_id
                WITH nodes(path) as path_nodes, relationships(path) as path_rels
                UNWIND path_nodes as node
                WITH DISTINCT node, path_rels
                RETURN id(node) as id, node as properties, labels(node) as labels
                LIMIT $limit
            """
            
            result = session.run(query, node_id=actual_id, depth=depth, limit=limit)
            
            for record in result:
                node_id_str = f"node_{record['id']}"
                properties = dict(record["properties"])
                labels = record["labels"]
                
                # Find best display property
                display_value = str(record['id'])
                for prop in ['name', 'title', 'label', 'id']:
                    if prop in properties:
                        display_value = str(properties[prop])
                        break
                
                nodes.append({
                    "id": node_id_str,
                    "label": display_value,
                    "type": labels[0] if labels else "unknown",
                    "properties": properties,
                    "distance": 0 if record['id'] == actual_id else -1
                })
                node_map[record['id']] = node_id_str
            
            # Get relationships
            if node_map:
                node_ids = list(node_map.keys())
                rel_query = """
                    MATCH (n)-[r]->(m)
                    WHERE id(n) IN $node_ids AND id(m) IN $node_ids
                    RETURN id(n) as source_id, id(m) as target_id,
                           type(r) as type, properties(r) as properties
                """
                
                result = session.run(rel_query, node_ids=node_ids)
                for record in result:
                    if record['source_id'] in node_map and record['target_id'] in node_map:
                        edges.append({
                            "source": node_map[record['source_id']],
                            "target": node_map[record['target_id']],
                            "type": record['type'],
                            "properties": dict(record['properties']) if record['properties'] else {}
                        })
        
        # Calculate distances from center
        center_id = f"node_{actual_id}"
        distances = {center_id: 0}
        changed = True
        while changed:
            changed = False
            for edge in edges:
                if edge['source'] in distances and edge['target'] not in distances:
                    distances[edge['target']] = distances[edge['source']] + 1
                    changed = True
                elif edge['target'] in distances and edge['source'] not in distances:
                    distances[edge['source']] = distances[edge['target']] + 1
                    changed = True
        
        # Update node distances
        for node in nodes:
            if node['id'] in distances:
                node['distance'] = distances[node['id']]
        
        return {
            "nodes": nodes,
            "edges": edges,
            "center": center_id,
            "depth": depth
        }
    
    def _find_display_property(self, properties: List[str]) -> str:
        """Find the best property to use for node display."""
        preferred_props = ['name', 'title', 'label', 'id', 'value']
        
        for prop in preferred_props:
            if prop in properties:
                return prop
        
        # Return first property if available
        return properties[0] if properties else 'id'
    
    def export_all_graphs(self, output_dir: str):
        """Export all graph types to JSON files."""
        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)
        
        # Discover schema first
        schema = self.discover_schema()
        
        # Export schema
        with open(output_path / "schema.json", "w") as f:
            json.dump(schema, f, indent=2)
        
        # Export overview graph
        logger.info("Generating overview graph...")
        overview = self.generate_overview_graph()
        with open(output_path / "overview_graph.json", "w") as f:
            json.dump(overview, f, indent=2)
        
        # Export statistics graph
        logger.info("Generating statistics graph...")
        stats = self.generate_statistics_graph()
        with open(output_path / "statistics_graph.json", "w") as f:
            json.dump(stats, f, indent=2)
        
        # Export focused graphs for each node label
        for label in schema["node_labels"]:
            logger.info(f"Generating focused graph for {label}...")
            focused = self.generate_focused_graph(label)
            filename = f"focused_{label.lower()}_graph.json"
            with open(output_path / filename, "w") as f:
                json.dump(focused, f, indent=2)
        
        logger.info(f"All graphs exported to {output_path}/")
    
    def search_nodes(self, query: str, limit: int = 50) -> List[Dict[str, Any]]:
        """Search for nodes matching a query string."""
        results = []
        
        with self.driver.session() as session:
            # Search across all string properties
            cypher_query = """
                MATCH (n)
                WHERE any(key in keys(n) WHERE 
                    n[key] =~ $pattern OR 
                    toString(n[key]) CONTAINS $query
                )
                RETURN id(n) as id, n as properties, labels(n) as labels
                LIMIT $limit
            """
            
            result = session.run(
                cypher_query, 
                query=query,
                pattern=f"(?i).*{query}.*",
                limit=limit
            )
            
            for record in result:
                properties = dict(record["properties"])
                labels = record["labels"]
                
                # Find matching property
                matching_prop = None
                for key, value in properties.items():
                    if query.lower() in str(value).lower():
                        matching_prop = key
                        break
                
                results.append({
                    "id": f"node_{record['id']}",
                    "labels": labels,
                    "properties": properties,
                    "matching_property": matching_prop,
                    "display": properties.get(self._find_display_property(list(properties.keys())), str(record['id']))
                })
        
        return results
    
    def generate_filtered_graph(self, node_labels: Optional[List[str]] = None,
                              relationship_types: Optional[List[str]] = None,
                              limit: int = 500,
                              connected_only: bool = True) -> Dict[str, Any]:
        """Generate a graph with custom filters on nodes and relationships."""
        nodes = []
        edges = []
        node_map = {}
        
        with self.driver.session() as session:
            # Build node query based on filters
            if node_labels:
                label_filter = " OR ".join([f"n:{label}" for label in node_labels])
                node_filter = f"({label_filter})"
            else:
                node_filter = "n"
            
            # Build relationship filter
            if relationship_types:
                rel_patterns = []
                for rel_type in relationship_types:
                    rel_patterns.append(f"(n)-[:{rel_type}]-()")
                rel_filter = " OR ".join(rel_patterns)
                degree_calc = " + ".join([f"COUNT{{{pattern}}}" for pattern in rel_patterns])
            else:
                rel_filter = "(n)-[]-()"
                degree_calc = "COUNT{(n)-[]-()} "
            
            if connected_only:
                query = f"""
                    MATCH {node_filter}
                    WHERE {rel_filter}
                    WITH n, {degree_calc} as degree
                    RETURN id(n) as id, n as properties, labels(n) as labels, degree
                    ORDER BY degree DESC
                    LIMIT $limit
                """
            else:
                query = f"""
                    MATCH {node_filter}
                    WITH n, {degree_calc} as degree
                    RETURN id(n) as id, n as properties, labels(n) as labels, degree
                    ORDER BY degree DESC
                    LIMIT $limit
                """
            
            result = session.run(query, limit=limit)
            
            for record in result:
                node_id = f"node_{record['id']}"
                properties = dict(record["properties"])
                labels = record["labels"]
                
                # Find best display property
                display_value = properties.get('name', properties.get('path', str(record['id'])))
                
                nodes.append({
                    "id": node_id,
                    "label": display_value,
                    "type": labels[0] if labels else "unknown",
                    "properties": properties,
                    "degree": record["degree"]
                })
                node_map[record['id']] = node_id
            
            # Get relationships
            node_ids = list(node_map.keys())
            if node_ids:
                if relationship_types:
                    rel_filter = " OR ".join([f"type(r) = '{rt}'" for rt in relationship_types])
                    rel_query = f"""
                        MATCH (n)-[r]->(m)
                        WHERE id(n) IN $node_ids AND id(m) IN $node_ids
                              AND ({rel_filter})
                        RETURN id(n) as source_id, id(m) as target_id,
                               type(r) as type, properties(r) as properties
                    """
                else:
                    rel_query = """
                        MATCH (n)-[r]->(m)
                        WHERE id(n) IN $node_ids AND id(m) IN $node_ids
                        RETURN id(n) as source_id, id(m) as target_id,
                               type(r) as type, properties(r) as properties
                    """
                
                result = session.run(rel_query, node_ids=node_ids)
                for record in result:
                    if record['source_id'] in node_map and record['target_id'] in node_map:
                        edges.append({
                            "source": node_map[record['source_id']],
                            "target": node_map[record['target_id']],
                            "type": record['type'],
                            "properties": dict(record['properties']) if record['properties'] else {}
                        })
        
        return {
            "nodes": nodes,
            "edges": edges,
            "filters": {
                "node_labels": node_labels,
                "relationship_types": relationship_types,
                "connected_only": connected_only
            }
        }