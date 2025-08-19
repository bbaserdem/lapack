"""Graph data generator for visualization.

This module extracts and prepares graph data from Neo4j for visualization.
"""

from typing import Dict, List, Optional, Set, Any, Tuple
from neo4j import Driver
import json
from dataclasses import dataclass, asdict


@dataclass
class GraphNode:
    """Represents a node in the visualization."""
    id: str
    label: str
    type: str
    properties: Dict[str, Any]
    group: int = 0
    size: float = 10.0


@dataclass
class GraphEdge:
    """Represents an edge in the visualization."""
    source: str
    target: str
    type: str
    properties: Optional[Dict[str, Any]] = None
    weight: float = 1.0


@dataclass
class GraphData:
    """Container for graph visualization data."""
    nodes: List[GraphNode]
    edges: List[GraphEdge]
    metadata: Dict[str, Any]


class GraphDataGenerator:
    """Generates graph data for visualization from Neo4j."""
    
    def __init__(self, driver: Driver):
        """Initialize with Neo4j driver."""
        self.driver = driver
        self._node_type_groups = {
            'Routine': 0,
            'File': 1,
            'Module': 2,
            'Interface': 3,
            'Type': 4,
            'Variable': 5,
            'Constant': 6,
            'CommonBlock': 7
        }
    
    def get_available_node_types(self) -> List[str]:
        """Get all available node types in the database."""
        with self.driver.session() as session:
            result = session.run("CALL db.labels()")
            return [record[0] for record in result]
    
    def get_available_relationship_types(self) -> List[str]:
        """Get all available relationship types in the database."""
        with self.driver.session() as session:
            result = session.run("CALL db.relationshipTypes()")
            return [record[0] for record in result]
    
    def get_node_counts(self) -> Dict[str, int]:
        """Get counts of each node type."""
        counts = {}
        node_types = self.get_available_node_types()
        
        with self.driver.session() as session:
            for node_type in node_types:
                result = session.run(f"MATCH (n:{node_type}) RETURN count(n) as count")
                counts[node_type] = result.single()['count']
        
        return counts
    
    def get_relationship_counts(self) -> Dict[str, int]:
        """Get counts of each relationship type."""
        counts = {}
        rel_types = self.get_available_relationship_types()
        
        with self.driver.session() as session:
            for rel_type in rel_types:
                result = session.run(f"MATCH ()-[r:{rel_type}]->() RETURN count(r) as count")
                counts[rel_type] = result.single()['count']
        
        return counts
    
    def generate_overview_graph(self, 
                               node_limit: int = 100,
                               node_types: Optional[List[str]] = None,
                               relationship_types: Optional[List[str]] = None) -> GraphData:
        """Generate an overview graph with filtering options."""
        nodes = []
        edges = []
        node_map = {}
        
        # Build node type filter
        node_filter = ""
        if node_types:
            labels = "|".join(node_types)
            node_filter = f":{labels}"
        
        # Build relationship type filter
        rel_filter = ""
        if relationship_types:
            rel_types = "|".join(relationship_types)
            rel_filter = f":{rel_types}"
        
        with self.driver.session() as session:
            # Get nodes
            query = f"""
            MATCH (n{node_filter})
            WITH n, rand() as r
            ORDER BY r
            LIMIT $limit
            RETURN id(n) as id, labels(n) as labels, properties(n) as props
            """
            
            result = session.run(query, limit=node_limit)
            for record in result:
                node_id = str(record['id'])
                labels = record['labels']
                props = record['props'] or {}
                
                # Determine node label
                if 'name' in props:
                    label = props['name']
                elif 'path' in props:
                    label = props['path'].split('/')[-1]
                else:
                    label = f"{labels[0]}_{node_id}"
                
                # Determine node type and group
                node_type = labels[0] if labels else 'Unknown'
                group = self._node_type_groups.get(node_type, 8)
                
                # Calculate node size based on relationships
                size_query = f"MATCH (n) WHERE id(n) = $id RETURN size((n)--()) as degree"
                degree_result = session.run(size_query, id=int(node_id))
                degree = degree_result.single()['degree']
                size = 10 + min(degree * 2, 40)  # Scale size with degree
                
                node = GraphNode(
                    id=node_id,
                    label=label,
                    type=node_type,
                    properties=props,
                    group=group,
                    size=size
                )
                nodes.append(node)
                node_map[node_id] = node
            
            # Get relationships between these nodes
            if nodes:
                node_ids = [int(n.id) for n in nodes]
                rel_query = f"""
                MATCH (a)-[r{rel_filter}]->(b)
                WHERE id(a) IN $node_ids AND id(b) IN $node_ids
                RETURN id(a) as source, id(b) as target, type(r) as type, properties(r) as props
                """
                
                result = session.run(rel_query, node_ids=node_ids)
                for record in result:
                    edge = GraphEdge(
                        source=str(record['source']),
                        target=str(record['target']),
                        type=record['type'],
                        properties=record['props']
                    )
                    edges.append(edge)
        
        metadata = {
            'node_count': len(nodes),
            'edge_count': len(edges),
            'node_types': list(set(n.type for n in nodes)),
            'relationship_types': list(set(e.type for e in edges)),
            'filters': {
                'node_types': node_types,
                'relationship_types': relationship_types,
                'node_limit': node_limit
            }
        }
        
        return GraphData(nodes=nodes, edges=edges, metadata=metadata)
    
    def generate_centered_graph(self,
                               center_node_id: Optional[int] = None,
                               center_node_name: Optional[str] = None,
                               center_node_type: str = 'Routine',
                               depth: int = 2,
                               node_types: Optional[List[str]] = None,
                               relationship_types: Optional[List[str]] = None) -> GraphData:
        """Generate a graph centered on a specific node."""
        nodes = []
        edges = []
        node_map = {}
        
        # Build relationship type filter
        rel_filter = ""
        if relationship_types:
            rel_types = "|".join(relationship_types)
            rel_filter = f":{rel_types}"
        else:
            rel_filter = ""
        
        with self.driver.session() as session:
            # Find center node
            if center_node_id is not None:
                center_query = "MATCH (n) WHERE id(n) = $id RETURN n"
                result = session.run(center_query, id=center_node_id)
            elif center_node_name is not None:
                center_query = f"MATCH (n:{center_node_type}) WHERE n.name = $name RETURN n"
                result = session.run(center_query, name=center_node_name)
            else:
                # Pick a random node of the specified type
                center_query = f"""
                MATCH (n:{center_node_type})
                WITH n, rand() as r
                ORDER BY r
                LIMIT 1
                RETURN n
                """
                result = session.run(center_query)
            
            center_record = result.single()
            if not center_record:
                return GraphData(nodes=[], edges=[], metadata={'error': 'Center node not found'})
            
            center_node = center_record['n']
            center_id = center_node.id
            
            # Get connected nodes within depth
            connected_query = f"""
            MATCH path = (center)-[{rel_filter}*0..{depth}]-(connected)
            WHERE id(center) = $center_id
            WITH DISTINCT connected, min(length(path)) as distance
            RETURN id(connected) as id, labels(connected) as labels, 
                   properties(connected) as props, distance
            ORDER BY distance
            """
            
            # Apply node type filter if specified
            if node_types:
                connected_query = f"""
                MATCH path = (center)-[{rel_filter}*0..{depth}]-(connected)
                WHERE id(center) = $center_id AND any(label in labels(connected) WHERE label IN $node_types)
                WITH DISTINCT connected, min(length(path)) as distance
                RETURN id(connected) as id, labels(connected) as labels, 
                       properties(connected) as props, distance
                ORDER BY distance
                """
                result = session.run(connected_query, center_id=center_id, node_types=node_types)
            else:
                result = session.run(connected_query, center_id=center_id)
            
            # Create nodes
            for record in result:
                node_id = str(record['id'])
                labels = record['labels']
                props = record['props'] or {}
                distance = record['distance']
                
                # Determine node label
                if 'name' in props:
                    label = props['name']
                elif 'path' in props:
                    label = props['path'].split('/')[-1]
                else:
                    label = f"{labels[0]}_{node_id}"
                
                # Determine node type and group
                node_type = labels[0] if labels else 'Unknown'
                group = self._node_type_groups.get(node_type, 8)
                
                # Size based on distance from center
                size = 30 - (distance * 5) if node_id == str(center_id) else 15 - (distance * 2)
                size = max(size, 8)
                
                node = GraphNode(
                    id=node_id,
                    label=label,
                    type=node_type,
                    properties=props,
                    group=group,
                    size=size
                )
                nodes.append(node)
                node_map[node_id] = node
            
            # Get relationships
            if nodes:
                node_ids = [int(n.id) for n in nodes]
                rel_query = f"""
                MATCH (a)-[r{rel_filter}]->(b)
                WHERE id(a) IN $node_ids AND id(b) IN $node_ids
                RETURN id(a) as source, id(b) as target, type(r) as type, properties(r) as props
                """
                
                result = session.run(rel_query, node_ids=node_ids)
                for record in result:
                    edge = GraphEdge(
                        source=str(record['source']),
                        target=str(record['target']),
                        type=record['type'],
                        properties=record['props']
                    )
                    edges.append(edge)
        
        metadata = {
            'node_count': len(nodes),
            'edge_count': len(edges),
            'node_types': list(set(n.type for n in nodes)),
            'relationship_types': list(set(e.type for e in edges)),
            'center_node': {
                'id': str(center_id),
                'name': center_node.get('name', 'Unknown'),
                'type': list(center_node.labels)[0] if center_node.labels else 'Unknown'
            },
            'depth': depth,
            'filters': {
                'node_types': node_types,
                'relationship_types': relationship_types
            }
        }
        
        return GraphData(nodes=nodes, edges=edges, metadata=metadata)
    
    def generate_call_hierarchy(self, 
                               routine_name: str,
                               direction: str = 'both',  # 'calls', 'called_by', 'both'
                               max_depth: int = 3) -> GraphData:
        """Generate a call hierarchy visualization for a specific routine."""
        rel_pattern = {
            'calls': '-[:CALLS]->',
            'called_by': '<-[:CALLS]-',
            'both': '-[:CALLS]-'
        }.get(direction, '-[:CALLS]-')
        
        with self.driver.session() as session:
            query = f"""
            MATCH (start:Routine {{name: $routine_name}})
            MATCH path = (start){rel_pattern}{{0,{max_depth}}}(other:Routine)
            WITH DISTINCT start, other, min(length(path)) as distance
            RETURN start, other, distance
            """
            
            result = session.run(query, routine_name=routine_name)
            
            nodes = []
            node_map = {}
            
            for record in result:
                for node_data in [record['start'], record['other']]:
                    node_id = str(node_data.id)
                    if node_id not in node_map:
                        props = dict(node_data)
                        label = props.get('name', f'Routine_{node_id}')
                        
                        # Special styling for the center node
                        size = 30 if label == routine_name else 15
                        
                        node = GraphNode(
                            id=node_id,
                            label=label,
                            type='Routine',
                            properties=props,
                            group=0,
                            size=size
                        )
                        nodes.append(node)
                        node_map[node_id] = node
            
            # Get edges
            edges = []
            if nodes:
                node_ids = [int(n.id) for n in nodes]
                edge_query = """
                MATCH (a:Routine)-[r:CALLS]->(b:Routine)
                WHERE id(a) IN $node_ids AND id(b) IN $node_ids
                RETURN id(a) as source, id(b) as target
                """
                
                edge_result = session.run(edge_query, node_ids=node_ids)
                for record in edge_result:
                    edge = GraphEdge(
                        source=str(record['source']),
                        target=str(record['target']),
                        type='CALLS'
                    )
                    edges.append(edge)
        
        metadata = {
            'node_count': len(nodes),
            'edge_count': len(edges),
            'center_routine': routine_name,
            'direction': direction,
            'max_depth': max_depth,
            'visualization_type': 'call_hierarchy'
        }
        
        return GraphData(nodes=nodes, edges=edges, metadata=metadata)
    
    def export_graph_data(self, graph_data: GraphData, format: str = 'json') -> str:
        """Export graph data in various formats."""
        if format == 'json':
            return json.dumps({
                'nodes': [asdict(n) for n in graph_data.nodes],
                'edges': [asdict(e) for e in graph_data.edges],
                'metadata': graph_data.metadata
            }, indent=2)
        else:
            raise ValueError(f"Unsupported export format: {format}")