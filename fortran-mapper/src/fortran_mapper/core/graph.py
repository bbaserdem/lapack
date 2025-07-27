"""Graph data structure for Fortran code."""

from typing import Dict, List, Set, Optional, Any
from .nodes import BaseNode, NodeType
from .relationships import Relationship, RelationType


class Graph:
    """Graph representation of Fortran code structure."""
    
    def __init__(self):
        self.nodes: Dict[str, BaseNode] = {}
        self.relationships: Set[Relationship] = set()
        self._node_index: Dict[NodeType, Set[str]] = {}
    
    def add_node(self, node: BaseNode) -> None:
        """Add a node to the graph."""
        if node.node_id not in self.nodes:
            self.nodes[node.node_id] = node
            
            # Update index
            if node.node_type not in self._node_index:
                self._node_index[node.node_type] = set()
            self._node_index[node.node_type].add(node.node_id)
    
    def add_relationship(self, relationship: Relationship) -> None:
        """Add a relationship to the graph."""
        # Verify nodes exist
        if (relationship.from_node_id in self.nodes and 
            relationship.to_node_id in self.nodes):
            self.relationships.add(relationship)
        else:
            # Handle placeholder relationships for forward references
            if relationship.rel_type == RelationType.CALLS:
                # Allow CALLS relationships to non-existent nodes (forward declarations)
                self.relationships.add(relationship)
    
    def get_node(self, node_id: str) -> Optional[BaseNode]:
        """Get a node by ID."""
        return self.nodes.get(node_id)
    
    def get_nodes_by_type(self, node_type: NodeType) -> List[BaseNode]:
        """Get all nodes of a specific type."""
        node_ids = self._node_index.get(node_type, set())
        return [self.nodes[node_id] for node_id in node_ids]
    
    def get_relationships_from(self, node_id: str) -> List[Relationship]:
        """Get all relationships originating from a node."""
        return [rel for rel in self.relationships if rel.from_node_id == node_id]
    
    def get_relationships_to(self, node_id: str) -> List[Relationship]:
        """Get all relationships pointing to a node."""
        return [rel for rel in self.relationships if rel.to_node_id == node_id]
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert graph to dictionary for serialization."""
        return {
            "nodes": [node.to_dict() for node in self.nodes.values()],
            "relationships": [rel.to_dict() for rel in self.relationships],
            "statistics": {
                "total_nodes": len(self.nodes),
                "total_relationships": len(self.relationships),
                "nodes_by_type": {
                    node_type.value: len(self.get_nodes_by_type(node_type))
                    for node_type in NodeType
                }
            }
        }
    
    def merge(self, other: 'Graph') -> None:
        """Merge another graph into this one."""
        # Merge nodes
        for node in other.nodes.values():
            self.add_node(node)
        
        # Merge relationships
        for rel in other.relationships:
            self.add_relationship(rel)