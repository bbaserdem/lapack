"""DOT/Graphviz export functionality."""

from pathlib import Path
from typing import Dict, Set, Optional

from ..core.graph import Graph
from ..core.nodes import NodeType
from ..core.relationships import RelationType


class DOTExporter:
    """Export graph to DOT format for Graphviz."""
    
    def __init__(self):
        self.node_colors = {
            NodeType.ROUTINE: "lightblue",
            NodeType.FILE: "lightgreen",
            NodeType.MODULE: "lightyellow",
            NodeType.CUSTOM: "lightcoral"
        }
        
        self.edge_colors = {
            RelationType.CALLS: "blue",
            RelationType.DEFINED_IN: "green",
            RelationType.CONTAINS: "orange",
            RelationType.USES_MODULE: "purple",
            RelationType.CUSTOM: "red"
        }
    
    def export(self, graph: Graph, output_path: Path, 
               include_node_types: Optional[Set[NodeType]] = None,
               include_edge_types: Optional[Set[RelationType]] = None) -> None:
        """Export graph to DOT file."""
        dot_content = self.generate_dot(graph, include_node_types, include_edge_types)
        
        with open(output_path, 'w') as f:
            f.write(dot_content)
    
    def generate_dot(self, graph: Graph,
                    include_node_types: Optional[Set[NodeType]] = None,
                    include_edge_types: Optional[Set[RelationType]] = None) -> str:
        """Generate DOT format string."""
        lines = ["digraph FortranGraph {"]
        lines.append("  rankdir=TB;")
        lines.append("  node [shape=box, style=filled];")
        lines.append("")
        
        # Filter nodes
        nodes_to_include = set()
        for node in graph.nodes.values():
            if include_node_types is None or node.node_type in include_node_types:
                nodes_to_include.add(node.node_id)
        
        # Add nodes
        for node in graph.nodes.values():
            if node.node_id in nodes_to_include:
                color = self.node_colors.get(node.node_type, "white")
                label = self._escape_label(node.name)
                
                # Add properties to label
                props = []
                if hasattr(node, 'properties') and node.properties:
                    for key, value in node.properties.items():
                        if key not in ['name', 'routine_type'] and value:
                            props.append(f"{key}={value}")
                
                if props:
                    label += "\\n" + "\\n".join(props[:3])  # Limit to 3 properties
                
                lines.append(f'  "{node.node_id}" [label="{label}", fillcolor={color}];')
        
        lines.append("")
        
        # Filter and add edges
        for rel in graph.relationships:
            # Check if both nodes are included
            if (rel.from_node_id in nodes_to_include and 
                rel.to_node_id in nodes_to_include):
                
                # Check edge type filter
                if include_edge_types is None or rel.rel_type in include_edge_types:
                    color = self.edge_colors.get(rel.rel_type, "black")
                    label = rel.rel_type.value
                    
                    lines.append(f'  "{rel.from_node_id}" -> "{rel.to_node_id}" '
                                f'[label="{label}", color={color}];')
        
        lines.append("}")
        return "\n".join(lines)
    
    def _escape_label(self, text: str) -> str:
        """Escape text for DOT format."""
        return text.replace('"', '\\"').replace('\n', '\\n')
    
    def export_call_graph_only(self, graph: Graph, output_path: Path) -> None:
        """Export only the call graph (routines and calls)."""
        self.export(
            graph, 
            output_path,
            include_node_types={NodeType.ROUTINE},
            include_edge_types={RelationType.CALLS}
        )
    
    def export_file_structure(self, graph: Graph, output_path: Path) -> None:
        """Export file structure (files, routines, and defined_in relationships)."""
        self.export(
            graph,
            output_path,
            include_node_types={NodeType.FILE, NodeType.ROUTINE},
            include_edge_types={RelationType.DEFINED_IN}
        )