"""JSON export functionality."""

import json
from pathlib import Path
from typing import Any, Dict

from ..core.graph import Graph


class JSONExporter:
    """Export graph to JSON format."""
    
    def __init__(self, indent: int = 2):
        self.indent = indent
    
    def export(self, graph: Graph, output_path: Path) -> None:
        """Export graph to JSON file."""
        data = graph.to_dict()
        
        with open(output_path, 'w') as f:
            json.dump(data, f, indent=self.indent, default=str)
    
    def export_to_string(self, graph: Graph) -> str:
        """Export graph to JSON string."""
        data = graph.to_dict()
        return json.dumps(data, indent=self.indent, default=str)
    
    def export_nodes_only(self, graph: Graph, output_path: Path) -> None:
        """Export only nodes to JSON."""
        data = {
            "nodes": [node.to_dict() for node in graph.nodes.values()],
            "statistics": {
                "total_nodes": len(graph.nodes)
            }
        }
        
        with open(output_path, 'w') as f:
            json.dump(data, f, indent=self.indent, default=str)
    
    def export_relationships_only(self, graph: Graph, output_path: Path) -> None:
        """Export only relationships to JSON."""
        data = {
            "relationships": [rel.to_dict() for rel in graph.relationships],
            "statistics": {
                "total_relationships": len(graph.relationships)
            }
        }
        
        with open(output_path, 'w') as f:
            json.dump(data, f, indent=self.indent, default=str)