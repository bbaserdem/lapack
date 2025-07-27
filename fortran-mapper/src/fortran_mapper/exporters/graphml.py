"""GraphML export functionality."""

import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Dict, Any

from ..core.graph import Graph


class GraphMLExporter:
    """Export graph to GraphML format."""
    
    def __init__(self):
        self.namespace = "http://graphml.graphdrawing.org/xmlns"
        ET.register_namespace("", self.namespace)
    
    def export(self, graph: Graph, output_path: Path) -> None:
        """Export graph to GraphML file."""
        root = self._create_graphml_root()
        graph_element = self._create_graph_element(root, graph)
        
        # Write to file
        tree = ET.ElementTree(root)
        tree.write(output_path, encoding='utf-8', xml_declaration=True)
    
    def _create_graphml_root(self) -> ET.Element:
        """Create the root GraphML element with schema."""
        root = ET.Element("graphml")
        root.set("xmlns", self.namespace)
        root.set("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
        root.set("xsi:schemaLocation", 
                f"{self.namespace} http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd")
        
        # Define keys for node and edge attributes
        node_keys = [
            ("d0", "node", "name", "string"),
            ("d1", "node", "type", "string"),
            ("d2", "node", "properties", "string"),
        ]
        
        edge_keys = [
            ("d3", "edge", "type", "string"),
            ("d4", "edge", "properties", "string"),
        ]
        
        for key_id, for_element, attr_name, attr_type in node_keys + edge_keys:
            key_elem = ET.SubElement(root, "key")
            key_elem.set("id", key_id)
            key_elem.set("for", for_element)
            key_elem.set("attr.name", attr_name)
            key_elem.set("attr.type", attr_type)
        
        return root
    
    def _create_graph_element(self, root: ET.Element, graph: Graph) -> ET.Element:
        """Create the graph element with nodes and edges."""
        graph_elem = ET.SubElement(root, "graph")
        graph_elem.set("id", "FortranGraph")
        graph_elem.set("edgedefault", "directed")
        
        # Add nodes
        for node in graph.nodes.values():
            node_elem = ET.SubElement(graph_elem, "node")
            node_elem.set("id", node.node_id)
            
            # Name
            data_elem = ET.SubElement(node_elem, "data")
            data_elem.set("key", "d0")
            data_elem.text = node.name
            
            # Type
            data_elem = ET.SubElement(node_elem, "data")
            data_elem.set("key", "d1")
            data_elem.text = node.node_type.value
            
            # Properties
            if hasattr(node, 'properties') and node.properties:
                data_elem = ET.SubElement(node_elem, "data")
                data_elem.set("key", "d2")
                data_elem.text = str(node.properties)
        
        # Add edges
        edge_id = 0
        for rel in graph.relationships:
            edge_elem = ET.SubElement(graph_elem, "edge")
            edge_elem.set("id", f"e{edge_id}")
            edge_elem.set("source", rel.from_node_id)
            edge_elem.set("target", rel.to_node_id)
            
            # Type
            data_elem = ET.SubElement(edge_elem, "data")
            data_elem.set("key", "d3")
            data_elem.text = rel.rel_type.value
            
            # Properties
            if rel.properties:
                data_elem = ET.SubElement(edge_elem, "data")
                data_elem.set("key", "d4")
                data_elem.text = str(rel.properties)
            
            edge_id += 1
        
        return graph_elem
    
    def export_to_string(self, graph: Graph) -> str:
        """Export graph to GraphML string."""
        root = self._create_graphml_root()
        self._create_graph_element(root, graph)
        
        return ET.tostring(root, encoding='unicode')