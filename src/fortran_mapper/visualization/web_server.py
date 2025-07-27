"""Web server for serving interactive graph visualizations."""

import http.server
import socketserver
import json
import os
import logging
from pathlib import Path
from urllib.parse import urlparse, parse_qs
from typing import Optional
from neo4j import Driver

from .graph_generator import DynamicGraphGenerator

logger = logging.getLogger(__name__)


class VisualizationHandler(http.server.SimpleHTTPRequestHandler):
    """HTTP request handler for visualization endpoints."""
    
    generator = None  # Class variable to store the generator
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    
    def do_GET(self):
        """Handle GET requests."""
        parsed_path = urlparse(self.path)
        path = parsed_path.path
        query_params = parse_qs(parsed_path.query)
        
        # API endpoints
        if path.startswith('/api/'):
            self.handle_api_request(path, query_params)
        else:
            # Serve static files
            super().do_GET()
    
    def handle_api_request(self, path: str, params: dict):
        """Handle API requests for graph data."""
        try:
            # Debug logging
            logger.info(f"API request: {path}")
            logger.info(f"Generator class var: {VisualizationHandler.generator}")
            logger.info(f"Generator instance var: {getattr(self, 'generator', None)}")
            
            if not VisualizationHandler.generator:
                self.send_error(500, "Generator not initialized")
                return
                
            if path == '/api/schema':
                data = VisualizationHandler.generator.discover_schema()
            
            elif path == '/api/graph/overview':
                limit = int(params.get('limit', ['500'])[0])
                data = VisualizationHandler.generator.generate_overview_graph(limit)
            
            elif path == '/api/graph/focused':
                node_label = params.get('label', [None])[0]
                rel_type = params.get('relationship', [None])[0]
                limit = int(params.get('limit', ['500'])[0])
                
                if not node_label:
                    self.send_error(400, "Missing 'label' parameter")
                    return
                
                data = VisualizationHandler.generator.generate_focused_graph(node_label, rel_type, limit)
            
            elif path == '/api/graph/statistics':
                data = VisualizationHandler.generator.generate_statistics_graph()
            
            elif path == '/api/graph/neighborhood':
                node_id = params.get('node_id', [None])[0]
                depth = int(params.get('depth', ['2'])[0])
                limit = int(params.get('limit', ['100'])[0])
                
                if not node_id:
                    self.send_error(400, "Missing 'node_id' parameter")
                    return
                
                data = VisualizationHandler.generator.generate_neighborhood_graph(node_id, depth, limit)
            
            elif path == '/api/search':
                query = params.get('q', [None])[0]
                limit = int(params.get('limit', ['50'])[0])
                
                if not query:
                    self.send_error(400, "Missing 'q' parameter")
                    return
                
                data = VisualizationHandler.generator.search_nodes(query, limit)
            
            elif path == '/api/graph/filtered':
                # Get filter parameters
                node_labels = params.get('node_labels[]', [])
                relationship_types = params.get('relationship_types[]', [])
                limit = int(params.get('limit', ['500'])[0])
                connected_only = params.get('connected_only', ['true'])[0].lower() == 'true'
                
                data = VisualizationHandler.generator.generate_filtered_graph(
                    node_labels=node_labels if node_labels else None,
                    relationship_types=relationship_types if relationship_types else None,
                    limit=limit,
                    connected_only=connected_only
                )
            
            else:
                self.send_error(404, "Unknown API endpoint")
                return
            
            # Send response
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.send_header('Access-Control-Allow-Origin', '*')
            self.end_headers()
            self.wfile.write(json.dumps(data).encode())
            
        except Exception as e:
            logger.error(f"API error: {e}")
            self.send_error(500, str(e))


class VisualizationServer:
    """Web server for graph visualization."""
    
    def __init__(self, driver: Driver, port: int = 8080):
        """Initialize the visualization server.
        
        Args:
            driver: Neo4j driver instance
            port: Port to run the server on
        """
        self.driver = driver
        self.port = port
        self.generator = DynamicGraphGenerator(driver)
        self.server = None
        
        # Path to static files (HTML, CSS, JS)
        self.static_dir = Path(__file__).parent / "static"
        
    def start(self):
        """Start the web server."""
        # Set generator as class variable so all instances can access it
        VisualizationHandler.generator = self.generator
        
        # Change to static directory
        original_dir = os.getcwd()
        try:
            os.chdir(self.static_dir)
            
            with socketserver.TCPServer(("", self.port), VisualizationHandler) as httpd:
                self.server = httpd
                print(f"\nVisualization server running at http://localhost:{self.port}/")
                print(f"Open http://localhost:{self.port}/graph_viewer.html to view the visualization")
                print("Press Ctrl+C to stop the server\n")
                
                try:
                    httpd.serve_forever()
                except KeyboardInterrupt:
                    print("\nServer stopped.")
        finally:
            os.chdir(original_dir)
    
    def export_data(self, output_dir: str):
        """Export all graph data to files."""
        self.generator.export_all_graphs(output_dir)