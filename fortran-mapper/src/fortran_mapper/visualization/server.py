"""Web server for visualization interface.

This module provides a Flask-based web server to serve the visualization interface
and handle API requests for graph data.
"""

import os
import json
import webbrowser
from pathlib import Path
from typing import Optional, Dict, Any, List
from flask import Flask, render_template, jsonify, request, send_from_directory
from flask_cors import CORS
from neo4j import Driver
import logging

from .graph_generator import GraphDataGenerator


class VisualizationServer:
    """Flask server for graph visualization."""
    
    def __init__(self, driver: Driver, host: str = '127.0.0.1', port: int = 8080):
        """Initialize the visualization server.
        
        Args:
            driver: Neo4j driver instance
            host: Host address to bind to
            port: Port to listen on
        """
        self.driver = driver
        self.host = host
        self.port = port
        self.generator = GraphDataGenerator(driver)
        
        # Create Flask app
        self.app = Flask(__name__, 
                         template_folder=str(Path(__file__).parent / 'templates'))
        CORS(self.app)
        
        # Find node_modules directory by going up from current location
        self.project_root = Path(__file__).parent
        while self.project_root.parent != self.project_root:
            if (self.project_root / 'node_modules').exists():
                break
            self.project_root = self.project_root.parent
        
        self.node_modules_path = self.project_root / 'node_modules'
        
        # Configure logging
        logging.basicConfig(level=logging.INFO)
        self.logger = logging.getLogger(__name__)
        
        # Register routes
        self._register_routes()
    
    def _register_routes(self):
        """Register all API routes."""
        
        @self.app.route('/')
        def index():
            """Serve the main visualization page."""
            return render_template('index.html')
        
        @self.app.route('/static/<path:filename>')
        def serve_static(filename):
            """Serve static files from node_modules."""
            if filename == 'd3.min.js':
                d3_path = self.node_modules_path / 'd3' / 'dist' / 'd3.min.js'
                if d3_path.exists():
                    return send_from_directory(str(d3_path.parent), 'd3.min.js')
                else:
                    self.logger.error(f"D3.js not found at {d3_path}")
                    return "D3.js not found. Please run 'pnpm install' in the project root.", 404
            return "File not found", 404
        
        @self.app.route('/api/metadata')
        def get_metadata():
            """Get available node and relationship types with counts."""
            try:
                node_types = self.generator.get_node_counts()
                rel_types = self.generator.get_relationship_counts()
                
                return jsonify({
                    'node_types': node_types,
                    'relationship_types': rel_types
                })
            except Exception as e:
                self.logger.error(f"Error getting metadata: {e}")
                return jsonify({'error': str(e)}), 500
        
        @self.app.route('/api/graph')
        def get_graph():
            """Get graph data based on query parameters."""
            try:
                mode = request.args.get('mode', 'overview')
                
                # Parse filters
                node_types = request.args.get('node_types', '').split(',') if request.args.get('node_types') else None
                rel_types = request.args.get('relationship_types', '').split(',') if request.args.get('relationship_types') else None
                
                if mode == 'overview':
                    limit = int(request.args.get('limit', 100))
                    graph_data = self.generator.generate_overview_graph(
                        node_limit=limit,
                        node_types=node_types,
                        relationship_types=rel_types
                    )
                
                elif mode == 'centered':
                    center_id = request.args.get('center_id', type=int)
                    depth = int(request.args.get('depth', 2))
                    graph_data = self.generator.generate_centered_graph(
                        center_node_id=center_id,
                        depth=depth,
                        node_types=node_types,
                        relationship_types=rel_types
                    )
                
                elif mode == 'hierarchy':
                    routine_name = request.args.get('routine_name', '')
                    depth = int(request.args.get('depth', 3))
                    direction = request.args.get('direction', 'both')
                    
                    if routine_name:
                        graph_data = self.generator.generate_call_hierarchy(
                            routine_name=routine_name,
                            direction=direction,
                            max_depth=depth
                        )
                    else:
                        # If no routine specified, return empty graph
                        from .graph_generator import GraphData
                        graph_data = GraphData(nodes=[], edges=[], 
                                              metadata={'error': 'No routine specified'})
                
                else:
                    return jsonify({'error': f'Unknown mode: {mode}'}), 400
                
                # Convert to JSON-serializable format
                response = {
                    'nodes': [node.__dict__ for node in graph_data.nodes],
                    'edges': [edge.__dict__ for edge in graph_data.edges],
                    'metadata': graph_data.metadata
                }
                
                return jsonify(response)
                
            except Exception as e:
                self.logger.error(f"Error generating graph: {e}")
                return jsonify({'error': str(e)}), 500
        
        @self.app.route('/api/search')
        def search_nodes():
            """Search for nodes by name or properties."""
            try:
                query = request.args.get('q', '')
                if not query:
                    return jsonify([])
                
                # Simple search implementation
                results = []
                with self.driver.session() as session:
                    # Search across all node types
                    search_query = """
                    MATCH (n)
                    WHERE any(prop in keys(n) WHERE toString(n[prop]) =~ $pattern)
                    RETURN id(n) as id, labels(n) as labels, properties(n) as props
                    LIMIT 20
                    """
                    
                    pattern = f"(?i).*{query}.*"
                    result = session.run(search_query, pattern=pattern)
                    
                    for record in result:
                        node_id = str(record['id'])
                        labels = record['labels']
                        props = record['props'] or {}
                        
                        # Determine display label
                        if 'name' in props:
                            label = props['name']
                        elif 'path' in props:
                            label = props['path'].split('/')[-1]
                        else:
                            label = f"{labels[0]}_{node_id}"
                        
                        results.append({
                            'id': node_id,
                            'label': label,
                            'type': labels[0] if labels else 'Unknown',
                            'properties': props
                        })
                
                return jsonify(results)
                
            except Exception as e:
                self.logger.error(f"Error searching nodes: {e}")
                return jsonify({'error': str(e)}), 500
        
        @self.app.route('/api/node/<int:node_id>')
        def get_node_details(node_id):
            """Get detailed information about a specific node."""
            try:
                with self.driver.session() as session:
                    query = """
                    MATCH (n)
                    WHERE id(n) = $id
                    OPTIONAL MATCH (n)-[r]-(connected)
                    RETURN n, collect({
                        relationship: type(r),
                        direction: CASE WHEN startNode(r) = n THEN 'outgoing' ELSE 'incoming' END,
                        target: {
                            id: id(connected),
                            labels: labels(connected),
                            properties: properties(connected)
                        }
                    }) as connections
                    """
                    
                    result = session.run(query, id=node_id).single()
                    if not result:
                        return jsonify({'error': 'Node not found'}), 404
                    
                    node = result['n']
                    connections = result['connections']
                    
                    return jsonify({
                        'id': node_id,
                        'labels': list(node.labels),
                        'properties': dict(node),
                        'connections': connections
                    })
                    
            except Exception as e:
                self.logger.error(f"Error getting node details: {e}")
                return jsonify({'error': str(e)}), 500
        
        @self.app.route('/api/stats')
        def get_stats():
            """Get database statistics."""
            try:
                with self.driver.session() as session:
                    # Get total counts
                    node_count = session.run("MATCH (n) RETURN count(n) as count").single()['count']
                    rel_count = session.run("MATCH ()-[r]->() RETURN count(r) as count").single()['count']
                    
                    # Get top connected nodes
                    top_nodes_query = """
                    MATCH (n)
                    WITH n, size((n)--()) as degree
                    ORDER BY degree DESC
                    LIMIT 10
                    RETURN id(n) as id, labels(n) as labels, properties(n) as props, degree
                    """
                    
                    top_nodes = []
                    for record in session.run(top_nodes_query):
                        props = record['props'] or {}
                        label = props.get('name', f"{record['labels'][0]}_{record['id']}")
                        top_nodes.append({
                            'id': record['id'],
                            'label': label,
                            'type': record['labels'][0] if record['labels'] else 'Unknown',
                            'degree': record['degree']
                        })
                    
                    return jsonify({
                        'total_nodes': node_count,
                        'total_relationships': rel_count,
                        'top_connected_nodes': top_nodes
                    })
                    
            except Exception as e:
                self.logger.error(f"Error getting stats: {e}")
                return jsonify({'error': str(e)}), 500
    
    def start(self, auto_open: bool = True):
        """Start the visualization server.
        
        Args:
            auto_open: Whether to automatically open the browser
        """
        url = f"http://{self.host}:{self.port}"
        
        self.logger.info(f"Starting visualization server at {url}")
        
        if auto_open:
            # Open browser after a short delay
            import threading
            import time
            
            def open_browser():
                time.sleep(1.5)
                webbrowser.open(url)
            
            threading.Thread(target=open_browser).start()
        
        # Run the Flask app
        self.app.run(host=self.host, port=self.port, debug=False)
    
    def export_static_visualization(self, output_path: Path, graph_data: Optional[Dict] = None):
        """Export a static HTML visualization with embedded data and D3.js.
        
        Args:
            output_path: Path to save the HTML file
            graph_data: Optional pre-computed graph data
        """
        if graph_data is None:
            # Generate default overview graph
            graph_data = self.generator.generate_overview_graph()
        
        # Read the template
        template_path = Path(__file__).parent / 'templates' / 'index.html'
        with open(template_path, 'r') as f:
            html_content = f.read()
        
        # Read D3.js library
        d3_path = self.node_modules_path / 'd3' / 'dist' / 'd3.min.js'
        if d3_path.exists():
            with open(d3_path, 'r') as f:
                d3_content = f.read()
            # Replace the script src with inline script
            html_content = html_content.replace(
                '<script src="/static/d3.min.js"></script>',
                f'<script>\n{d3_content}\n</script>'
            )
        else:
            self.logger.warning("D3.js not found, exported file will require internet connection")
        
        # Inject the graph data
        data_script = f"""
        <script>
        // Embedded graph data
        window.embeddedGraphData = {json.dumps({
            'nodes': [node.__dict__ for node in graph_data.nodes],
            'edges': [edge.__dict__ for edge in graph_data.edges],
            'metadata': graph_data.metadata
        })};
        
        // Override the updateGraph function to use embedded data
        window.updateGraph = async function() {{
            graphData = window.embeddedGraphData;
            renderGraph();
            updateStats();
        }};
        
        // Hide controls that require server connection
        document.addEventListener('DOMContentLoaded', function() {{
            document.getElementById('update-graph').style.display = 'none';
            document.getElementById('search-group').style.display = 'none';
        }});
        </script>
        """
        
        # Insert before closing body tag
        html_content = html_content.replace('</body>', data_script + '</body>')
        
        # Save to file
        with open(output_path, 'w') as f:
            f.write(html_content)
        
        self.logger.info(f"Exported static visualization to {output_path}")