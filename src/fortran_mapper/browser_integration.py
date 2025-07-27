#!/usr/bin/env python3
"""
Neo4j Browser integration for LAPACK graph visualization.

This module provides methods to send queries directly to Neo4j Browser
for visualization without manual copy-pasting.
"""

import json
import webbrowser
import urllib.parse
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime

# Make requests optional
try:
    import requests
    HAS_REQUESTS = True
except ImportError:
    HAS_REQUESTS = False


class Neo4jBrowserIntegration:
    """Integration with Neo4j Browser for direct query visualization."""
    
    def __init__(self, host: str = "localhost", port: int = 7474):
        """Initialize browser integration.
        
        Args:
            host: Neo4j server host
            port: Neo4j browser port (default 7474)
        """
        self.host = host
        self.port = port
        self.browser_url = f"http://{host}:{port}/browser/"
        
    def open_with_query(self, query: str, open_browser: bool = True) -> str:
        """Open Neo4j Browser with a pre-populated query.
        
        Args:
            query: Cypher query to execute
            open_browser: Whether to automatically open the browser
            
        Returns:
            The URL that can be used to open the browser with the query
        """
        # Neo4j Browser accepts queries via URL parameters
        encoded_query = urllib.parse.quote(query)
        
        # The browser accepts queries in the cmd parameter
        url = f"{self.browser_url}?cmd=edit&arg={encoded_query}"
        
        if open_browser:
            webbrowser.open(url)
            print(f"Opened Neo4j Browser with query")
        
        return url
    
    def create_guide(self, queries: Dict[str, str], output_file: str = "neo4j_guide.html") -> Path:
        """Create a Neo4j Browser guide with clickable queries.
        
        Neo4j Browser guides are HTML files that can be loaded directly
        into the browser and provide clickable query execution.
        
        Args:
            queries: Dictionary of query names to Cypher queries
            output_file: Path to save the guide HTML
            
        Returns:
            Path to the created guide file
        """
        guide_html = """<!DOCTYPE html>
<html>
<head>
    <title>LAPACK Graph Analysis Guide</title>
    <style>
        body {{
            font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
            margin: 20px;
            line-height: 1.6;
        }}
        h1, h2 {{
            color: #1e3a5f;
        }}
        .query-section {{
            margin: 20px 0;
            padding: 15px;
            background-color: #f5f5f5;
            border-radius: 5px;
        }}
        pre {{
            background-color: #2e3440;
            color: #d8dee9;
            padding: 15px;
            border-radius: 5px;
            overflow-x: auto;
        }}
        .run-button {{
            background-color: #008cc1;
            color: white;
            padding: 8px 16px;
            border: none;
            border-radius: 3px;
            cursor: pointer;
            text-decoration: none;
            display: inline-block;
            margin-top: 10px;
        }}
        .run-button:hover {{
            background-color: #006fa1;
        }}
        .description {{
            color: #666;
            font-style: italic;
            margin-bottom: 10px;
        }}
    </style>
</head>
<body>
    <h1>LAPACK Computational Graph Analysis</h1>
    <p>Generated: {timestamp}</p>
    <p>Click the "Run Query" buttons to execute queries directly in Neo4j Browser.</p>
    
    <h2>Visualization Queries</h2>
"""
        
        for name, query in queries.items():
            # Create a clickable link that Neo4j Browser understands
            encoded_query = urllib.parse.quote(query)
            neo4j_link = f":play {encoded_query}"
            
            # For direct execution, use the command format
            run_link = f"http://localhost:7474/browser/?cmd=edit&arg={encoded_query}"
            
            # Escape query for HTML
            escaped_query = query.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
            
            guide_html += f"""
    <div class="query-section">
        <h3>{name}</h3>
        <pre>{escaped_query}</pre>
        <a href="{run_link}" class="run-button" target="_blank">Run Query</a>
    </div>
"""
        
        guide_html += """
</body>
</html>
"""
        
        # Replace the timestamp placeholder
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        guide_html = guide_html.replace("{timestamp}", timestamp)
        
        output_path = Path(output_file)
        output_path.write_text(guide_html)
        
        print(f"Created Neo4j guide: {output_path}")
        print(f"To use: Open Neo4j Browser and drag this file into it, or use ':play {output_path.absolute()}'")
        
        return output_path
    
    def create_cypher_script(self, queries: Dict[str, str], output_file: str = "lapack_queries.cypher") -> Path:
        """Create a Cypher script file that can be loaded in Neo4j Browser.
        
        Args:
            queries: Dictionary of query names to Cypher queries
            output_file: Path to save the Cypher script
            
        Returns:
            Path to the created script file
        """
        script_content = f"""// LAPACK Graph Analysis Queries
// Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
// 
// To use in Neo4j Browser:
// 1. Copy individual queries and paste in the query editor
// 2. Or use the browser integration tools to open directly

"""
        
        for name, query in queries.items():
            script_content += f"""
// ==========================================
// {name}
// ==========================================
{query}

"""
        
        output_path = Path(output_file)
        output_path.write_text(script_content)
        
        print(f"Created Cypher script: {output_path}")
        
        return output_path
    
    def send_to_browser_api(self, query: str, database: str = "neo4j") -> Optional[Dict]:
        """Send query via Neo4j Browser API (if available).
        
        Note: This requires Neo4j Browser to be configured to accept external requests.
        
        Args:
            query: Cypher query to execute
            database: Database name
            
        Returns:
            Query result if successful, None otherwise
        """
        if not HAS_REQUESTS:
            print("Note: 'requests' package not available. Using URL-based method only.")
            return None
            
        try:
            # Try to use the browser API endpoint
            api_url = f"http://{self.host}:{self.port}/db/{database}/tx/commit"
            
            payload = {
                "statements": [{
                    "statement": query,
                    "resultDataContents": ["graph", "row"]
                }]
            }
            
            headers = {
                "Content-Type": "application/json",
                "Accept": "application/json"
            }
            
            response = requests.post(api_url, json=payload, headers=headers)
            
            if response.status_code == 200:
                return response.json()
            else:
                print(f"API request failed: {response.status_code}")
                return None
                
        except Exception as e:
            print(f"Could not send via API: {e}")
            print("Falling back to URL-based method")
            return None


def create_visualization_suite():
    """Create a comprehensive visualization suite for LAPACK graph."""
    
    # Import the query templates
    from query_templates import QUERY_TEMPLATES, get_routine_dependencies, get_routine_neighborhood
    
    # Define visualization queries
    viz_queries = {
        "Graph Overview": """
// Sample of graph structure
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WITH r1, r2, rand() as random
ORDER BY random
LIMIT 150
RETURN r1, r2
""",
        
        "Most Connected Hubs": """
// Top 25 most connected routines
MATCH (r:Routine)
WITH r, COUNT{(r)-[:CALLS]-()} as degree
ORDER BY degree DESC
LIMIT 25
MATCH (r)-[rel:CALLS]-(connected)
RETURN r, rel, connected
""",
        
        "Precision Network": """
// How different precisions interact
MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
WHERE r1.precision IS NOT NULL AND r2.precision IS NOT NULL
WITH r1, r2, r1.precision as p1, r2.precision as p2
WHERE p1 <> p2
LIMIT 100
RETURN r1, r2
""",
        
        "XERBLA Error Network": """
// Error handling network around XERBLA
MATCH (xerbla:Routine {name: 'XERBLA'})
MATCH (caller)-[:CALLS]->(xerbla)
WITH xerbla, caller
LIMIT 50
RETURN xerbla, caller
""",
        
        "File Clusters": """
// Routines grouped by source file
MATCH (f:File)<-[:DEFINED_IN]-(r:Routine)
WITH f, collect(r) as routines
WHERE size(routines) > 5 AND size(routines) < 20
LIMIT 3
UNWIND routines as r
OPTIONAL MATCH (r)-[c:CALLS]-(other:Routine)
WHERE other IN routines
RETURN f, r, c, other
""",
        
        "BLAS Level 3 Network": """
// BLAS Level 3 routine interactions
MATCH (r:Routine {type: 'blas3'})
OPTIONAL MATCH (r)-[c:CALLS]-(connected:Routine)
RETURN r, c, connected
LIMIT 100
""",
        
        "Factorization Routines": """
// All factorization routines and their connections
MATCH (r:Routine)
WHERE r.category CONTAINS 'factorization'
OPTIONAL MATCH (r)-[c:CALLS]-(connected:Routine)
WITH r, c, connected
LIMIT 200
RETURN r, c, connected
""",
        
        "Isolated Components": """
// Find isolated groups of routines
MATCH (r1:Routine)-[:CALLS]-(r2:Routine)
WHERE NOT (r1)-[:CALLS]-(:Routine)-[:CALLS]-(r2)
WITH r1, collect(r2) as group
WHERE size(group) > 2
RETURN r1, group[0..5] as sample_group
LIMIT 10
"""
    }
    
    return viz_queries


def main():
    """Demonstrate browser integration features."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Neo4j Browser integration for LAPACK visualization')
    parser.add_argument('--host', default='localhost', help='Neo4j host')
    parser.add_argument('--port', type=int, default=7474, help='Neo4j browser port')
    parser.add_argument('--query', help='Single query to open in browser')
    parser.add_argument('--guide', action='store_true', help='Create HTML guide')
    parser.add_argument('--script', action='store_true', help='Create Cypher script')
    parser.add_argument('--open', action='store_true', help='Open query in browser')
    
    args = parser.parse_args()
    
    browser = Neo4jBrowserIntegration(args.host, args.port)
    
    if args.query:
        # Open a single query
        url = browser.open_with_query(args.query, args.open)
        print(f"Query URL: {url}")
        
    elif args.guide:
        # Create HTML guide
        queries = create_visualization_suite()
        guide_path = browser.create_guide(queries, "visualizations/lapack_neo4j_guide.html")
        print(f"\nGuide created: {guide_path}")
        print("\nTo use the guide:")
        print("1. Open Neo4j Browser")
        print(f"2. Drag and drop '{guide_path}' into the browser")
        print("3. Or type: :play " + str(guide_path.absolute()))
        
    elif args.script:
        # Create Cypher script
        queries = create_visualization_suite()
        script_path = browser.create_cypher_script(queries, "visualizations/lapack_queries.cypher")
        
    else:
        # Demo: Open most connected routines query
        demo_query = """
MATCH (r:Routine)
WITH r, COUNT{(r)-[:CALLS]-()} as degree
ORDER BY degree DESC
LIMIT 25
MATCH (r)-[rel:CALLS]-(connected)
RETURN r, rel, connected
"""
        print("Demo: Opening Neo4j Browser with 'Most Connected Routines' query...")
        url = browser.open_with_query(demo_query, args.open)
        print(f"\nBrowser URL: {url}")
        print("\nYou can also:")
        print("1. Run with --guide to create an HTML guide")
        print("2. Run with --script to create a Cypher script file")
        print("3. Run with --query 'YOUR QUERY' to open a specific query")


if __name__ == '__main__':
    main()