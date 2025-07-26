"""Visualization and analysis tools for LAPACK computational graph in Neo4j."""

from typing import Dict, List, Optional, Any
from .neo4j_client import Neo4jClient
import json
from pathlib import Path


class GraphVisualizer:
    """Tools for visualizing and analyzing LAPACK computational graphs."""
    
    def __init__(self, neo4j_client: Neo4jClient):
        """Initialize with a Neo4j client."""
        self.client = neo4j_client
    
    def get_graph_summary(self) -> Dict[str, Any]:
        """Get a comprehensive summary of the graph."""
        summary = self.client.verify_data()
        
        # Add more detailed metrics
        with self.client.driver.session() as session:
            # Get file statistics
            file_stats = session.run("""
                MATCH (f:File)
                WITH f, COUNT{(f)<-[:DEFINED_IN]-()} as routine_count
                RETURN count(f) as total_files,
                       avg(routine_count) as avg_routines_per_file,
                       max(routine_count) as max_routines_per_file
            """).single()
            
            summary['file_statistics'] = dict(file_stats)
            
            # Get connectivity metrics
            connectivity = session.run("""
                MATCH (r:Routine)
                WITH r, COUNT{(r)-[:CALLS]->()} as out_calls,
                     COUNT{(r)<-[:CALLS]-()} as in_calls
                RETURN avg(out_calls) as avg_out_calls,
                       max(out_calls) as max_out_calls,
                       avg(in_calls) as avg_in_calls,
                       max(in_calls) as max_in_calls,
                       count(CASE WHEN out_calls = 0 AND in_calls = 0 THEN 1 END) as isolated_routines,
                       count(CASE WHEN out_calls = 0 THEN 1 END) as leaf_routines,
                       count(CASE WHEN in_calls = 0 THEN 1 END) as root_routines
            """).single()
            
            summary['connectivity_metrics'] = dict(connectivity)
            
        return summary
    
    def get_top_hubs(self, limit: int = 20) -> List[Dict[str, Any]]:
        """Get the most connected routines (hubs) in the graph."""
        query = """
            MATCH (r:Routine)
            WITH r, 
                 COUNT{(r)-[:CALLS]->()} as out_calls,
                 COUNT{(r)<-[:CALLS]-()} as in_calls
            WITH r, out_calls, in_calls, 
                 out_calls + in_calls as total_calls
            WHERE total_calls > 0
            RETURN r.name as routine_name,
                   r.precision as precision,
                   r.category as category,
                   out_calls,
                   in_calls,
                   total_calls
            ORDER BY total_calls DESC
            LIMIT $limit
        """
        
        with self.client.driver.session() as session:
            result = session.run(query, limit=limit)
            return [dict(record) for record in result]
    
    def get_call_chain(self, routine_name: str, direction: str = "both", depth: int = 3) -> Dict[str, Any]:
        """Get the call chain for a specific routine.
        
        Args:
            routine_name: Name of the routine to analyze
            direction: "in" for callers, "out" for callees, "both" for both
            depth: Maximum depth of the call chain
        """
        result = {"routine": routine_name, "chains": {}}
        
        if direction in ["in", "both"]:
            # Get routines that call this one
            caller_query = """
                MATCH (target:Routine {name: $name})
                OPTIONAL MATCH path = (caller:Routine)-[:CALLS*1..$depth]->(target)
                WITH path, length(path) as chain_length
                WHERE path IS NOT NULL
                RETURN [n in nodes(path) | n.name] as chain, chain_length
                ORDER BY chain_length DESC
                LIMIT 50
            """
            with self.client.driver.session() as session:
                callers_result = session.run(caller_query, name=routine_name, depth=depth)
                callers = [dict(record) for record in callers_result]
            result["chains"]["callers"] = callers
        
        if direction in ["out", "both"]:
            # Get routines this one calls
            callee_query = """
                MATCH (source:Routine {name: $name})
                OPTIONAL MATCH path = (source)-[:CALLS*1..$depth]->(callee:Routine)
                WITH path, length(path) as chain_length
                WHERE path IS NOT NULL
                RETURN [n in nodes(path) | n.name] as chain, chain_length
                ORDER BY chain_length DESC
                LIMIT 50
            """
            with self.client.driver.session() as session:
                callees_result = session.run(callee_query, name=routine_name, depth=depth)
                callees = [dict(record) for record in callees_result]
            result["chains"]["callees"] = callees
        
        return result
    
    def find_strongly_connected_components(self, min_size: int = 3) -> List[Dict[str, Any]]:
        """Find strongly connected components (potential circular dependencies)."""
        # This is a simplified version - for full SCC, you'd want to use APOC
        query = """
            MATCH (r1:Routine)-[:CALLS]->(r2:Routine)-[:CALLS]->(r3:Routine)
            WHERE (r3)-[:CALLS*1..3]->(r1)
            WITH collect(DISTINCT r1.name) + collect(DISTINCT r2.name) + collect(DISTINCT r3.name) as component
            WITH component, size(component) as component_size
            WHERE component_size >= $min_size
            RETURN component, component_size
            ORDER BY component_size DESC
            LIMIT 20
        """
        
        with self.client.driver.session() as session:
            result = session.run(query, min_size=min_size)
            return [dict(record) for record in result]
    
    def analyze_precision_patterns(self) -> Dict[str, Any]:
        """Analyze how routines of different precisions interact."""
        query = """
            MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
            WITH r1.precision as from_precision, 
                 r2.precision as to_precision,
                 count(*) as call_count
            RETURN from_precision, to_precision, call_count
            ORDER BY call_count DESC
        """
        
        with self.client.driver.session() as session:
            precision_calls_result = session.run(query)
            precision_calls = [dict(record) for record in precision_calls_result]
        
        # Also get routine counts by precision
        with self.client.driver.session() as session:
            precision_counts_result = session.run("""
            MATCH (r:Routine)
            RETURN r.precision as precision, count(*) as count
            ORDER BY count DESC
        """)
            precision_counts = [dict(record) for record in precision_counts_result]
        
        return {
            "precision_interactions": precision_calls,
            "routines_by_precision": precision_counts
        }
    
    def analyze_categories(self) -> Dict[str, Any]:
        """Analyze routine categories and their interactions."""
        # Get category distributions
        category_query = """
            MATCH (r:Routine)
            WHERE r.category IS NOT NULL
            RETURN r.category as category, count(*) as count
            ORDER BY count DESC
        """
        with self.client.driver.session() as session:
            categories_result = session.run(category_query)
            categories = [dict(record) for record in categories_result]
        
        # Get category interactions
        interaction_query = """
            MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
            WHERE r1.category IS NOT NULL AND r2.category IS NOT NULL
            WITH r1.category as from_category,
                 r2.category as to_category,
                 count(*) as call_count
            RETURN from_category, to_category, call_count
            ORDER BY call_count DESC
            LIMIT 50
        """
        with self.client.driver.session() as session:
            interactions_result = session.run(interaction_query)
            interactions = [dict(record) for record in interactions_result]
        
        return {
            "category_distribution": categories,
            "category_interactions": interactions
        }
    
    def find_isolated_routines(self) -> List[Dict[str, Any]]:
        """Find routines with no connections."""
        query = """
            MATCH (r:Routine)
            WHERE NOT (r)-[:CALLS]-() AND NOT ()-[:CALLS]-(r)
            RETURN r.name as routine_name,
                   r.precision as precision,
                   r.category as category,
                   r.purpose as purpose
            ORDER BY r.name
        """
        
        with self.client.driver.session() as session:
            result = session.run(query)
            return [dict(record) for record in result]
    
    def get_file_coupling(self, limit: int = 20) -> List[Dict[str, Any]]:
        """Analyze coupling between files based on routine calls."""
        query = """
            MATCH (f1:File)<-[:DEFINED_IN]-(r1:Routine)-[:CALLS]->(r2:Routine)-[:DEFINED_IN]->(f2:File)
            WHERE f1 <> f2
            WITH f1.path as file1, f2.path as file2, count(*) as cross_file_calls
            RETURN file1, file2, cross_file_calls
            ORDER BY cross_file_calls DESC
            LIMIT $limit
        """
        
        with self.client.driver.session() as session:
            result = session.run(query, limit=limit)
            return [dict(record) for record in result]
    
    def export_for_visualization(self, output_file: str, 
                               max_nodes: int = 500,
                               routine_filter: Optional[str] = None) -> None:
        """Export a subset of the graph for visualization in tools like Gephi or D3.js."""
        
        # Build the query based on filters
        if routine_filter:
            node_query = """
                MATCH (r:Routine)
                WHERE r.name CONTAINS $filter
                WITH r LIMIT $max_nodes
                OPTIONAL MATCH (r)-[:CALLS]-(connected:Routine)
                WITH collect(DISTINCT r) + collect(DISTINCT connected) as nodes
                UNWIND nodes as n
                RETURN DISTINCT n.name as id, 
                       n.name as label,
                       n.precision as precision,
                       n.category as category,
                       labels(n)[0] as node_type
            """
            params = {"filter": routine_filter, "max_nodes": max_nodes}
        else:
            # Get the most connected nodes
            node_query = """
                MATCH (r:Routine)
                WITH r, COUNT{(r)-[:CALLS]-()} as degree
                ORDER BY degree DESC
                LIMIT $max_nodes
                RETURN r.name as id,
                       r.name as label,
                       r.precision as precision,
                       r.category as category,
                       'Routine' as node_type
            """
            params = {"max_nodes": max_nodes}
        
        with self.client.driver.session() as session:
            nodes_result = session.run(node_query, **params)
            nodes = [dict(record) for record in nodes_result]
        
        # Get relationships between these nodes
        node_ids = [n['id'] for n in nodes]
        
        edge_query = """
            MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
            WHERE r1.name IN $node_ids AND r2.name IN $node_ids
            RETURN r1.name as source, r2.name as target, 'CALLS' as type
        """
        
        with self.client.driver.session() as session:
            edges_result = session.run(edge_query, node_ids=node_ids)
            edges = [dict(record) for record in edges_result]
        
        # Create the export format
        export_data = {
            "nodes": nodes,
            "edges": edges,
            "metadata": {
                "total_nodes": len(nodes),
                "total_edges": len(edges),
                "filter": routine_filter or "top_connected",
                "max_nodes": max_nodes
            }
        }
        
        with open(output_file, 'w') as f:
            json.dump(export_data, f, indent=2)
        
        print(f"Exported {len(nodes)} nodes and {len(edges)} edges to {output_file}")
    
    def generate_visualization_queries(self) -> Dict[str, str]:
        """Generate useful Cypher queries for Neo4j Browser visualization."""
        queries = {
            "overview": """
                // Graph overview - sample of connections
                MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
                WITH r1, r2, rand() as random
                ORDER BY random
                LIMIT 100
                RETURN r1, r2
            """,
            
            "top_hubs": """
                // Most connected routines
                MATCH (r:Routine)
                WITH r, COUNT{(r)-[:CALLS]-()} as degree
                ORDER BY degree DESC
                LIMIT 25
                MATCH (r)-[rel:CALLS]-(connected)
                RETURN r, rel, connected
            """,
            
            "precision_network": """
                // Network colored by precision
                MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
                WHERE r1.precision IN ['d', 's'] AND r2.precision IN ['d', 's']
                WITH r1, r2, rand() as random
                ORDER BY random
                LIMIT 100
                RETURN r1, r2
            """,
            
            "file_clusters": """
                // Routines clustered by file
                MATCH (f:File)<-[:DEFINED_IN]-(r:Routine)
                WITH f, collect(r) as routines
                WHERE size(routines) > 5
                LIMIT 5
                UNWIND routines as r
                OPTIONAL MATCH (r)-[c:CALLS]-(other:Routine)
                WHERE other IN routines
                RETURN f, r, c, other
            """,
            
            "call_depth_3": """
                // Call chains of depth 3
                MATCH path = (r1:Routine)-[:CALLS*3]->(r3:Routine)
                WHERE r1.name STARTS WITH 'dge'
                WITH path LIMIT 20
                RETURN path
            """,
            
            "isolated_components": """
                // Find isolated groups
                MATCH (r:Routine)
                WHERE NOT (r)-[:CALLS]-()
                RETURN r
                LIMIT 50
            """
        }
        
        return queries