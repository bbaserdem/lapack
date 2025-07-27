"""Common Cypher query templates for LAPACK graph analysis."""

# Basic statistics queries
QUERY_TEMPLATES = {
    # Overview queries
    "node_counts": """
        MATCH (n)
        RETURN labels(n)[0] as label, count(n) as count
        ORDER BY count DESC
    """,
    
    "relationship_counts": """
        MATCH ()-[r]->()
        RETURN type(r) as type, count(r) as count
        ORDER BY count DESC
    """,
    
    # Routine analysis
    "routine_by_precision": """
        MATCH (r:Routine)
        WHERE r.precision IS NOT NULL
        RETURN r.precision as precision, count(r) as count
        ORDER BY count DESC
    """,
    
    "routine_by_category": """
        MATCH (r:Routine)
        WHERE r.category IS NOT NULL
        RETURN r.category as category, count(r) as count
        ORDER BY count DESC
        LIMIT 25
    """,
    
    "most_called_routines": """
        MATCH (caller)-[:CALLS]->(r:Routine)
        RETURN r.name as routine, r.precision as precision, r.category as category,
               count(caller) as call_count
        ORDER BY call_count DESC
        LIMIT 20
    """,
    
    "routines_with_most_dependencies": """
        MATCH (r:Routine)-[:CALLS]->(dependency)
        RETURN r.name as routine, r.precision as precision, 
               count(DISTINCT dependency) as dependency_count
        ORDER BY dependency_count DESC
        LIMIT 20
    """,
    
    # Connectivity patterns
    "precision_call_matrix": """
        MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
        WHERE r1.precision IS NOT NULL AND r2.precision IS NOT NULL
        RETURN r1.precision as from_precision, r2.precision as to_precision, 
               count(*) as call_count
        ORDER BY call_count DESC
    """,
    
    "category_interaction_matrix": """
        MATCH (r1:Routine)-[:CALLS]->(r2:Routine)
        WHERE r1.category IS NOT NULL AND r2.category IS NOT NULL
        RETURN r1.category as from_category, r2.category as to_category, 
               count(*) as call_count
        ORDER BY call_count DESC
        LIMIT 50
    """,
    
    # File analysis
    "files_with_most_routines": """
        MATCH (f:File)<-[:DEFINED_IN]-(r:Routine)
        RETURN f.path as file, count(r) as routine_count
        ORDER BY routine_count DESC
        LIMIT 20
    """,
    
    "file_dependency_graph": """
        MATCH (f1:File)<-[:DEFINED_IN]-(r1:Routine)-[:CALLS]->(r2:Routine)-[:DEFINED_IN]->(f2:File)
        WHERE f1 <> f2
        RETURN f1.path as source_file, f2.path as target_file, count(*) as dependency_count
        ORDER BY dependency_count DESC
        LIMIT 30
    """,
    
    # Complex analysis
    "isolated_routines": """
        MATCH (r:Routine)
        WHERE NOT (r)-[:CALLS]->() AND NOT (r)<-[:CALLS]-()
        RETURN r.name as routine, r.precision as precision, r.category as category
        ORDER BY r.name
    """,
    
    "leaf_routines": """
        MATCH (r:Routine)
        WHERE NOT (r)-[:CALLS]->() AND (r)<-[:CALLS]-()
        RETURN r.name as routine, r.precision as precision, r.category as category,
               count{(r)<-[:CALLS]-()} as incoming_calls
        ORDER BY incoming_calls DESC
        LIMIT 20
    """,
    
    "root_routines": """
        MATCH (r:Routine)
        WHERE (r)-[:CALLS]->() AND NOT (r)<-[:CALLS]-()
        RETURN r.name as routine, r.precision as precision, r.category as category,
               count{(r)-[:CALLS]->()} as outgoing_calls
        ORDER BY outgoing_calls DESC
        LIMIT 20
    """,
    
    # Path analysis
    "shortest_path_between": """
        MATCH path = shortestPath((r1:Routine {name: $routine1})-[:CALLS*]-(r2:Routine {name: $routine2}))
        RETURN [n IN nodes(path) | n.name] as path, length(path) as path_length
    """,
    
    "all_paths_from_routine": """
        MATCH path = (r:Routine {name: $routine})-[:CALLS*..3]->()
        RETURN [n IN nodes(path) | n.name] as path, length(path) as path_length
        ORDER BY path_length
        LIMIT 20
    """,
    
    # Cycle detection
    "detect_cycles": """
        MATCH path = (r:Routine)-[:CALLS*]->(r)
        RETURN [n IN nodes(path) | n.name] as cycle, length(path) as cycle_length
        ORDER BY cycle_length
        LIMIT 10
    """,
    
    # Community detection preparation
    "routine_clustering_data": """
        MATCH (r1:Routine)-[:CALLS]-(r2:Routine)
        RETURN r1.name as routine1, r2.name as routine2, 
               r1.category as category1, r2.category as category2
    """
}


def get_routine_dependencies(routine_name: str, depth: int = 2) -> str:
    """Generate query to get dependencies of a specific routine."""
    return f"""
        MATCH path = (r:Routine {{name: '{routine_name}'}})-[:CALLS*..{depth}]->(dep:Routine)
        RETURN DISTINCT dep.name as dependency, dep.precision as precision, 
               dep.category as category, length(path) as distance
        ORDER BY distance, dependency
    """


def get_routine_dependents(routine_name: str, depth: int = 2) -> str:
    """Generate query to get routines that depend on a specific routine."""
    return f"""
        MATCH path = (dep:Routine)-[:CALLS*..{depth}]->(r:Routine {{name: '{routine_name}'}})
        RETURN DISTINCT dep.name as dependent, dep.precision as precision, 
               dep.category as category, length(path) as distance
        ORDER BY distance, dependent
    """


def get_routine_neighborhood(routine_name: str, depth: int = 1) -> str:
    """Generate query to get the immediate neighborhood of a routine."""
    return f"""
        MATCH (r:Routine {{name: '{routine_name}'}})
        OPTIONAL MATCH (r)-[:CALLS]->(called:Routine)
        OPTIONAL MATCH (caller:Routine)-[:CALLS]->(r)
        RETURN r.name as routine, r.precision as precision, r.category as category,
               collect(DISTINCT called.name) as calls,
               collect(DISTINCT caller.name) as called_by
    """


def search_routines_by_pattern(pattern: str) -> str:
    """Generate query to search routines by name pattern."""
    return f"""
        MATCH (r:Routine)
        WHERE r.name CONTAINS '{pattern.upper()}'
        RETURN r.name as routine, r.precision as precision, r.category as category
        ORDER BY r.name
        LIMIT 50
    """


def get_precision_migration_candidates() -> str:
    """Find routines that might benefit from precision migration."""
    return """
        MATCH (r:Routine)
        WHERE r.precision = 'single' OR r.precision = 'double'
        OPTIONAL MATCH (r)-[:CALLS]->(dep:Routine)
        WHERE dep.precision <> r.precision
        WITH r, count(DISTINCT dep) as mixed_precision_deps
        WHERE mixed_precision_deps > 0
        RETURN r.name as routine, r.precision as current_precision, 
               mixed_precision_deps, r.category as category
        ORDER BY mixed_precision_deps DESC
        LIMIT 30
    """