#!/usr/bin/env python3
"""
Example: Find circular dependencies in LAPACK/BLAS
"""

from pathlib import Path
import sys
from collections import defaultdict, deque

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.lapack_util.fortran_parser import FortranParser


def build_call_graph(parser):
    """Build a directed graph of routine calls."""
    graph = defaultdict(set)
    
    for file_path, file_data in parser._parsed_files.items():
        for routine in file_data.routines:
            if routine.calls:
                graph[routine.name].update(routine.calls)
    
    return graph


def find_cycles_dfs(graph):
    """Find all cycles in the call graph using DFS."""
    cycles = []
    visited = set()
    rec_stack = set()
    path = []
    
    def dfs(node):
        visited.add(node)
        rec_stack.add(node)
        path.append(node)
        
        if node in graph:
            for neighbor in graph[node]:
                if neighbor not in visited:
                    if dfs(neighbor):
                        return True
                elif neighbor in rec_stack:
                    # Found a cycle
                    cycle_start_idx = path.index(neighbor)
                    cycle = path[cycle_start_idx:] + [neighbor]
                    cycles.append(cycle)
        
        path.pop()
        rec_stack.remove(node)
        return False
    
    # Check all nodes
    for node in graph:
        if node not in visited:
            dfs(node)
    
    return cycles


def find_strongly_connected_components(graph):
    """Find strongly connected components using Tarjan's algorithm."""
    index = 0
    stack = []
    indices = {}
    lowlinks = {}
    on_stack = set()
    sccs = []
    
    def strongconnect(v):
        nonlocal index
        indices[v] = index
        lowlinks[v] = index
        index += 1
        stack.append(v)
        on_stack.add(v)
        
        if v in graph:
            for w in graph[v]:
                if w not in indices:
                    strongconnect(w)
                    lowlinks[v] = min(lowlinks[v], lowlinks[w])
                elif w in on_stack:
                    lowlinks[v] = min(lowlinks[v], indices[w])
        
        if lowlinks[v] == indices[v]:
            scc = []
            while True:
                w = stack.pop()
                on_stack.remove(w)
                scc.append(w)
                if w == v:
                    break
            if len(scc) > 1:  # Only keep non-trivial SCCs
                sccs.append(scc)
    
    for v in graph:
        if v not in indices:
            strongconnect(v)
    
    return sccs


def find_shortest_cycle(graph, start_node):
    """Find the shortest cycle containing start_node using BFS."""
    queue = deque([(start_node, [start_node])])
    visited = {start_node: 0}
    
    while queue:
        node, path = queue.popleft()
        
        if node in graph:
            for neighbor in graph[node]:
                if neighbor == start_node and len(path) > 1:
                    # Found a cycle back to start
                    return path + [start_node]
                
                if neighbor not in visited or visited[neighbor] > len(path):
                    visited[neighbor] = len(path)
                    queue.append((neighbor, path + [neighbor]))
    
    return None


def analyze_cycle_impact(graph, cycle):
    """Analyze the impact of a cycle by finding all routines that depend on it."""
    cycle_nodes = set(cycle[:-1])  # Exclude repeated last node
    impacted = set()
    
    # Find all nodes that can reach any node in the cycle
    for node in graph:
        if node not in cycle_nodes:
            # BFS to check if this node can reach the cycle
            queue = deque([node])
            visited = set([node])
            
            while queue:
                current = queue.popleft()
                if current in cycle_nodes:
                    impacted.add(node)
                    break
                
                if current in graph:
                    for neighbor in graph[current]:
                        if neighbor not in visited:
                            visited.add(neighbor)
                            queue.append(neighbor)
    
    return impacted


def main():
    # Parse source files
    parser = FortranParser()
    
    print("Parsing source files...")
    
    # Parse BLAS
    blas_path = Path("BLAS/SRC")
    if blas_path.exists():
        for f_file in blas_path.glob("*.f"):
            parser.parse_file(f_file)
    
    # Parse LAPACK (limited to avoid too many files)
    lapack_path = Path("SRC")
    if lapack_path.exists():
        # Just parse a subset for demonstration
        count = 0
        for f_file in lapack_path.glob("*.f"):
            parser.parse_file(f_file)
            count += 1
            if count >= 100:  # Limit for faster analysis
                break
    
    print(f"Parsed {len(parser._parsed_files)} files")
    
    # Build call graph
    graph = build_call_graph(parser)
    print(f"Call graph has {len(graph)} nodes")
    
    # Find strongly connected components
    print("\n=== Strongly Connected Components ===")
    sccs = find_strongly_connected_components(graph)
    
    if not sccs:
        print("No circular dependencies found!")
    else:
        print(f"Found {len(sccs)} strongly connected components:")
        
        for i, scc in enumerate(sccs[:10]):  # Show first 10
            print(f"\nComponent {i+1} ({len(scc)} routines):")
            print(f"  Members: {', '.join(scc[:5])}")
            if len(scc) > 5:
                print(f"  ... and {len(scc)-5} more")
            
            # Find shortest cycle in this component
            cycle = find_shortest_cycle(graph, scc[0])
            if cycle:
                print(f"  Shortest cycle: {' -> '.join(cycle)}")
            
            # Analyze impact
            impacted = analyze_cycle_impact(graph, cycle if cycle else scc + [scc[0]])
            print(f"  Impacted routines: {len(impacted)}")
    
    # Look for specific patterns
    print("\n=== Mutual Dependencies ===")
    mutual_deps = []
    
    for node in graph:
        for neighbor in graph[node]:
            if neighbor in graph and node in graph[neighbor] and node < neighbor:
                mutual_deps.append((node, neighbor))
    
    if mutual_deps:
        print(f"Found {len(mutual_deps)} mutual dependencies:")
        for a, b in mutual_deps[:10]:
            print(f"  {a} <-> {b}")
    else:
        print("No mutual dependencies found")
    
    # Find self-loops
    print("\n=== Self-Loops ===")
    self_loops = [node for node in graph if node in graph[node]]
    
    if self_loops:
        print(f"Found {len(self_loops)} routines that call themselves:")
        for routine in self_loops:
            print(f"  {routine}")
    else:
        print("No self-loops found")
    
    # Find potential indirect recursion
    print("\n=== Potential Indirect Recursion ===")
    print("(Cycles of length 2-4)")
    
    short_cycles = []
    for node in graph:
        for length in range(2, 5):
            cycle = find_shortest_cycle(graph, node)
            if cycle and len(cycle) == length + 1:  # +1 because cycle includes start node twice
                cycle_normalized = tuple(sorted(cycle[:-1]))
                if cycle_normalized not in short_cycles:
                    short_cycles.append(cycle_normalized)
    
    if short_cycles:
        print(f"Found {len(short_cycles)} short cycles:")
        for cycle in short_cycles[:10]:
            print(f"  {' -> '.join(cycle)} -> {cycle[0]}")
    
    # Summary
    print("\n=== Summary ===")
    total_cycles = len(sccs) + len(self_loops)
    print(f"Total circular dependency issues: {total_cycles}")
    
    if total_cycles > 0:
        print("\nRecommendations:")
        print("1. Review strongly connected components for refactoring opportunities")
        print("2. Check if mutual dependencies can be resolved by extracting common functionality")
        print("3. Verify that self-loops (if any) are intentional recursive algorithms")


if __name__ == "__main__":
    main()