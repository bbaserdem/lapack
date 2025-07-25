#!/usr/bin/env python3
"""
Demo script showing how to use the enhanced error tracking in Neo4j
"""

import sys
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from lapack_util.lapack_to_neo4j import LAPACKGraphBuilder


def main():
    # Initialize the graph builder
    lapack_root = Path("/path/to/lapack")  # Update this path
    builder = LAPACKGraphBuilder(lapack_root)
    
    # Parse sources - now returns both routines and errors
    routines_by_file, errors_by_file = builder.parse_sources(["BLAS/SRC", "SRC"])
    
    # Show error statistics
    print(f"\n=== Error Statistics ===")
    print(f"Total files with errors: {len(errors_by_file)}")
    print(f"Total errors: {sum(len(errors) for errors in errors_by_file.values())}")
    
    # Show sample errors
    if errors_by_file:
        print("\n=== Sample Errors ===")
        for file_path, errors in list(errors_by_file.items())[:3]:
            print(f"\nFile: {file_path}")
            for error in errors[:2]:  # Show first 2 errors per file
                print(f"  - Type: {error['type']}")
                print(f"    Severity: {error['severity']}")
                print(f"    Message: {error['message'][:100]}...")
                if error.get('line_number'):
                    print(f"    Line: {error['line_number']}")
    
    # Build graph schema with errors included
    schema = builder.build_graph_schema(routines_by_file, errors_by_file, include_errors=True)
    
    # Count error nodes
    error_nodes = [n for n in schema.nodes if n.node_type.value == "ParseError"]
    print(f"\n=== Graph Schema Stats ===")
    print(f"Total nodes: {len(schema.nodes)}")
    print(f"Error nodes: {len(error_nodes)}")
    print(f"Total relationships: {len(schema.relationships)}")
    
    # Export schema with errors
    output_dir = Path("output_with_errors")
    builder.export_schema(output_dir)
    print(f"\nExported schema to {output_dir}")
    
    # Example: Load to Neo4j (uncomment and configure)
    # builder.load_to_neo4j(
    #     uri="bolt://localhost:7687",
    #     username="neo4j",
    #     password="your_password"
    # )


if __name__ == "__main__":
    main()