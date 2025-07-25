#!/usr/bin/env python3
"""
Example: Export LAPACK/BLAS parsing results to different formats
"""

from pathlib import Path
import sys
import json
import csv
import xml.etree.ElementTree as ET
from collections import defaultdict

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.lapack_util.fortran_parser import FortranParser


def export_to_json(parser, output_file="lapack_graph.json"):
    """Export parsing results to JSON format."""
    data = {
        "metadata": {
            "total_files": len(parser._parsed_files),
            "total_routines": sum(len(f.routines) for f in parser._parsed_files.values())
        },
        "files": {},
        "routines": {},
        "dependencies": []
    }
    
    # Collect file and routine data
    for file_path, file_data in parser._parsed_files.items():
        data["files"][str(file_path)] = {
            "routine_count": len(file_data.routines),
            "routines": [r.name for r in file_data.routines]
        }
        
        for routine in file_data.routines:
            data["routines"][routine.name] = {
                "file": str(file_path),
                "type": routine.routine_type,
                "precision": routine.precision,
                "operation": routine.operation,
                "lines": [routine.line_start, routine.line_end],
                "calls": list(routine.calls) if routine.calls else []
            }
            
            # Add dependencies
            if routine.calls:
                for called in routine.calls:
                    data["dependencies"].append({
                        "from": routine.name,
                        "to": called
                    })
    
    with open(output_file, 'w') as f:
        json.dump(data, f, indent=2)
    
    print(f"Exported to {output_file}")
    return data


def export_to_csv(parser, prefix="lapack"):
    """Export parsing results to CSV files."""
    
    # Export routines to CSV
    routines_file = f"{prefix}_routines.csv"
    with open(routines_file, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['name', 'file', 'type', 'precision', 'operation', 'line_start', 'line_end'])
        
        for file_path, file_data in parser._parsed_files.items():
            for routine in file_data.routines:
                writer.writerow([
                    routine.name,
                    str(file_path),
                    routine.routine_type,
                    routine.precision or '',
                    routine.operation or '',
                    routine.line_start,
                    routine.line_end
                ])
    
    print(f"Exported routines to {routines_file}")
    
    # Export dependencies to CSV
    deps_file = f"{prefix}_dependencies.csv"
    with open(deps_file, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['caller', 'callee'])
        
        for file_path, file_data in parser._parsed_files.items():
            for routine in file_data.routines:
                if routine.calls:
                    for called in routine.calls:
                        writer.writerow([routine.name, called])
    
    print(f"Exported dependencies to {deps_file}")


def export_to_graphml(parser, output_file="lapack_graph.graphml"):
    """Export to GraphML format for visualization tools."""
    
    # Create root element
    graphml = ET.Element('graphml', xmlns="http://graphml.graphdrawing.org/xmlns")
    
    # Define attributes
    key_name = ET.SubElement(graphml, 'key', id="name", **{'for': 'node', 'attr.name': 'name', 'attr.type': 'string'})
    key_type = ET.SubElement(graphml, 'key', id="type", **{'for': 'node', 'attr.name': 'type', 'attr.type': 'string'})
    key_precision = ET.SubElement(graphml, 'key', id="precision", **{'for': 'node', 'attr.name': 'precision', 'attr.type': 'string'})
    key_file = ET.SubElement(graphml, 'key', id="file", **{'for': 'node', 'attr.name': 'file', 'attr.type': 'string'})
    
    # Create graph
    graph = ET.SubElement(graphml, 'graph', id="G", edgedefault="directed")
    
    # Add nodes (routines)
    routine_ids = {}
    node_id = 0
    
    for file_path, file_data in parser._parsed_files.items():
        for routine in file_data.routines:
            node = ET.SubElement(graph, 'node', id=str(node_id))
            
            data_name = ET.SubElement(node, 'data', key="name")
            data_name.text = routine.name
            
            data_type = ET.SubElement(node, 'data', key="type")
            data_type.text = routine.routine_type
            
            if routine.precision:
                data_precision = ET.SubElement(node, 'data', key="precision")
                data_precision.text = routine.precision
            
            data_file = ET.SubElement(node, 'data', key="file")
            data_file.text = str(file_path)
            
            routine_ids[routine.name] = node_id
            node_id += 1
    
    # Add edges (dependencies)
    edge_id = 0
    for file_path, file_data in parser._parsed_files.items():
        for routine in file_data.routines:
            if routine.calls:
                source_id = routine_ids.get(routine.name)
                if source_id is not None:
                    for called in routine.calls:
                        target_id = routine_ids.get(called)
                        if target_id is not None:
                            edge = ET.SubElement(graph, 'edge', 
                                               id=str(edge_id),
                                               source=str(source_id),
                                               target=str(target_id))
                            edge_id += 1
    
    # Write to file
    tree = ET.ElementTree(graphml)
    tree.write(output_file, encoding='utf-8', xml_declaration=True)
    
    print(f"Exported to {output_file}")
    print(f"  Nodes: {node_id}")
    print(f"  Edges: {edge_id}")


def export_to_dot(parser, output_file="lapack_graph.dot"):
    """Export to Graphviz DOT format."""
    
    with open(output_file, 'w') as f:
        f.write("digraph LAPACK {\n")
        f.write("  rankdir=LR;\n")
        f.write("  node [shape=box];\n\n")
        
        # Group by precision for coloring
        precision_colors = {
            's': 'lightblue',
            'd': 'lightgreen',
            'c': 'lightyellow',
            'z': 'lightpink'
        }
        
        # Add nodes grouped by precision
        routines_by_precision = defaultdict(list)
        for file_path, file_data in parser._parsed_files.items():
            for routine in file_data.routines:
                prec = routine.precision or 'unknown'
                routines_by_precision[prec].append(routine)
        
        # Write nodes
        for prec, routines in routines_by_precision.items():
            color = precision_colors.get(prec, 'white')
            f.write(f"  // {prec} precision routines\n")
            for routine in routines:
                label = f"{routine.name}\\n({routine.routine_type})"
                f.write(f'  "{routine.name}" [label="{label}", fillcolor={color}, style=filled];\n')
            f.write("\n")
        
        # Write edges
        f.write("  // Dependencies\n")
        for file_path, file_data in parser._parsed_files.items():
            for routine in file_data.routines:
                if routine.calls:
                    for called in routine.calls:
                        f.write(f'  "{routine.name}" -> "{called}";\n')
        
        f.write("}\n")
    
    print(f"Exported to {output_file}")
    print("To visualize: dot -Tpng lapack_graph.dot -o lapack_graph.png")


def create_summary_report(parser, output_file="lapack_summary.txt"):
    """Create a human-readable summary report."""
    
    with open(output_file, 'w') as f:
        f.write("LAPACK/BLAS Code Analysis Summary\n")
        f.write("=" * 50 + "\n\n")
        
        # Basic statistics
        total_files = len(parser._parsed_files)
        total_routines = sum(len(f.routines) for f in parser._parsed_files.values())
        
        f.write(f"Total files parsed: {total_files}\n")
        f.write(f"Total routines found: {total_routines}\n\n")
        
        # Files with most routines
        f.write("Files with most routines:\n")
        file_counts = [(path, len(data.routines)) 
                      for path, data in parser._parsed_files.items()]
        file_counts.sort(key=lambda x: x[1], reverse=True)
        
        for path, count in file_counts[:10]:
            f.write(f"  {path}: {count} routines\n")
        
        # Precision distribution
        f.write("\nPrecision distribution:\n")
        precision_counts = defaultdict(int)
        for file_data in parser._parsed_files.values():
            for routine in file_data.routines:
                if routine.precision:
                    precision_counts[routine.precision] += 1
        
        for prec in ['s', 'd', 'c', 'z']:
            count = precision_counts[prec]
            pct = count / total_routines * 100 if total_routines > 0 else 0
            f.write(f"  {prec}: {count} ({pct:.1f}%)\n")
        
        # Most called routines
        f.write("\nMost frequently called routines:\n")
        called_counts = defaultdict(int)
        for file_data in parser._parsed_files.values():
            for routine in file_data.routines:
                if routine.calls:
                    for called in routine.calls:
                        called_counts[called] += 1
        
        most_called = sorted(called_counts.items(), key=lambda x: x[1], reverse=True)
        for name, count in most_called[:10]:
            f.write(f"  {name}: called {count} times\n")
    
    print(f"Created summary report: {output_file}")


def main():
    # Parse BLAS and LAPACK source files
    parser = FortranParser()
    
    print("Parsing source files...")
    file_count = 0
    
    # Parse BLAS
    blas_path = Path("BLAS/SRC")
    if blas_path.exists():
        for f_file in blas_path.glob("*.f"):
            result = parser.parse_file(f_file)
            if not result.error:
                file_count += 1
    
    # Parse LAPACK
    lapack_path = Path("SRC")
    if lapack_path.exists():
        for f_file in lapack_path.glob("*.f"):
            result = parser.parse_file(f_file)
            if not result.error:
                file_count += 1
            
            # Show progress
            if file_count % 50 == 0:
                print(f"  Parsed {file_count} files...")
    
    print(f"\nTotal files parsed: {file_count}")
    
    # Export to different formats
    print("\nExporting to different formats...")
    
    # JSON export
    export_to_json(parser)
    
    # CSV export
    export_to_csv(parser)
    
    # GraphML export (for Gephi, yEd, etc.)
    export_to_graphml(parser)
    
    # DOT export (for Graphviz)
    # Note: Full graph might be too large for visualization
    # Consider filtering for specific routines
    print("\nSkipping DOT export (graph too large for visualization)")
    print("To export a subset, modify the script to filter routines")
    
    # Create summary report
    create_summary_report(parser)
    
    print("\nExport complete!")


if __name__ == "__main__":
    main()