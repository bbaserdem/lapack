#!/usr/bin/env python3
"""
Example: Analyze dependencies and find critical routines in LAPACK/BLAS
"""

from pathlib import Path
import sys
from collections import defaultdict, Counter

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.lapack_util.fortran_parser import FortranParser


def analyze_call_graph(parser):
    """Analyze the call graph to find critical routines and dependencies."""
    
    # Build forward and reverse call graphs
    calls_to = defaultdict(set)  # routine -> set of routines it calls
    called_by = defaultdict(set)  # routine -> set of routines that call it
    routine_info = {}  # routine -> info dict
    
    for file_path, file_data in parser._parsed_files.items():
        for routine in file_data.routines:
            routine_info[routine.name] = {
                'file': file_path,
                'precision': routine.precision,
                'operation': routine.operation,
                'type': routine.routine_type
            }
            
            if routine.calls:
                calls_to[routine.name] = routine.calls
                for called in routine.calls:
                    called_by[called].add(routine.name)
    
    return calls_to, called_by, routine_info


def find_critical_routines(called_by, top_n=20):
    """Find the most frequently called routines."""
    call_counts = [(name, len(callers)) for name, callers in called_by.items()]
    return sorted(call_counts, key=lambda x: x[1], reverse=True)[:top_n]


def find_leaf_routines(calls_to, called_by):
    """Find routines that don't call any other routines (leaf nodes)."""
    all_routines = set(calls_to.keys()) | set(called_by.keys())
    return [r for r in all_routines if r not in calls_to or not calls_to[r]]


def find_entry_points(calls_to, called_by):
    """Find routines that are not called by any other routine."""
    all_routines = set(calls_to.keys()) | set(called_by.keys())
    return [r for r in all_routines if r not in called_by or not called_by[r]]


def analyze_call_chains(routine_name, calls_to, max_depth=5):
    """Find all call chains starting from a routine."""
    chains = []
    
    def traverse(current, path, depth):
        if depth >= max_depth:
            return
        
        if current in calls_to:
            for called in calls_to[current]:
                if called not in path:  # Avoid cycles
                    new_path = path + [called]
                    chains.append(new_path)
                    traverse(called, new_path, depth + 1)
    
    traverse(routine_name, [routine_name], 0)
    return chains


def find_precision_variants(routine_info):
    """Group routines by their base operation across precisions."""
    operation_variants = defaultdict(lambda: {'s': [], 'd': [], 'c': [], 'z': []})
    
    for name, info in routine_info.items():
        if info['operation'] and info['precision']:
            operation_variants[info['operation']][info['precision']].append(name)
    
    return operation_variants


def main():
    # Parse all BLAS and LAPACK source files
    parser = FortranParser()
    
    print("Parsing BLAS source files...")
    blas_count = 0
    for f_file in Path("BLAS/SRC").glob("*.f"):
        result = parser.parse_file(f_file)
        if not result.error:
            blas_count += len(result.routines)
    
    print("Parsing LAPACK source files...")
    lapack_count = 0
    src_path = Path("SRC")
    if src_path.exists():
        for f_file in src_path.glob("*.f"):
            result = parser.parse_file(f_file)
            if not result.error:
                lapack_count += len(result.routines)
    
    print(f"\nTotal routines parsed: BLAS={blas_count}, LAPACK={lapack_count}")
    
    # Analyze the call graph
    calls_to, called_by, routine_info = analyze_call_graph(parser)
    
    # Find critical routines
    print("\n=== Most Called Routines ===")
    critical = find_critical_routines(called_by, 20)
    for name, count in critical:
        info = routine_info.get(name, {})
        print(f"{name:15} - called by {count:4} routines ({info.get('type', 'unknown')})")
    
    # Find leaf routines
    leaves = find_leaf_routines(calls_to, called_by)
    print(f"\n=== Leaf Routines (no outgoing calls) ===")
    print(f"Total: {len(leaves)}")
    print("Examples:", ", ".join(leaves[:10]))
    
    # Find entry points
    entries = find_entry_points(calls_to, called_by)
    print(f"\n=== Entry Points (not called by others) ===")
    print(f"Total: {len(entries)}")
    
    # Analyze specific important routines
    print("\n=== Call Chain Analysis ===")
    for routine in ['DGEMM', 'DGETRF', 'DGESVD']:
        if routine in calls_to:
            chains = analyze_call_chains(routine, calls_to, max_depth=3)
            print(f"\n{routine} call chains (max depth 3):")
            for chain in chains[:5]:  # Show first 5 chains
                print(f"  {' -> '.join(chain)}")
    
    # Find precision variants
    print("\n=== Precision Variants ===")
    variants = find_precision_variants(routine_info)
    
    # Show operations with all 4 precision variants
    complete_ops = []
    for op, precisions in variants.items():
        if all(precisions[p] for p in ['s', 'd', 'c', 'z']):
            complete_ops.append(op)
    
    print(f"Operations with all 4 precisions: {len(complete_ops)}")
    for op in sorted(complete_ops)[:10]:
        print(f"  {op}: ", end="")
        for p in ['s', 'd', 'c', 'z']:
            if variants[op][p]:
                print(f"{p}={variants[op][p][0]}", end=" ")
        print()
    
    # Analyze precision distribution
    print("\n=== Precision Distribution ===")
    precision_counts = Counter()
    for name, info in routine_info.items():
        if info['precision']:
            precision_counts[info['precision']] += 1
    
    total = sum(precision_counts.values())
    for prec in ['s', 'd', 'c', 'z']:
        count = precision_counts[prec]
        print(f"  {prec}: {count:4} ({count/total*100:5.1f}%)")


if __name__ == "__main__":
    main()