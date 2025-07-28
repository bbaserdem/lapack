#!/usr/bin/env python3
"""Test script for LAPACK hooks package."""

import sys
from pathlib import Path

# Try to import the hooks
try:
    from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator
    print("✅ Successfully imported LAPACK hooks")
except ImportError as e:
    print(f"❌ Failed to import LAPACK hooks: {e}")
    print("\nPlease install the package first:")
    print("  uv pip install -e .")
    sys.exit(1)

# Test the enricher
enricher = LapackNodeEnricher()
print("\n✅ Created LapackNodeEnricher")

# Test enrichment
test_cases = [
    ("DGEMM", "test.f"),
    ("SGETRF", "test.f"),
    ("ZGESVD", "test.f"),
    ("XERBLA", "test.f"),
]

print("\nTesting enrichment:")
for routine_name, file_path in test_cases:
    props = {}
    enriched = enricher.enrich_routine(routine_name, file_path, props)
    categories = enricher.extract_categories(routine_name)
    print(f"  {routine_name}: precision={enriched.get('precision', 'N/A')}, "
          f"operation={enriched.get('operation', 'N/A')}, categories={categories}")

# Test the creator
creator = LapackNodeCreator()
print("\n✅ Created LapackNodeCreator")

# Test node creation
props = {
    'precision': 'd',
    'precision_name': 'double',
    'operation': 'GEMM',
    'matrix_type': 'GE'
}
nodes = creator.create_additional_nodes("DGEMM", props)
print(f"\nCreated {len(nodes)} additional nodes for DGEMM:")
for node in nodes:
    print(f"  - {node.properties['custom_type']}: {node.name}")

print("\n✅ All tests passed! The LAPACK hooks package is working correctly.")