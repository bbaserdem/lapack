#!/usr/bin/env python3
"""
Demo showing how to integrate fortran-mapper with LAPACK.
This demonstrates the migration path from lapack-util to fortran-mapper.
"""

import sys
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from fortran_mapper import FortranMapper
# Import from the separate LAPACK hooks package
# Make sure to install it first: cd hooks/lapack && uv pip install -e .
try:
    from fortran_mapper_hooks_lapack import LapackNodeEnricher, LapackNodeCreator
except ImportError:
    print("ERROR: fortran-mapper-hooks-lapack not installed!")
    print("Please install it first:")
    print("  cd hooks/lapack")
    print("  uv pip install -e .")
    sys.exit(1)


def demo_simple_usage():
    """Show basic usage that replaces lapack-util functionality."""
    print("=== Simple LAPACK Integration Demo ===")
    
    # Create mapper with LAPACK hooks (replaces LapackParser)
    mapper = FortranMapper()
    mapper.register_hook("node_enricher", LapackNodeEnricher())
    mapper.register_hook("node_creator", LapackNodeCreator())
    
    # This would replace: parser = LapackParser()
    # And: parser.parse_directory(directory)
    
    # For demonstration, we'll use the test files from the parent directory
    test_script_path = Path(__file__).parent.parent / "test_fortran_mapper.py"
    
    print("✅ Created mapper with LAPACK hooks")
    print("✅ This replaces the old LapackParser class")
    
    # Show how the export methods work
    print("\n📁 Export capabilities (replacing old export methods):")
    print("  - mapper.export_to_json(graph, 'output.json')")
    print("  - mapper.export_to_dot(graph, 'output.dot')") 
    print("  - mapper.export_to_graphml(graph, 'output.graphml')")
    print("  - mapper.export_to_neo4j(graph, driver)")
    
    print("\n🔧 Hook system allows customization:")
    print("  - NodeEnricher: Add LAPACK naming convention parsing")
    print("  - NodeCreator: Create Precision, Operation, MatrixType nodes") 
    print("  - ParseFilter: Control what gets parsed")
    print("  - ErrorHandler: Handle parsing errors gracefully")


def demo_advanced_features():
    """Show advanced features not available in lapack-util."""
    print("\n=== Advanced Features Demo ===")
    
    # Create a custom filter
    from fortran_mapper.hooks.base import ParseFilter
    
    class OnlyDoublePrecisionFilter(ParseFilter):
        def should_parse_file(self, file_path: str) -> bool:
            return True  # Parse all files
        
        def should_include_routine(self, routine_name: str, routine_type: str) -> bool:
            # Only include double precision routines
            return routine_name.startswith('D') or routine_name.startswith('Z')
    
    # Create a custom relationship creator
    from fortran_mapper.hooks.base import RelationshipCreator
    from fortran_mapper.core.relationships import Relationship, RelationType
    
    class CustomRelationshipCreator(RelationshipCreator):
        def create_relationships(self, from_node, to_node, context):
            # This is just a demo - in practice you'd add meaningful relationships
            return []
        
        def create_routine_relationships(self, routine_node, context):
            # Create custom performance relationships
            relationships = []
            # Could add relationships like "OPTIMIZED_FOR" -> "GPU", etc.
            return relationships
    
    mapper = FortranMapper()
    mapper.register_hook("node_enricher", LapackNodeEnricher())
    mapper.register_hook("node_creator", LapackNodeCreator()) 
    mapper.register_hook("parse_filter", OnlyDoublePrecisionFilter())
    mapper.register_hook("relationship_creator", CustomRelationshipCreator())
    
    print("✅ Created mapper with custom hooks")
    print("  - OnlyDoublePrecisionFilter: Only includes D/Z precision routines")
    print("  - CustomRelationshipCreator: Adds performance relationships")
    
    print("\n🚀 New capabilities:")
    print("  - Multiple hooks of the same type")
    print("  - Dynamic filtering during parsing")
    print("  - Custom node types with domain logic")
    print("  - Extensible relationship creation")
    print("  - Error handling with recovery")


def demo_migration_path():
    """Show the migration path from lapack-util."""
    print("\n=== Migration Path ===")
    
    print("📋 Steps to migrate from lapack-util:")
    print("  1. Replace 'from lapack_analyzer.parser import LapackParser'")
    print("     with 'from fortran_mapper import FortranMapper'")
    print("     and 'from fortran_mapper.adapters.lapack import LapackNodeEnricher, LapackNodeCreator'")
    
    print("\n  2. Replace parser initialization:")
    print("     OLD: parser = LapackParser()")
    print("     NEW: mapper = FortranMapper()")
    print("          mapper.register_hook('node_enricher', LapackNodeEnricher())")
    print("          mapper.register_hook('node_creator', LapackNodeCreator())")
    
    print("\n  3. Update parsing calls:")
    print("     OLD: parser.parse_directory(directory)")
    print("     NEW: graph = mapper.parse_directory(directory)")
    
    print("\n  4. Update export calls:")
    print("     OLD: parser.export_to_neo4j(driver)")
    print("     NEW: mapper.export_to_neo4j(graph, driver)")
    
    print("\n  5. Access parsed data through graph:")
    print("     NEW: routines = graph.get_nodes_by_type(NodeType.ROUTINE)")
    print("          files = graph.get_nodes_by_type(NodeType.FILE)")
    print("          custom_nodes = graph.get_nodes_by_type(NodeType.CUSTOM)")
    
    print("\n✅ Benefits of migration:")
    print("  - Generic Fortran parsing (not just LAPACK)")
    print("  - Extensible hook system")
    print("  - Better error handling")
    print("  - Multiple export formats")
    print("  - Cleaner architecture")
    print("  - Type safety and documentation")


def demo_comparison():
    """Compare old vs new API."""
    print("\n=== API Comparison ===")
    
    print("📊 lapack-util (old):")
    print("""
    from lapack_analyzer.parser import LapackParser
    
    parser = LapackParser()
    parser.parse_directory("path/to/lapack")
    parser.export_to_json("output.json")
    parser.export_to_neo4j(driver)
    
    # Limited customization
    # LAPACK-specific only
    # Fixed node types
    """)
    
    print("🚀 fortran-mapper (new):")
    print("""
    from fortran_mapper import FortranMapper
    from fortran_mapper.adapters.lapack import LapackNodeEnricher, LapackNodeCreator
    
    mapper = FortranMapper()
    mapper.register_hook("node_enricher", LapackNodeEnricher())
    mapper.register_hook("node_creator", LapackNodeCreator())
    
    graph = mapper.parse_directory("path/to/fortran")
    mapper.export_to_json(graph, "output.json")
    mapper.export_to_neo4j(graph, driver)
    
    # Fully extensible
    # Generic Fortran support
    # Custom hooks and nodes
    # Better error handling
    # Multiple export formats
    """)


def main():
    """Run the demo."""
    print("🔧 Fortran Mapper - LAPACK Integration Demo")
    print("=" * 50)
    
    demo_simple_usage()
    demo_advanced_features()
    demo_migration_path()
    demo_comparison()
    
    print("\n" + "=" * 50)
    print("🎉 Demo completed! The fortran-mapper provides a clean migration")
    print("   path from lapack-util while adding powerful extensibility.")
    print("\n📚 Next steps:")
    print("   - Review the migration guide in the documentation")
    print("   - Start with simple replacement of LapackParser")
    print("   - Gradually add custom hooks for domain-specific needs")
    print("   - Leverage the generic Fortran parsing for other projects")


if __name__ == "__main__":
    main()