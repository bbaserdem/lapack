"""lapack_util - Python utilities for LAPACK code analysis and graph generation"""

from .fortran_parser import FortranParser, Routine, ParseResult
from .graph_schema import (
    GraphSchema, GraphNode, GraphRelationship,
    NodeType, RelationType, create_schema_from_routines
)
from .lapack_to_neo4j import LAPACKGraphBuilder

__all__ = [
    'FortranParser', 'Routine', 'ParseResult',
    'GraphSchema', 'GraphNode', 'GraphRelationship',
    'NodeType', 'RelationType', 'create_schema_from_routines',
    'LAPACKGraphBuilder'
]

def main() -> None:
    """Entry point for the lapack-util command"""
    from .lapack_to_neo4j import main as lapack_main
    lapack_main()
