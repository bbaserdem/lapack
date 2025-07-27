"""
Fortran Mapper - A flexible, extensible Fortran code mapper.
"""

from .core.parser import FortranMapper
from .core.nodes import NodeType, RoutineNode, FileNode
from .core.relationships import RelationType, Relationship
from .core.graph import Graph
from .hooks.base import NodeEnricher, NodeCreator, RelationshipCreator
from .hooks.registry import HookRegistry

__version__ = "0.1.0"
__all__ = [
    "FortranMapper",
    "NodeType",
    "RoutineNode",
    "FileNode",
    "RelationType",
    "Relationship",
    "Graph",
    "NodeEnricher",
    "NodeCreator",
    "RelationshipCreator",
    "HookRegistry",
]