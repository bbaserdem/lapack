"""
Connectome - LAPACK computational connectome analysis workspace.

Built on top of fortran-mapper for analyzing and visualizing 
computational relationships in LAPACK.
"""

__version__ = "0.1.0"
__author__ = "LAPACK Analysis Team"

# Re-export core fortran-mapper functionality
from fortran_mapper import FortranMapper, NodeType, RoutineNode, FileNode
from fortran_mapper import RelationType, Relationship, Graph
from fortran_mapper.hooks.base import NodeEnricher, NodeCreator, RelationshipCreator

# Connectome-specific modules (to be implemented)
# from .analysis import ConnectomeAnalyzer
# from .visualization import ConnectomeVisualizer

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
]
