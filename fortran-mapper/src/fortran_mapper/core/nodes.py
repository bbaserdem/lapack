"""Base node types for Fortran code graph."""

from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, Any, Optional, Set


class NodeType(Enum):
    """Types of nodes in the graph."""
    ROUTINE = "Routine"
    FILE = "File"
    MODULE = "Module"
    PROGRAM = "Program"
    CUSTOM = "Custom"  # For hook-created nodes


@dataclass
class BaseNode:
    """Base class for all graph nodes."""
    node_type: NodeType
    name: str
    properties: Dict[str, Any] = field(default_factory=dict)
    
    @property
    def node_id(self) -> str:
        """Generate unique node ID."""
        return f"{self.node_type.value.lower()}:{self.name}"
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "id": self.node_id,
            "type": self.node_type.value,
            "name": self.name,
            "properties": self.properties
        }


@dataclass
class RoutineNode(BaseNode):
    """Represents a Fortran routine (subroutine or function)."""
    
    def __init__(self, name: str, routine_type: str = "subroutine", **properties):
        super().__init__(
            node_type=NodeType.ROUTINE,
            name=name,
            properties={
                "routine_type": routine_type,
                **properties
            }
        )
        self.calls: Set[str] = set()
        self.called_by: Set[str] = set()
    
    def add_call(self, routine_name: str) -> None:
        """Add a routine that this routine calls."""
        self.calls.add(routine_name)
    
    def add_caller(self, routine_name: str) -> None:
        """Add a routine that calls this routine."""
        self.called_by.add(routine_name)


@dataclass
class FileNode(BaseNode):
    """Represents a Fortran source file."""
    
    def __init__(self, file_path: str, **properties):
        from pathlib import Path
        p = Path(file_path)
        super().__init__(
            node_type=NodeType.FILE,
            name=str(file_path),
            properties={
                "path": str(file_path),
                "filename": p.name,
                "directory": str(p.parent),
                **properties
            }
        )
        self.routines: Set[str] = set()
    
    def add_routine(self, routine_name: str) -> None:
        """Add a routine defined in this file."""
        self.routines.add(routine_name)


@dataclass
class ModuleNode(BaseNode):
    """Represents a Fortran module."""
    
    def __init__(self, name: str, **properties):
        super().__init__(
            node_type=NodeType.MODULE,
            name=name,
            properties=properties
        )
        self.contains: Set[str] = set()
        self.uses: Set[str] = set()
    
    def add_contained(self, item_name: str) -> None:
        """Add an item contained in this module."""
        self.contains.add(item_name)
    
    def add_use(self, module_name: str) -> None:
        """Add a module that this module uses."""
        self.uses.add(module_name)


@dataclass
class CustomNode(BaseNode):
    """Custom node type for hook-created nodes."""
    
    def __init__(self, custom_type: str, name: str, **properties):
        super().__init__(
            node_type=NodeType.CUSTOM,
            name=name,
            properties={
                "custom_type": custom_type,
                **properties
            }
        )
    
    @property
    def node_id(self) -> str:
        """Generate unique node ID including custom type."""
        custom_type = self.properties.get("custom_type", "custom")
        return f"{custom_type.lower()}:{self.name}"