"""Base relationship types for Fortran code graph."""

from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, Any


class RelationType(Enum):
    """Types of relationships in the graph."""
    CALLS = "CALLS"
    DEFINED_IN = "DEFINED_IN"
    CONTAINS = "CONTAINS"
    USES_MODULE = "USES_MODULE"
    DEPENDS_ON = "DEPENDS_ON"
    CUSTOM = "CUSTOM"  # For hook-created relationships


@dataclass
class Relationship:
    """Represents a relationship between two nodes."""
    
    rel_type: RelationType
    from_node_id: str
    to_node_id: str
    properties: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "type": self.rel_type.value,
            "from": self.from_node_id,
            "to": self.to_node_id,
            "properties": self.properties
        }
    
    def __hash__(self) -> int:
        """Make relationship hashable for deduplication."""
        return hash((self.rel_type, self.from_node_id, self.to_node_id))
    
    def __eq__(self, other) -> bool:
        """Check equality for deduplication."""
        if not isinstance(other, Relationship):
            return False
        return (self.rel_type == other.rel_type and
                self.from_node_id == other.from_node_id and
                self.to_node_id == other.to_node_id)