"""Hook registry for managing extensibility."""

from typing import Dict, List, Any, Optional, Type
from .base import NodeEnricher, NodeCreator, RelationshipCreator, ParseFilter, ErrorHandler


class HookRegistry:
    """Registry for managing hooks."""
    
    def __init__(self):
        self._hooks: Dict[str, List[Any]] = {
            "node_enricher": [],
            "node_creator": [],
            "relationship_creator": [],
            "parse_filter": [],
            "error_handler": []
        }
        
        self._hook_types = {
            "node_enricher": NodeEnricher,
            "node_creator": NodeCreator,
            "relationship_creator": RelationshipCreator,
            "parse_filter": ParseFilter,
            "error_handler": ErrorHandler
        }
    
    def register(self, hook_type: str, hook: Any) -> None:
        """Register a hook."""
        if hook_type not in self._hooks:
            raise ValueError(f"Unknown hook type: {hook_type}")
        
        expected_type = self._hook_types[hook_type]
        if not isinstance(hook, expected_type):
            raise TypeError(f"Hook must be instance of {expected_type.__name__}")
        
        self._hooks[hook_type].append(hook)
    
    def unregister(self, hook_type: str, hook: Any) -> None:
        """Unregister a hook."""
        if hook_type in self._hooks and hook in self._hooks[hook_type]:
            self._hooks[hook_type].remove(hook)
    
    def get_hooks(self, hook_type: str) -> List[Any]:
        """Get all hooks of a specific type."""
        return self._hooks.get(hook_type, [])
    
    def clear_hooks(self, hook_type: Optional[str] = None) -> None:
        """Clear hooks of a specific type or all hooks."""
        if hook_type:
            if hook_type in self._hooks:
                self._hooks[hook_type].clear()
        else:
            for hooks in self._hooks.values():
                hooks.clear()
    
    def has_hooks(self, hook_type: str) -> bool:
        """Check if any hooks of a specific type are registered."""
        return bool(self._hooks.get(hook_type))
    
    # Convenience methods for specific hook types
    
    def get_node_enrichers(self) -> List[NodeEnricher]:
        """Get all node enricher hooks."""
        return self.get_hooks("node_enricher")
    
    def get_node_creators(self) -> List[NodeCreator]:
        """Get all node creator hooks."""
        return self.get_hooks("node_creator")
    
    def get_relationship_creators(self) -> List[RelationshipCreator]:
        """Get all relationship creator hooks."""
        return self.get_hooks("relationship_creator")
    
    def get_parse_filters(self) -> List[ParseFilter]:
        """Get all parse filter hooks."""
        return self.get_hooks("parse_filter")
    
    def get_error_handlers(self) -> List[ErrorHandler]:
        """Get all error handler hooks."""
        return self.get_hooks("error_handler")