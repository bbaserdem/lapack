"""Base hook interfaces for extensibility."""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional, Tuple
from ..core.nodes import BaseNode
from ..core.relationships import Relationship


class NodeEnricher(ABC):
    """Hook for enriching node properties."""
    
    @abstractmethod
    def enrich_routine(self, name: str, file_path: str, 
                      properties: Dict[str, Any]) -> Dict[str, Any]:
        """
        Enrich routine node with additional properties.
        
        Args:
            name: Routine name
            file_path: Path to the file containing the routine
            properties: Existing properties dictionary
            
        Returns:
            Updated properties dictionary
        """
        pass
    
    @abstractmethod
    def extract_categories(self, name: str) -> List[str]:
        """
        Extract categories from routine name.
        
        Args:
            name: Routine name
            
        Returns:
            List of category names
        """
        pass
    
    def enrich_file(self, file_path: str, properties: Dict[str, Any]) -> Dict[str, Any]:
        """
        Enrich file node with additional properties.
        Default implementation returns properties unchanged.
        """
        return properties
    
    def enrich_module(self, name: str, properties: Dict[str, Any]) -> Dict[str, Any]:
        """
        Enrich module node with additional properties.
        Default implementation returns properties unchanged.
        """
        return properties


class NodeCreator(ABC):
    """Hook for creating additional nodes."""
    
    @abstractmethod
    def create_additional_nodes(self, routine_name: str, 
                               routine_properties: Dict[str, Any]) -> List[BaseNode]:
        """
        Create additional nodes based on routine information.
        
        Args:
            routine_name: Name of the routine
            routine_properties: Properties of the routine
            
        Returns:
            List of additional nodes to create
        """
        pass
    
    def create_file_nodes(self, file_path: str, 
                         file_properties: Dict[str, Any]) -> List[BaseNode]:
        """
        Create additional nodes based on file information.
        Default implementation returns empty list.
        """
        return []


class RelationshipCreator(ABC):
    """Hook for creating custom relationships."""
    
    @abstractmethod
    def create_relationships(self, from_node: BaseNode, to_node: BaseNode,
                           context: Dict[str, Any]) -> List[Relationship]:
        """
        Create custom relationships between nodes.
        
        Args:
            from_node: Source node
            to_node: Target node
            context: Additional context (e.g., parser state)
            
        Returns:
            List of relationships to create
        """
        pass
    
    def create_routine_relationships(self, routine_node: BaseNode,
                                   context: Dict[str, Any]) -> List[Relationship]:
        """
        Create relationships for a routine node.
        Default implementation returns empty list.
        """
        return []


class ParseFilter(ABC):
    """Hook for filtering what gets parsed."""
    
    @abstractmethod
    def should_parse_file(self, file_path: str) -> bool:
        """
        Determine if a file should be parsed.
        
        Args:
            file_path: Path to the file
            
        Returns:
            True if file should be parsed, False otherwise
        """
        pass
    
    @abstractmethod
    def should_include_routine(self, routine_name: str, 
                             routine_type: str) -> bool:
        """
        Determine if a routine should be included in the graph.
        
        Args:
            routine_name: Name of the routine
            routine_type: Type of routine (subroutine/function)
            
        Returns:
            True if routine should be included, False otherwise
        """
        pass
    
    def should_include_call(self, caller: str, callee: str) -> bool:
        """
        Determine if a call relationship should be included.
        Default implementation returns True.
        """
        return True


class ErrorHandler(ABC):
    """Hook for handling parsing errors."""
    
    @abstractmethod
    def handle_parse_error(self, file_path: str, error: Exception) -> Optional[Dict[str, Any]]:
        """
        Handle a parsing error.
        
        Args:
            file_path: Path to the file that caused the error
            error: The exception that occurred
            
        Returns:
            Optional error information to store, or None to skip
        """
        pass
    
    def handle_routine_error(self, routine_name: str, error: Exception) -> Optional[Dict[str, Any]]:
        """
        Handle an error while parsing a specific routine.
        Default implementation returns None.
        """
        return None