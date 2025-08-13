"""Generic Fortran parser without domain-specific logic."""

import subprocess
import re
import logging
from pathlib import Path
from typing import List, Dict, Optional, Set, Any
from dataclasses import dataclass, field

from .nodes import RoutineNode, FileNode, ModuleNode, BaseNode
from .relationships import Relationship, RelationType
from .graph import Graph
from ..hooks.registry import HookRegistry
from ..exporters.json import JSONExporter
from ..exporters.dot import DOTExporter
from ..exporters.graphml import GraphMLExporter
from ..exporters.neo4j import Neo4jExporter

logger = logging.getLogger(__name__)


@dataclass
class ParsedRoutine:
    """Raw parsed routine data before node creation."""
    name: str
    routine_type: str
    file_path: str
    calls: Set[str] = field(default_factory=set)
    line_start: Optional[int] = None
    line_end: Optional[int] = None
    arguments: List[str] = field(default_factory=list)


@dataclass
class ParseResult:
    """Result of parsing a file."""
    file_path: str
    routines: List[ParsedRoutine]
    modules: List[str] = field(default_factory=list)
    uses: List[str] = field(default_factory=list)
    error: Optional[str] = None


class FortranParser:
    """Generic Fortran parser using fortran-src."""
    
    def __init__(self, fortran_src_path: str = "fortran-src"):
        self.fortran_src = fortran_src_path
        self._check_fortran_src()
    
    def _check_fortran_src(self):
        """Check if fortran-src is available."""
        try:
            subprocess.run([self.fortran_src, "--version"], 
                         capture_output=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError):
            logger.warning(f"fortran-src not found at '{self.fortran_src}', "
                          "falling back to regex parsing")
            self.fortran_src = None
    
    def parse_file(self, file_path: Path) -> ParseResult:
        """Parse a single Fortran file."""
        logger.info(f"Parsing {file_path}")
        
        if self.fortran_src:
            return self._parse_with_fortran_src(file_path)
        else:
            return self._parse_with_regex(file_path)
    
    def _parse_with_fortran_src(self, file_path: Path) -> ParseResult:
        """Parse using fortran-src."""
        try:
            # Get reprinted version for easier parsing
            result = subprocess.run(
                [self.fortran_src, "-r", str(file_path)],
                capture_output=True,
                text=True,
                check=True
            )
            
            routines = self._extract_routines_from_reprinted(result.stdout, file_path)
            modules, uses = self._extract_modules_from_reprinted(result.stdout)
            
            return ParseResult(
                file_path=str(file_path),
                routines=routines,
                modules=modules,
                uses=uses
            )
            
        except subprocess.CalledProcessError as e:
            logger.warning(f"fortran-src failed to parse {file_path}: {e.stderr}")
            logger.info(f"Falling back to regex parser for {file_path}")
            # Fall back to regex parsing
            result = self._parse_with_regex(file_path)
            if result.routines:
                logger.info(f"Regex parser successfully found {len(result.routines)} routines in {file_path}")
            return result
    
    def _parse_with_regex(self, file_path: Path) -> ParseResult:
        """Fallback regex-based parsing."""
        try:
            with open(file_path, 'r', encoding='latin-1') as f:
                content = f.read()
            
            routines = self._extract_routines_from_content(content, file_path)
            modules, uses = self._extract_modules_from_content(content)
            
            return ParseResult(
                file_path=str(file_path),
                routines=routines,
                modules=modules,
                uses=uses
            )
            
        except Exception as e:
            logger.error(f"Regex parser failed for {file_path}: {e}")
            return ParseResult(
                file_path=str(file_path),
                routines=[],
                error=str(e)
            )
    
    def _extract_routines_from_reprinted(self, content: str, 
                                       file_path: Path) -> List[ParsedRoutine]:
        """Extract routines from fortran-src reprinted output."""
        return self._extract_routines_from_content(content, file_path)
    
    def _extract_routines_from_content(self, content: str, 
                                     file_path: Path) -> List[ParsedRoutine]:
        """Extract routines from Fortran source content."""
        routines = []
        lines = content.split('\n')
        current_routine = None
        
        for i, line in enumerate(lines):
            line_upper = line.upper().strip()
            
            # Check for subroutine definition
            if line_upper.startswith('SUBROUTINE '):
                match = re.match(r'SUBROUTINE\s+(\w+)\s*\((.*?)\)', line_upper)
                if match:
                    if current_routine:
                        routines.append(current_routine)
                    
                    name = match.group(1)
                    args = match.group(2) if match.group(2) else ""
                    
                    current_routine = ParsedRoutine(
                        name=name,
                        routine_type='subroutine',
                        file_path=str(file_path),
                        line_start=i + 1,
                        arguments=self._parse_arguments(args)
                    )
            
            # Check for function definition
            elif 'FUNCTION' in line_upper:
                # Match various function declaration patterns
                patterns = [
                    r'^\s*(\w+\s+)*FUNCTION\s+(\w+)\s*\((.*?)\)',  # TYPE FUNCTION NAME(ARGS)
                    r'^\s*FUNCTION\s+(\w+)\s*\((.*?)\)',           # FUNCTION NAME(ARGS)
                ]
                
                for pattern in patterns:
                    match = re.search(pattern, line_upper)
                    if match:
                        if current_routine:
                            routines.append(current_routine)
                        
                        # Extract function name (last group for first pattern, first for second)
                        if len(match.groups()) == 3:
                            name = match.group(2)
                            args = match.group(3)
                        else:
                            name = match.group(1)
                            args = match.group(2)
                        
                        current_routine = ParsedRoutine(
                            name=name,
                            routine_type='function',
                            file_path=str(file_path),
                            line_start=i + 1,
                            arguments=self._parse_arguments(args) if args else []
                        )
                        break
            
            # Check for CALL statements
            elif current_routine and line_upper.strip().startswith('CALL '):
                match = re.match(r'CALL\s+(\w+)', line_upper.strip())
                if match:
                    called_routine = match.group(1)
                    current_routine.calls.add(called_routine)
            
            # Check for function calls in expressions
            elif current_routine:
                # Look for function calls (word followed by parentheses)
                func_calls = re.findall(r'\b([A-Z][A-Z0-9_]*)\s*\(', line_upper)
                for func_name in func_calls:
                    # Skip common Fortran keywords and intrinsics
                    if func_name not in ['IF', 'DO', 'WRITE', 'READ', 'OPEN', 'CLOSE']:
                        current_routine.calls.add(func_name)
            
            # Check for END statements
            if current_routine and (
                line_upper.startswith('END SUBROUTINE') or 
                line_upper.startswith('END FUNCTION') or
                line_upper == 'END'
            ):
                current_routine.line_end = i + 1
                routines.append(current_routine)
                current_routine = None
        
        # Don't forget the last routine if file doesn't end with END
        if current_routine:
            routines.append(current_routine)
        
        return routines
    
    def _extract_modules_from_reprinted(self, content: str) -> tuple[List[str], List[str]]:
        """Extract modules from fortran-src reprinted output."""
        return self._extract_modules_from_content(content)
    
    def _extract_modules_from_content(self, content: str) -> tuple[List[str], List[str]]:
        """Extract module definitions and use statements."""
        modules = []
        uses = []
        
        lines = content.split('\n')
        for line in lines:
            line_upper = line.upper().strip()
            
            # Module definition
            if line_upper.startswith('MODULE ') and not line_upper.startswith('MODULE PROCEDURE'):
                match = re.match(r'MODULE\s+(\w+)', line_upper)
                if match:
                    modules.append(match.group(1))
            
            # Use statement
            elif line_upper.startswith('USE '):
                match = re.match(r'USE\s+(\w+)', line_upper)
                if match:
                    uses.append(match.group(1))
        
        return modules, uses
    
    def _parse_arguments(self, args_str: str) -> List[str]:
        """Parse argument list string."""
        if not args_str.strip():
            return []
        
        # Remove comments and clean up
        args_clean = re.sub(r'!.*$', '', args_str, flags=re.MULTILINE)
        args = [arg.strip() for arg in args_clean.split(',') if arg.strip()]
        return args


class FortranMapper:
    """Main mapper class with hook support."""
    
    def __init__(self, fortran_src_path: str = "fortran-src"):
        self.parser = FortranParser(fortran_src_path)
        self.hooks = HookRegistry()
    
    def register_hook(self, hook_type: str, hook: Any) -> None:
        """Register a hook for extensibility."""
        self.hooks.register(hook_type, hook)
    
    def parse_file(self, file_path: Path) -> Graph:
        """Parse a single file and return a graph."""
        # Check parse filters
        for filter_hook in self.hooks.get_parse_filters():
            if not filter_hook.should_parse_file(str(file_path)):
                logger.info(f"Skipping {file_path} due to filter")
                return Graph()
        
        try:
            result = self.parser.parse_file(file_path)
            return self._build_graph_from_result(result)
        except Exception as e:
            # Handle error through hooks
            for error_handler in self.hooks.get_error_handlers():
                error_info = error_handler.handle_parse_error(str(file_path), e)
                if error_info:
                    logger.error(f"Parse error in {file_path}: {error_info}")
            raise
    
    def parse_directory(self, directory: Path, 
                       extensions: List[str] = None) -> Graph:
        """Parse all Fortran files in a directory."""
        if extensions is None:
            extensions = ['.f', '.f90', '.f77', '.F', '.F90']
        
        main_graph = Graph()
        
        for ext in extensions:
            for file_path in directory.rglob(f'*{ext}'):
                try:
                    file_graph = self.parse_file(file_path)
                    main_graph.merge(file_graph)
                except Exception as e:
                    logger.error(f"Failed to parse {file_path}: {e}")
                    continue
        
        return main_graph
    
    # Convenience export methods
    def export_to_json(self, graph: Graph, output_path: Path) -> None:
        """Export graph to JSON format."""
        exporter = JSONExporter()
        exporter.export(graph, output_path)
    
    def export_to_dot(self, graph: Graph, output_path: Path) -> None:
        """Export graph to DOT format."""
        exporter = DOTExporter()
        exporter.export(graph, output_path)
    
    def export_to_graphml(self, graph: Graph, output_path: Path) -> None:
        """Export graph to GraphML format."""
        exporter = GraphMLExporter()
        exporter.export(graph, output_path)
    
    def export_to_neo4j(self, graph: Graph, driver: Any, clear_existing: bool = True) -> None:
        """Export graph to Neo4j database."""
        exporter = Neo4jExporter()
        exporter.export(graph, driver, clear_existing)
    
    def _build_graph_from_result(self, result: ParseResult) -> Graph:
        """Build graph from parse result."""
        graph = Graph()
        
        # Create file node
        file_props = {"path": result.file_path}
        
        # Enrich file properties through hooks
        for enricher in self.hooks.get_node_enrichers():
            file_props = enricher.enrich_file(result.file_path, file_props)
        
        file_node = FileNode(result.file_path, **file_props)
        graph.add_node(file_node)
        
        # Create additional file-related nodes
        for creator in self.hooks.get_node_creators():
            additional_nodes = creator.create_file_nodes(result.file_path, file_props)
            for node in additional_nodes:
                graph.add_node(node)
        
        # Create routine nodes
        for routine_data in result.routines:
            # Check filters
            include_routine = True
            for filter_hook in self.hooks.get_parse_filters():
                if not filter_hook.should_include_routine(routine_data.name, 
                                                         routine_data.routine_type):
                    include_routine = False
                    break
            
            if not include_routine:
                continue
            
            # Build routine properties
            routine_props = {
                "routine_type": routine_data.routine_type,
                "line_start": routine_data.line_start,
                "line_end": routine_data.line_end,
                "arguments": routine_data.arguments
            }
            
            # Enrich through hooks
            for enricher in self.hooks.get_node_enrichers():
                routine_props = enricher.enrich_routine(
                    routine_data.name, routine_data.file_path, routine_props
                )
                
                # Add categories
                categories = enricher.extract_categories(routine_data.name)
                if categories:
                    routine_props["categories"] = categories
            
            # Create routine node
            routine_node = RoutineNode(routine_data.name, **routine_props)
            graph.add_node(routine_node)
            
            # Create additional nodes for this routine
            for creator in self.hooks.get_node_creators():
                additional_nodes = creator.create_additional_nodes(
                    routine_data.name, routine_props
                )
                for node in additional_nodes:
                    graph.add_node(node)
            
            # Create DEFINED_IN relationship
            graph.add_relationship(Relationship(
                rel_type=RelationType.DEFINED_IN,
                from_node_id=routine_node.node_id,
                to_node_id=file_node.node_id
            ))
            
            # Create CALLS relationships
            for called_name in routine_data.calls:
                # Check call filters
                include_call = True
                for filter_hook in self.hooks.get_parse_filters():
                    if not filter_hook.should_include_call(routine_data.name, called_name):
                        include_call = False
                        break
                
                if include_call:
                    called_node_id = f"routine:{called_name}"
                    graph.add_relationship(Relationship(
                        rel_type=RelationType.CALLS,
                        from_node_id=routine_node.node_id,
                        to_node_id=called_node_id,
                        properties={"direct": True}
                    ))
            
            # Create custom relationships through hooks
            for rel_creator in self.hooks.get_relationship_creators():
                custom_rels = rel_creator.create_routine_relationships(
                    routine_node, {"parse_result": result}
                )
                for rel in custom_rels:
                    graph.add_relationship(rel)
        
        # Create module nodes
        for module_name in result.modules:
            module_node = ModuleNode(module_name)
            graph.add_node(module_node)
            
            # Module defined in file
            graph.add_relationship(Relationship(
                rel_type=RelationType.DEFINED_IN,
                from_node_id=module_node.node_id,
                to_node_id=file_node.node_id
            ))
        
        return graph