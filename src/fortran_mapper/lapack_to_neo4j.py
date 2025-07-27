#!/usr/bin/env python3
"""
Main module for parsing LAPACK/BLAS source code and loading into Neo4j.

This module orchestrates the entire process:
1. Parse Fortran files using fortran-src
2. Extract routine information and relationships
3. Create graph schema
4. Load into Neo4j database
"""

import logging
import argparse
from pathlib import Path
from typing import Dict, List, Optional
import json
import sys

from .fortran_parser import FortranParser, ParseResult
from .graph_schema import (
    GraphSchema, 
    create_schema_from_routines, 
    create_schema_from_routines_with_errors,
    SCHEMA_DOCUMENTATION
)


# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class LAPACKGraphBuilder:
    """Build a graph database from LAPACK source code"""
    
    def __init__(self, lapack_root: Path, fortran_src_path: str = "fortran-src"):
        self.lapack_root = lapack_root
        self.parser = FortranParser(fortran_src_path)
        self.results: List[ParseResult] = []
        self.schema: Optional[GraphSchema] = None
    
    def parse_sources(self, directories: Optional[List[str]] = None) -> tuple[Dict[str, List], Dict[str, List]]:
        """Parse all Fortran source files in specified directories
        
        Returns:
            Tuple of (routines_by_file, errors_by_file)
        """
        if directories is None:
            directories = ["BLAS/SRC", "SRC"]
        
        routines_by_file = {}
        errors_by_file = {}
        total_files = 0
        failed_files = 0
        
        for dir_name in directories:
            dir_path = self.lapack_root / dir_name
            if not dir_path.exists():
                logger.warning(f"Directory {dir_path} does not exist, skipping")
                continue
            
            logger.info(f"Parsing files in {dir_path}")
            results = self.parser.parse_directory(dir_path)
            
            for result in results:
                total_files += 1
                if result.error:
                    failed_files += 1
                    logger.error(f"Failed to parse {result.file_path}: {result.error}")
                    
                    # Store structured errors if available
                    if result.structured_errors:
                        errors_by_file[result.file_path] = [
                            {
                                'type': err.type,
                                'severity': err.severity,
                                'message': err.message,
                                'line_number': err.line_number,
                                'column_number': err.column_number,
                                'context': err.context,
                                'timestamp': err.timestamp,
                                'parser_version': err.parser_version
                            }
                            for err in result.structured_errors
                        ]
                    else:
                        # Create a basic error entry if no structured errors
                        errors_by_file[result.file_path] = [{
                            'type': 'parse_error',
                            'severity': 'error',
                            'message': result.error,
                            'parser_version': 'fortran-src'
                        }]
                        
                    # Still include routines if any were partially parsed
                    if result.routines:
                        routines_by_file[result.file_path] = result.routines
                else:
                    if result.routines:
                        routines_by_file[result.file_path] = result.routines
                        logger.debug(f"Found {len(result.routines)} routines in {result.file_path}")
            
            self.results.extend(results)
        
        logger.info(f"Parsed {total_files} files, {failed_files} failed")
        logger.info(f"Found {sum(len(r) for r in routines_by_file.values())} total routines")
        logger.info(f"Tracked {len(errors_by_file)} files with errors")
        
        return routines_by_file, errors_by_file
    
    def build_graph_schema(self, routines_by_file: Dict[str, List], 
                          errors_by_file: Optional[Dict[str, List]] = None,
                          include_errors: bool = True) -> GraphSchema:
        """Build the graph schema from parsed routines"""
        logger.info("Building graph schema...")
        
        if include_errors and errors_by_file:
            self.schema = create_schema_from_routines_with_errors(routines_by_file, errors_by_file)
        else:
            self.schema = create_schema_from_routines(routines_by_file)
        
        logger.info(f"Created {len(self.schema.nodes)} nodes and "
                   f"{len(self.schema.relationships)} relationships")
        
        # Log statistics
        node_counts = {}
        for node in self.schema.nodes:
            node_type = node.node_type.value
            node_counts[node_type] = node_counts.get(node_type, 0) + 1
        
        for node_type, count in node_counts.items():
            logger.info(f"  {node_type}: {count}")
        
        rel_counts = {}
        for rel in self.schema.relationships:
            rel_type = rel.rel_type.value
            rel_counts[rel_type] = rel_counts.get(rel_type, 0) + 1
        
        for rel_type, count in rel_counts.items():
            logger.info(f"  {rel_type}: {count}")
        
        return self.schema
    
    def export_schema(self, output_dir: Path):
        """Export schema in various formats"""
        output_dir.mkdir(exist_ok=True)
        
        # Export as JSON
        json_path = output_dir / "lapack_graph.json"
        self.schema.to_json(json_path)
        logger.info(f"Exported graph to {json_path}")
        
        # Export as CSV for neo4j-admin import
        csv_dir = output_dir / "csv"
        self.schema.to_csv(csv_dir)
        logger.info(f"Exported CSV files to {csv_dir}")
        
        # Export Cypher statements
        cypher_path = output_dir / "lapack_graph.cypher"
        statements = self.schema.to_cypher_statements()
        with open(cypher_path, 'w') as f:
            for stmt in statements:
                if isinstance(stmt, tuple):
                    f.write(f"{stmt[0]};\n")
                else:
                    f.write(f"{stmt};\n")
        logger.info(f"Exported Cypher statements to {cypher_path}")
        
        # Export documentation
        doc_path = output_dir / "schema_documentation.md"
        with open(doc_path, 'w') as f:
            f.write(SCHEMA_DOCUMENTATION)
        logger.info(f"Exported schema documentation to {doc_path}")
    
    def load_to_neo4j(self, uri: str, username: str, password: str):
        """Load the graph schema into Neo4j"""
        try:
            from neo4j import GraphDatabase
        except ImportError:
            logger.error("neo4j package not installed. Install with: pip install neo4j")
            return
        
        logger.info(f"Connecting to Neo4j at {uri}")
        
        driver = GraphDatabase.driver(uri, auth=(username, password))
        
        try:
            with driver.session() as session:
                # Create constraints and indexes
                logger.info("Creating constraints and indexes...")
                statements = self.schema.to_cypher_statements()
                
                for stmt in statements[:7]:  # First 7 are constraints/indexes
                    session.run(stmt)
                
                # Create nodes and relationships
                logger.info("Creating nodes and relationships...")
                for stmt in statements[7:]:
                    if isinstance(stmt, tuple):
                        session.run(stmt[0], stmt[1])
                    else:
                        session.run(stmt)
                
                logger.info("Successfully loaded graph into Neo4j")
                
        finally:
            driver.close()
    
    def generate_statistics(self) -> Dict:
        """Generate parsing and graph statistics"""
        stats = {
            "total_files": len(self.results),
            "successful_files": len([r for r in self.results if not r.error]),
            "failed_files": len([r for r in self.results if r.error]),
            "total_routines": sum(len(r.routines) for r in self.results),
            "total_calls": sum(len(routine.calls) for r in self.results for routine in r.routines),
        }
        
        if self.schema:
            stats["total_nodes"] = len(self.schema.nodes)
            stats["total_relationships"] = len(self.schema.relationships)
            
            # Count by precision
            precision_counts = {}
            for result in self.results:
                for routine in result.routines:
                    if routine.precision:
                        precision_counts[routine.precision] = precision_counts.get(routine.precision, 0) + 1
            stats["precision_counts"] = precision_counts
        
        return stats


def main():
    """Main entry point for command-line usage"""
    parser = argparse.ArgumentParser(
        description="Parse LAPACK/BLAS source code and build graph database"
    )
    parser.add_argument(
        "lapack_root",
        type=Path,
        help="Root directory of LAPACK source code"
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("lapack_graph_output"),
        help="Output directory for graph data (default: lapack_graph_output)"
    )
    parser.add_argument(
        "--directories",
        nargs="+",
        default=["BLAS/SRC", "SRC"],
        help="Directories to parse (default: BLAS/SRC SRC)"
    )
    parser.add_argument(
        "--neo4j-uri",
        help="Neo4j URI (e.g., bolt://localhost:7687)"
    )
    parser.add_argument(
        "--neo4j-user",
        default="neo4j",
        help="Neo4j username (default: neo4j)"
    )
    parser.add_argument(
        "--neo4j-password",
        help="Neo4j password"
    )
    parser.add_argument(
        "--fortran-src",
        default="fortran-src",
        help="Path to fortran-src executable (default: fortran-src)"
    )
    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Enable verbose logging"
    )
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    # Build the graph
    builder = LAPACKGraphBuilder(args.lapack_root, args.fortran_src)
    
    # Parse sources
    routines_by_file = builder.parse_sources(args.directories)
    
    if not routines_by_file:
        logger.error("No routines found!")
        sys.exit(1)
    
    # Build schema
    schema = builder.build_graph_schema(routines_by_file)
    
    # Export schema
    builder.export_schema(args.output_dir)
    
    # Generate and print statistics
    stats = builder.generate_statistics()
    print("\n=== Parsing Statistics ===")
    for key, value in stats.items():
        print(f"{key}: {value}")
    
    # Load to Neo4j if credentials provided
    if args.neo4j_uri and args.neo4j_password:
        builder.load_to_neo4j(args.neo4j_uri, args.neo4j_user, args.neo4j_password)


if __name__ == "__main__":
    main()