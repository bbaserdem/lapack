"""Tests for the visualization module."""

import unittest
from unittest.mock import Mock, MagicMock, patch
import json
from pathlib import Path
import tempfile
import shutil

from src.fortran_mapper.visualization import DynamicGraphGenerator, VisualizationServer


class TestDynamicGraphGenerator(unittest.TestCase):
    """Test the dynamic graph generator."""
    
    def setUp(self):
        """Set up test fixtures."""
        # Create a mock Neo4j driver
        self.mock_driver = Mock()
        self.mock_session = MagicMock()
        self.mock_driver.session.return_value.__enter__.return_value = self.mock_session
        self.mock_driver.session.return_value.__exit__.return_value = None
        
        self.generator = DynamicGraphGenerator(self.mock_driver)
    
    def test_discover_schema(self):
        """Test schema discovery."""
        # Mock the query results
        self.mock_session.run.side_effect = [
            # Labels query
            Mock(single=Mock(return_value={"labels": ["Routine", "File"]})),
            # Relationship types query
            Mock(single=Mock(return_value={"types": ["CALLS", "DEFINED_IN"]})),
            # Node properties for Routine
            [{"key": "name"}, {"key": "precision"}, {"key": "category"}],
            # Node properties for File
            [{"key": "path"}, {"key": "name"}],
            # Relationship counts
            [{"type": "CALLS", "count": 100}, {"type": "DEFINED_IN", "count": 50}],
            # Node count for Routine
            Mock(single=Mock(return_value={"count": 200})),
            # Node count for File
            Mock(single=Mock(return_value={"count": 75}))
        ]
        
        schema = self.generator.discover_schema()
        
        # Verify schema structure
        self.assertIn("node_labels", schema)
        self.assertIn("relationship_types", schema)
        self.assertIn("node_properties", schema)
        self.assertIn("statistics", schema)
        
        # Verify content
        self.assertEqual(schema["node_labels"], ["Routine", "File"])
        self.assertEqual(schema["relationship_types"], ["CALLS", "DEFINED_IN"])
        self.assertEqual(schema["node_properties"]["Routine"], ["name", "precision", "category"])
        self.assertEqual(schema["statistics"]["node_counts"]["Routine"], 200)
    
    def test_generate_overview_graph(self):
        """Test overview graph generation."""
        # Set up cached schema
        self.generator._schema_cache = {
            "node_labels": ["Routine", "File"],
            "node_properties": {
                "Routine": ["name", "precision"],
                "File": ["path", "name"]
            },
            "statistics": {}
        }
        
        # Mock query results
        self.mock_session.run.side_effect = [
            # Nodes query for Routine
            [
                {"id": 1, "properties": {"name": "DGEMM", "precision": "d"}},
                {"id": 2, "properties": {"name": "DGETRF", "precision": "d"}}
            ],
            # Nodes query for File
            [
                {"id": 3, "properties": {"path": "/src/dgemm.f", "name": "dgemm.f"}}
            ],
            # Relationships query
            [
                {"source_id": 2, "target_id": 1, "type": "CALLS", "properties": None}
            ]
        ]
        
        result = self.generator.generate_overview_graph(limit=10)
        
        # Verify structure
        self.assertIn("nodes", result)
        self.assertIn("edges", result)
        self.assertIn("schema", result)
        
        # Verify nodes
        self.assertEqual(len(result["nodes"]), 3)
        self.assertEqual(result["nodes"][0]["label"], "DGEMM")
        self.assertEqual(result["nodes"][0]["type"], "Routine")
        
        # Verify edges
        self.assertEqual(len(result["edges"]), 1)
        self.assertEqual(result["edges"][0]["type"], "CALLS")
    
    def test_search_nodes(self):
        """Test node search functionality."""
        # Mock query results
        self.mock_session.run.return_value = [
            {
                "id": 1,
                "properties": {"name": "DGEMM", "precision": "d"},
                "labels": ["Routine"]
            },
            {
                "id": 2,
                "properties": {"name": "SGEMM", "precision": "s"},
                "labels": ["Routine"]
            }
        ]
        
        results = self.generator.search_nodes("GEMM")
        
        self.assertEqual(len(results), 2)
        self.assertEqual(results[0]["display"], "DGEMM")
        self.assertEqual(results[0]["labels"], ["Routine"])
        self.assertIn("matching_property", results[0])
    
    def test_export_all_graphs(self):
        """Test exporting all graph types."""
        # Create temporary directory
        with tempfile.TemporaryDirectory() as temp_dir:
            # Set up cached schema
            self.generator._schema_cache = {
                "node_labels": ["Routine"],
                "relationship_types": ["CALLS"],
                "node_properties": {"Routine": ["name"]},
                "statistics": {
                    "node_counts": {"Routine": 10},
                    "relationship_counts": {"CALLS": 20}
                }
            }
            
            # Mock the generation methods
            self.generator.generate_overview_graph = Mock(return_value={"nodes": [], "edges": []})
            self.generator.generate_statistics_graph = Mock(return_value={"nodes": [], "edges": []})
            self.generator.generate_focused_graph = Mock(return_value={"nodes": [], "edges": []})
            
            # Export
            self.generator.export_all_graphs(temp_dir)
            
            # Verify files were created
            files = list(Path(temp_dir).glob("*.json"))
            self.assertGreater(len(files), 0)
            
            # Verify schema was exported
            schema_file = Path(temp_dir) / "schema.json"
            self.assertTrue(schema_file.exists())
            
            with open(schema_file) as f:
                exported_schema = json.load(f)
                self.assertEqual(exported_schema["node_labels"], ["Routine"])


class TestVisualizationServer(unittest.TestCase):
    """Test the visualization web server."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.mock_driver = Mock()
        self.server = VisualizationServer(self.mock_driver, port=8888)
    
    def test_initialization(self):
        """Test server initialization."""
        self.assertEqual(self.server.port, 8888)
        self.assertIsNotNone(self.server.generator)
        self.assertEqual(self.server.driver, self.mock_driver)
    
    def test_export_data(self):
        """Test data export functionality."""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Mock the generator's export method
            self.server.generator.export_all_graphs = Mock()
            
            self.server.export_data(temp_dir)
            
            # Verify export was called
            self.server.generator.export_all_graphs.assert_called_once_with(temp_dir)


class TestVisualizationIntegration(unittest.TestCase):
    """Integration tests for visualization commands."""
    
    @patch('src.fortran_mapper.commands.Neo4jClient')
    @patch('src.fortran_mapper.commands.VisualizationServer')
    def test_visualize_serve_command(self, mock_server_class, mock_client_class):
        """Test the visualize serve command."""
        from src.fortran_mapper.commands import visualize_serve_command
        
        # Set up mocks
        mock_client = Mock()
        mock_client_class.return_value.__enter__.return_value = mock_client
        mock_client_class.return_value.__exit__.return_value = None
        
        mock_server = Mock()
        mock_server_class.return_value = mock_server
        
        # Run command
        visualize_serve_command(port=9999, auto_open=False)
        
        # Verify server was created and started
        mock_server_class.assert_called_once_with(mock_client.driver, port=9999)
        mock_server.start.assert_called_once()
    
    @patch('src.fortran_mapper.commands.Neo4jClient')
    @patch('src.fortran_mapper.commands.DynamicGraphGenerator')
    def test_visualize_export_command(self, mock_generator_class, mock_client_class):
        """Test the visualize export command."""
        from src.fortran_mapper.commands import visualize_export_command
        
        # Set up mocks
        mock_client = Mock()
        mock_client_class.return_value.__enter__.return_value = mock_client
        mock_client_class.return_value.__exit__.return_value = None
        
        mock_generator = Mock()
        mock_generator_class.return_value = mock_generator
        
        with tempfile.TemporaryDirectory() as temp_dir:
            # Run command
            visualize_export_command(temp_dir)
            
            # Verify generator was created and export was called
            mock_generator_class.assert_called_once_with(mock_client.driver)
            mock_generator.export_all_graphs.assert_called_once_with(temp_dir)


if __name__ == '__main__':
    unittest.main()