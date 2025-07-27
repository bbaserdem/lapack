# Fortran Mapper Visualization Module

A dynamic, web-based graph visualization system for exploring Neo4j databases created by fortran-mapper. This module automatically discovers your graph schema and provides interactive visualizations that adapt to any data structure.

## Features

### 🎯 Dynamic Schema Discovery
- Automatically detects node labels, relationship types, and properties
- No hardcoded assumptions about data structure
- Works with any Neo4j graph, not just Fortran/LAPACK data

### 📊 Multiple Visualization Types
1. **Overview Graph** - Sample of all node types and relationships
2. **Statistics Graph** - Meta-view showing node/relationship distributions
3. **Focused View** - Deep dive into specific node types or relationships
4. **Neighborhood Explorer** - Explore connections around specific nodes

### 🎨 Flexible Visualization Options
- **Color schemes**: By node type, connectivity, or custom properties
- **Node sizing**: Uniform, by degree, or by property values
- **Layouts**: Force-directed, radial, or hierarchical
- **Search**: Find nodes by property values
- **Export**: Save graph data as JSON

## Installation

The visualization module is included with fortran-mapper. No additional installation required.

## Usage

### 1. Start the Visualization Server

```bash
# Basic usage - opens browser automatically
fortran-mapper visualize serve

# Custom port
fortran-mapper visualize serve --port 9000

# Without auto-opening browser
fortran-mapper visualize serve --no-open

# With Neo4j credentials
fortran-mapper visualize serve --username neo4j --password mypassword
```

### 2. View Graph Schema

```bash
# Display schema in terminal
fortran-mapper visualize schema

# Export schema to JSON file
fortran-mapper visualize schema --output schema.json
```

### 3. Export Visualization Data

```bash
# Export all graph data to JSON files
fortran-mapper visualize export ./viz-data/

# This creates:
# - schema.json
# - overview_graph.json
# - statistics_graph.json
# - focused_<label>_graph.json (for each node type)
```

## Web Interface Guide

### Navigation
- **Zoom**: Mouse wheel or pinch gesture
- **Pan**: Click and drag on empty space
- **Move nodes**: Click and drag individual nodes
- **Fix node position**: Hold Shift while dragging

### Controls Panel
- **Graph Type**: Switch between visualization modes
- **Color By**: Change node coloring scheme
- **Node Size**: Adjust sizing method
- **Layout**: Switch graph layout algorithm
- **Labels**: Toggle node and edge labels

### Graph Types Explained

#### Overview
Shows a representative sample of your entire graph, with nodes from each label type and their relationships.

#### Statistics
Displays a meta-graph where:
- Nodes represent node types (labels)
- Node size indicates count
- Edges show relationships between types
- Edge labels show relationship counts

#### Focused View
Explore specific parts of your graph:
- Select a node type to focus on
- Optionally filter by relationship type
- Shows only relevant nodes and connections

#### Node Neighborhood
Explore the local structure around a specific node:
- Search for a node by any property
- Adjust depth to see more distant connections
- Nodes are colored by distance from center

### Customization

#### Color Schemes
- **Node Type**: Different color for each label
- **Connectivity**: Gradient based on connection count
- **Custom Property**: Color by any node property (numeric or categorical)

#### Size Options
- **Uniform**: All nodes same size
- **By Connectivity**: Larger nodes have more connections
- **By Property**: Size based on numeric property value

## API Endpoints

The visualization server provides these REST endpoints:

- `GET /api/schema` - Get graph schema information
- `GET /api/graph/overview?limit=500` - Get overview graph
- `GET /api/graph/statistics` - Get statistics graph
- `GET /api/graph/focused?label=<type>&relationship=<type>` - Get focused graph
- `GET /api/graph/neighborhood?node_id=<id>&depth=2` - Get node neighborhood
- `GET /api/search?q=<query>&limit=50` - Search nodes

## Example Workflows

### Exploring a Fortran Codebase

1. Parse your Fortran code and load into Neo4j:
   ```bash
   fortran-mapper parse src/ --output neo4j
   ```

2. Start the visualization:
   ```bash
   fortran-mapper visualize serve
   ```

3. In the browser:
   - Start with "Overview" to understand the structure
   - Switch to "Statistics" to see distribution
   - Use "Focused View" on "Routine" nodes to explore functions
   - Search for specific routines and explore their neighborhoods

### Analyzing Call Relationships

1. Select "Focused View"
2. Choose "Routine" as node type
3. Choose "CALLS" as relationship type
4. This shows only the function call graph

### Finding Isolated Components

1. Use "Overview" mode
2. Look for disconnected clusters
3. Click nodes for details
4. Use neighborhood view to explore local structure

## Performance Tips

- For large graphs (>10,000 nodes):
  - Use focused views instead of overview
  - Disable labels for better performance
  - Export data and use external tools for very large graphs

- Browser performance:
  - Chrome/Edge typically perform best
  - Close other tabs for better performance
  - Use hardware acceleration if available

## Extending the Visualization

### Adding Custom Graph Types

Edit `graph_generator.py` to add new visualization methods:

```python
def generate_custom_graph(self, param1, param2):
    # Your custom query logic
    with self.driver.session() as session:
        result = session.run("YOUR CYPHER QUERY")
        # Process results into nodes and edges
    return {"nodes": nodes, "edges": edges}
```

### Customizing the UI

The visualization uses standard web technologies:
- `static/graph_viewer.html` - Main HTML structure
- `static/styles.css` - All styling
- `static/graph_viewer.js` - Visualization logic (D3.js)

## Troubleshooting

### "Cannot connect to Neo4j"
- Ensure Neo4j is running: `fortran-mapper neo4j status`
- Check credentials: Add `--username` and `--password`
- Verify connection URI: Default is `bolt://localhost:7687`

### "No data displayed"
- Check that your Neo4j database has data: `fortran-mapper stats`
- Try refreshing with the "Refresh" button
- Check browser console for errors (F12)

### "Slow performance"
- Reduce node limit in focused views
- Disable labels for large graphs
- Try different layout algorithms
- Use Chrome/Edge for best performance

## Integration with Other Tools

### Export for Gephi
```bash
fortran-mapper export graphml output.graphml
```

### Export for Cytoscape
```bash
fortran-mapper export json output.json
```

### Use with Jupyter Notebooks
```python
from py2neo import Graph
import json

# Load exported data
with open('viz-data/overview_graph.json') as f:
    data = json.load(f)

# Process with pandas, networkx, etc.
```

## Contributing

To contribute to the visualization module:

1. The code is in `src/fortran_mapper/visualization/`
2. Tests are in `tests/test_visualization.py`
3. Static files are in `visualization/static/`

Key files:
- `graph_generator.py` - Graph data generation logic
- `web_server.py` - HTTP server implementation
- `static/graph_viewer.js` - Frontend visualization code