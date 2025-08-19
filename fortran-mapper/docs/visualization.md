# Interactive Graph Visualization

The fortran-mapper visualization module provides an interactive, web-based interface for exploring Fortran code structure using force-directed graphs.

## Features

- **Animated Force-Directed Layout**: Real-time physics simulation for natural graph layouts
- **Multiple Visualization Modes**:
  - **Overview**: Random sample of nodes showing overall structure
  - **Centered**: Focus on a specific node and its neighborhood
  - **Hierarchy**: Call hierarchy visualization for routines
- **Interactive Filtering**:
  - Filter by node types (Routines, Files, Modules, etc.)
  - Filter by relationship types (CALLS, DEFINED_IN, etc.)
  - Adjustable node count and connection depth
- **Real-time Controls**:
  - Adjustable physics parameters (link distance, repulsion force)
  - Node search with auto-complete
  - Zoom and pan navigation
- **Export Options**:
  - Export current view as SVG
  - Export graph data as JSON
  - Generate static HTML files for offline viewing

## Installation

The visualization module requires D3.js, which is managed via pnpm:

```bash
# In the project root directory
pnpm install
```

This installs D3.js locally, ensuring the visualization works offline without internet access.

## Usage

### Interactive Server

Start the visualization server to explore graphs interactively:

```bash
# Basic usage
fortran-mapper visualize serve

# Custom host and port
fortran-mapper visualize serve --host 0.0.0.0 --port 8888

# Don't open browser automatically
fortran-mapper visualize serve --no-browser

# With Neo4j authentication
fortran-mapper visualize serve --neo4j-user neo4j --neo4j-password password
```

Once started, open your browser to `http://localhost:8080` (or your custom host/port).

### Static Export

Export visualizations as self-contained HTML files:

```bash
# Export overview graph
fortran-mapper visualize export output.html

# Export with custom node limit
fortran-mapper visualize export output.html --limit 200

# Export centered on a specific routine
fortran-mapper visualize export output.html --center-node DGEMM --depth 3

# Export call hierarchy for a routine
fortran-mapper visualize export output.html --mode hierarchy --routine DGEMM --depth 4
```

## Visualization Modes

### Overview Mode

Shows a random sample of nodes from the graph, useful for understanding overall structure:

- Adjustable node limit (10-500)
- Random sampling ensures diverse representation
- Node size reflects connection count

### Centered Mode

Focuses on a specific node and shows its neighborhood:

- Search or click to select center node
- Adjustable depth (1-5 levels)
- Node size decreases with distance from center
- Useful for exploring local structure

### Hierarchy Mode

Specialized view for routine call hierarchies:

- Shows calls to/from a specific routine
- Directional arrows indicate call direction
- Adjustable depth for call chain exploration
- Ideal for understanding routine dependencies

## Interactive Controls

### Graph Settings

- **Maximum Nodes**: Control how many nodes to display (overview mode)
- **Link Distance**: Adjust spacing between connected nodes
- **Repulsion Force**: Control how strongly nodes push apart
- **Depth**: Set exploration depth (centered/hierarchy modes)

### Filtering

- **Node Types**: Toggle visibility of different node types
- **Relationship Types**: Show/hide specific relationship types
- **Select/Deselect All**: Quick filter management

### Navigation

- **Pan**: Click and drag on empty space
- **Zoom**: Scroll or pinch to zoom
- **Select Node**: Click to select and view details
- **Drag Node**: Click and drag nodes to reposition
- **Search**: Type to find specific nodes by name

## Examples

### Visualizing LAPACK Structure

```bash
# Parse LAPACK source
fortran-mapper parse /path/to/lapack/SRC --neo4j-uri bolt://localhost:7687

# Start visualization server
fortran-mapper visualize serve

# Export BLAS Level 3 routines
fortran-mapper visualize export blas3.html --center-node DGEMM --depth 2
```

### Creating Presentation Graphics

```bash
# Export high-level overview
fortran-mapper visualize export overview.html --limit 50

# Export specific routine analysis
fortran-mapper visualize export dgetrf-calls.html \
    --mode hierarchy --routine DGETRF --depth 3

# Export module structure
fortran-mapper visualize export modules.html \
    --mode overview --limit 100
# Then manually filter to show only Module nodes in the UI
```

### Offline Documentation

Generate static visualizations for documentation:

```python
from neo4j import GraphDatabase
from fortran_mapper.visualization import VisualizationServer, GraphDataGenerator

# Connect to Neo4j
driver = GraphDatabase.driver("bolt://localhost:7687")
generator = GraphDataGenerator(driver)
server = VisualizationServer(driver)

# Generate multiple views
views = [
    ("overview.html", generator.generate_overview_graph(node_limit=100)),
    ("dgemm-calls.html", generator.generate_call_hierarchy("DGEMM", max_depth=3)),
    ("file-structure.html", generator.generate_overview_graph(node_types=["File"]))
]

# Export all views
for filename, graph_data in views:
    server.export_static_visualization(Path(filename), graph_data)
    print(f"Exported {filename}")

driver.close()
```

## Tips and Best Practices

1. **Performance**: For large graphs, start with a lower node limit and increase gradually
2. **Exploration**: Use centered mode to explore specific areas of interest
3. **Filtering**: Combine node and relationship filters to focus on specific patterns
4. **Layout**: Let the simulation stabilize before exporting for best visual results
5. **Offline Use**: Exported HTML files include D3.js, making them fully self-contained

## Troubleshooting

### D3.js Not Found

If you see "D3.js not found" error:

```bash
# Ensure you're in the project root
cd /path/to/project

# Install dependencies
pnpm install
```

### Graph Not Loading

1. Check Neo4j is running: `neo4j status`
2. Verify connection: `fortran-mapper stats`
3. Ensure data is loaded: `fortran-mapper query "MATCH (n) RETURN count(n)"`

### Performance Issues

- Reduce node limit for smoother animation
- Adjust physics parameters (lower repulsion, higher link distance)
- Use filtering to show fewer elements
- Try different visualization modes

## API Reference

### GraphDataGenerator

```python
# Create generator
generator = GraphDataGenerator(driver)

# Get available types
node_types = generator.get_available_node_types()
rel_types = generator.get_available_relationship_types()

# Generate graphs
overview = generator.generate_overview_graph(
    node_limit=100,
    node_types=["Routine", "Module"],
    relationship_types=["CALLS", "USES"]
)

centered = generator.generate_centered_graph(
    center_node_name="DGEMM",
    depth=2,
    node_types=["Routine"]
)

hierarchy = generator.generate_call_hierarchy(
    routine_name="DGETRF",
    direction="both",  # "calls", "called_by", or "both"
    max_depth=3
)
```

### VisualizationServer

```python
# Create server
server = VisualizationServer(driver, host="127.0.0.1", port=8080)

# Start interactive server
server.start(auto_open=True)

# Export static visualization
server.export_static_visualization(
    Path("output.html"),
    graph_data
)
```