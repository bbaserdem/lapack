# LAPACK Graph Visualization Guide

This guide explains how to visualize the LAPACK computational graph using Neo4j Browser integration.

## Overview

We've created tools that allow you to:
1. Send Cypher queries directly to Neo4j Browser for visualization
2. Create interactive HTML guides with pre-built queries
3. Open specific visualizations with a single command

## Quick Start

### 1. Create an Interactive Guide

```bash
python -m lapack_util.auto_visualize --guide
```

This creates an HTML file with all visualization queries. You can:
- Drag and drop the HTML file into Neo4j Browser
- Click any "Run Query" button to execute that visualization

### 2. Open All Visualizations

```bash
python -m lapack_util.auto_visualize --all
```

This opens all visualization queries in separate browser tabs (with a 2-second delay between each).

### 3. Visualize a Specific Routine

```bash
# Visualize DGEMM and its connections
python examples/visualize_routine.py DGEMM

# Visualize with deeper call chain (depth 3)
python examples/visualize_routine.py XERBLA --depth 3
```

### 4. Run a Custom Query

```bash
python -m lapack_util.auto_visualize -q "MATCH (r:Routine {type: 'blas3'}) RETURN r LIMIT 50"
```

## Available Visualizations

1. **Graph Overview** - Random sample of routine connections
2. **Most Connected Routines** - Hub routines that are central to LAPACK
3. **Precision Distribution Network** - How different precisions interact
4. **Category Interaction Pattern** - How different routine categories call each other
5. **XERBLA Error Handling Network** - Error handling structure
6. **BLAS Level 3 Operations** - Matrix-matrix operations network
7. **Factorization Routines Network** - All factorization routines and dependencies
8. **Eigenvalue Solver Dependencies** - Eigenvalue computation structure
9. **File-based Clustering** - Routines grouped by source files
10. **Isolated Routine Groups** - Routines with no connections

## Using the Tools

### Browser Integration Module

```python
from lapack_util.browser_integration import Neo4jBrowserIntegration

browser = Neo4jBrowserIntegration()

# Open a query in browser
query = "MATCH (r:Routine) RETURN r LIMIT 25"
browser.open_with_query(query)

# Create an HTML guide
queries = {
    "My Query 1": "MATCH ...",
    "My Query 2": "MATCH ..."
}
browser.create_guide(queries, "my_guide.html")
```

### Auto Visualizer Module

```python
from lapack_util.auto_visualize import LAPACKAutoVisualizer

viz = LAPACKAutoVisualizer()

# Open all visualizations
viz.open_all_visualizations()

# Visualize specific routine
viz.visualize_routine_network("DGEMM", depth=2)

# Create interactive guide
viz.create_interactive_guide()
```

## Query Templates

Pre-built query templates are available in `lapack_util.query_templates`:

```python
from lapack_util.query_templates import QUERY_TEMPLATES, get_routine_dependencies

# Use a template
precision_query = QUERY_TEMPLATES["routine_by_precision"]

# Get dependencies for a specific routine
deps_query = get_routine_dependencies("DGEMM", depth=3)
```

## Tips for Neo4j Browser

1. **Visualization Settings**: Click the gear icon in Neo4j Browser to adjust:
   - Node colors by property (e.g., color by precision or category)
   - Node labels to show routine names
   - Relationship thickness based on call frequency

2. **Layouts**: Try different graph layouts:
   - Force-directed (default) - Good for general visualization
   - Hierarchical - Good for dependency trees
   - Radial - Good for hub-and-spoke patterns

3. **Filtering**: Use the filter panel to:
   - Show only certain node types
   - Filter by properties (precision, category, etc.)
   - Hide/show relationship types

4. **Export**: You can export visualizations as:
   - PNG/SVG images
   - JSON data
   - CSV tables

## MCP Integration

These tools can also be used with MCP (Model Context Protocol) to execute queries programmatically:

```python
# Execute query via MCP
mcp__neo4j-lapack__read_neo4j_cypher(query)
```

## Example Workflow

1. Start by creating the interactive guide:
   ```bash
   python -m lapack_util.auto_visualize --guide
   ```

2. Open Neo4j Browser: http://localhost:7474/

3. Drag the generated HTML file into the browser

4. Click queries to explore different aspects of the graph

5. For specific routines of interest:
   ```bash
   python examples/visualize_routine.py DGETRF --depth 3
   ```

6. Use custom queries for detailed analysis:
   ```bash
   python -m lapack_util.auto_visualize -q "MATCH (r:Routine)-[:CALLS]->(r) RETURN r"
   ```

## Troubleshooting

- **Browser doesn't open**: Check that Neo4j is running on the expected port (7474)
- **Empty visualizations**: Ensure LAPACK data is loaded in Neo4j
- **Too many nodes**: Adjust LIMIT clauses in queries to reduce complexity
- **Can't see labels**: Click on nodes and check the property panel