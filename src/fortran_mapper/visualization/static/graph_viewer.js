// Graph Viewer - Dynamic visualization for any Neo4j graph
(function() {
    'use strict';
    
    // Global variables
    let graphData = null;
    let schema = null;
    let simulation = null;
    let svg = null;
    let container = null;
    let zoom = null;
    let colorScale = null;
    let currentGraphType = 'overview';
    let selectedNode = null;
    
    // Color palettes
    const colorPalettes = {
        categorical: d3.schemeCategory10,
        sequential: d3.interpolateBlues,
        diverging: d3.interpolateRdBu
    };
    
    // Initialize the application
    async function init() {
        try {
            // Load schema first
            await loadSchema();
            
            // Setup UI controls
            setupEventListeners();
            
            // Load initial graph
            await loadGraphData();
            
            hideLoading();
            initializeGraph();
        } catch (error) {
            console.error('Initialization error:', error);
            showError('Failed to initialize visualization. Check console for details.');
        }
    }
    
    // Load schema from API
    async function loadSchema() {
        try {
            const response = await fetch('/api/schema');
            if (!response.ok) {
                throw new Error(`Failed to load schema: ${response.statusText}`);
            }
            
            schema = await response.json();
            
            // Check for error in schema
            if (schema.error) {
                console.error('Schema error:', schema.error);
                showError(`Database error: ${schema.error}`);
                return;
            }
            
            updateSchemaUI();
        } catch (error) {
            console.error('Error loading schema:', error);
            showError(`Failed to load schema: ${error.message}`);
        }
    }
    
    // Update UI based on schema
    function updateSchemaUI() {
        // Update node type dropdown
        const nodeTypeSelect = document.getElementById('nodeType');
        nodeTypeSelect.innerHTML = '<option value="">All Node Types</option>';
        schema.node_labels.forEach(label => {
            const option = document.createElement('option');
            option.value = label;
            option.textContent = `${label} (${schema.statistics.node_counts[label] || 0})`;
            nodeTypeSelect.appendChild(option);
        });
        
        // Update relationship type dropdown
        const relTypeSelect = document.getElementById('relationType');
        relTypeSelect.innerHTML = '<option value="">All Relationships</option>';
        schema.relationship_types.forEach(type => {
            const option = document.createElement('option');
            option.value = type;
            option.textContent = `${type} (${schema.statistics.relationship_counts[type] || 0})`;
            relTypeSelect.appendChild(option);
        });
        
        // Update filter checkboxes
        const nodeTypeCheckboxes = document.getElementById('nodeTypeCheckboxes');
        nodeTypeCheckboxes.innerHTML = '';
        schema.node_labels.forEach(label => {
            const div = document.createElement('div');
            div.innerHTML = `
                <label>
                    <input type="checkbox" name="nodeType" value="${label}" checked>
                    ${label} (${schema.statistics.node_counts[label] || 0})
                </label>
            `;
            nodeTypeCheckboxes.appendChild(div);
        });
        
        const relTypeCheckboxes = document.getElementById('relationshipTypeCheckboxes');
        relTypeCheckboxes.innerHTML = '';
        schema.relationship_types.forEach(type => {
            const div = document.createElement('div');
            div.innerHTML = `
                <label>
                    <input type="checkbox" name="relationType" value="${type}" checked>
                    ${type} (${schema.statistics.relationship_counts[type] || 0})
                </label>
            `;
            relTypeCheckboxes.appendChild(div);
        });
        
        // Update schema info display
        const schemaContent = document.getElementById('schemaContent');
        let schemaHtml = '';
        
        schema.node_labels.forEach(label => {
            schemaHtml += `<div class="schema-item">
                <strong>${label}:</strong> ${schema.statistics.node_counts[label] || 0} nodes
            </div>`;
        });
        
        schemaContent.innerHTML = schemaHtml;
    }
    
    // Load graph data based on current settings
    async function loadGraphData() {
        const graphType = document.getElementById('graphType').value;
        currentGraphType = graphType;
        
        showLoading();
        
        try {
            let response;
            
            switch(graphType) {
                case 'overview':
                    const maxNodes = document.getElementById('maxNodes').value || 500;
                    response = await fetch(`/api/graph/overview?limit=${maxNodes}`);
                    break;
                    
                case 'statistics':
                    response = await fetch('/api/graph/statistics');
                    break;
                    
                case 'focused':
                    const nodeType = document.getElementById('nodeType').value;
                    const relType = document.getElementById('relationType').value;
                    let url = '/api/graph/focused?limit=500';
                    if (nodeType) url += `&label=${encodeURIComponent(nodeType)}`;
                    if (relType) url += `&relationship=${encodeURIComponent(relType)}`;
                    
                    if (!nodeType) {
                        showError('Please select a node type for focused view');
                        hideLoading();
                        return;
                    }
                    
                    response = await fetch(url);
                    break;
                    
                case 'neighborhood':
                    if (!selectedNode) {
                        showError('Please search and select a node first');
                        hideLoading();
                        return;
                    }
                    
                    const depth = document.getElementById('depth').value;
                    response = await fetch(
                        `/api/graph/neighborhood?node_id=${encodeURIComponent(selectedNode)}&depth=${depth}&limit=200`
                    );
                    break;
                    
                case 'filtered':
                    // Get selected filters
                    const selectedNodeTypes = Array.from(
                        document.querySelectorAll('input[name="nodeType"]:checked')
                    ).map(cb => cb.value);
                    
                    const selectedRelTypes = Array.from(
                        document.querySelectorAll('input[name="relationType"]:checked')
                    ).map(cb => cb.value);
                    
                    const connectedOnly = document.getElementById('connectedOnly').checked;
                    const maxNodesFiltered = document.getElementById('maxNodes').value || 500;
                    
                    // Build query string
                    let queryParams = new URLSearchParams();
                    queryParams.append('limit', maxNodesFiltered);
                    queryParams.append('connected_only', connectedOnly);
                    
                    selectedNodeTypes.forEach(type => {
                        queryParams.append('node_labels[]', type);
                    });
                    
                    selectedRelTypes.forEach(type => {
                        queryParams.append('relationship_types[]', type);
                    });
                    
                    response = await fetch(`/api/graph/filtered?${queryParams.toString()}`);
                    break;
                    
                default:
                    throw new Error(`Unknown graph type: ${graphType}`);
            }
            
            if (!response.ok) {
                throw new Error(`Failed to load graph: ${response.statusText}`);
            }
            
            graphData = await response.json();
            updateGraphInfo();
            
            // Re-render if graph is already initialized
            if (svg) {
                renderGraph();
            }
            
        } catch (error) {
            console.error('Error loading graph:', error);
            showError(`Failed to load graph data: ${error.message}`);
        } finally {
            hideLoading();
        }
    }
    
    // Initialize the graph visualization
    function initializeGraph() {
        const width = window.innerWidth;
        const height = window.innerHeight;
        
        // Clear existing SVG
        d3.select('#graph').selectAll('*').remove();
        
        // Create SVG
        svg = d3.select('#graph')
            .append('svg')
            .attr('width', width)
            .attr('height', height);
        
        // Add arrow markers for directed edges
        const defs = svg.append('defs');
        
        defs.append('marker')
            .attr('id', 'arrow')
            .attr('viewBox', '0 -5 10 10')
            .attr('refX', 20)
            .attr('refY', 0)
            .attr('markerWidth', 6)
            .attr('markerHeight', 6)
            .attr('orient', 'auto')
            .append('path')
            .attr('d', 'M0,-5L10,0L0,5')
            .attr('class', 'arrow');
        
        // Create container for zoom
        container = svg.append('g');
        
        // Add zoom behavior
        zoom = d3.zoom()
            .scaleExtent([0.1, 10])
            .on('zoom', (event) => {
                container.attr('transform', event.transform);
            });
        
        svg.call(zoom);
        
        // Initial render
        renderGraph();
        updateLegend();
    }
    
    // Render the graph
    function renderGraph() {
        if (!graphData || !graphData.nodes) return;
        
        const width = window.innerWidth;
        const height = window.innerHeight;
        const layout = document.getElementById('layout').value;
        
        // Prepare data
        const nodes = graphData.nodes.map(d => ({...d}));
        const links = graphData.edges.map(d => ({...d}));
        
        // Clear container
        container.selectAll('*').remove();
        
        // Setup color scale
        updateColorScale();
        
        // Create simulation based on layout
        if (layout === 'force') {
            simulation = d3.forceSimulation(nodes)
                .force('link', d3.forceLink(links)
                    .id(d => d.id)
                    .distance(graphData.type === 'statistics' ? 150 : 80))
                .force('charge', d3.forceManyBody()
                    .strength(graphData.type === 'statistics' ? -1000 : -300))
                .force('center', d3.forceCenter(width / 2, height / 2))
                .force('collision', d3.forceCollide().radius(d => getNodeSize(d) + 5));
                
        } else if (layout === 'radial') {
            // Radial layout
            const radius = Math.min(width, height) / 3;
            const angleStep = (2 * Math.PI) / nodes.length;
            
            nodes.forEach((node, i) => {
                const angle = i * angleStep;
                node.x = width / 2 + radius * Math.cos(angle);
                node.y = height / 2 + radius * Math.sin(angle);
                node.fx = node.x;
                node.fy = node.y;
            });
            
            simulation = d3.forceSimulation(nodes)
                .force('link', d3.forceLink(links).id(d => d.id));
                
        } else if (layout === 'hierarchical') {
            // Simple hierarchical layout
            const levelHeight = height / 5;
            const nodesByLevel = {};
            
            // Assign levels based on connectivity
            nodes.forEach(node => {
                const inDegree = links.filter(l => l.target === node.id).length;
                const level = Math.min(4, inDegree);
                if (!nodesByLevel[level]) nodesByLevel[level] = [];
                nodesByLevel[level].push(node);
            });
            
            // Position nodes by level
            Object.entries(nodesByLevel).forEach(([level, levelNodes]) => {
                const levelWidth = width / (levelNodes.length + 1);
                levelNodes.forEach((node, i) => {
                    node.x = levelWidth * (i + 1);
                    node.y = levelHeight * (parseInt(level) + 1);
                    node.fx = node.x;
                    node.fy = node.y;
                });
            });
            
            simulation = d3.forceSimulation(nodes)
                .force('link', d3.forceLink(links).id(d => d.id));
        }
        
        // Create links
        const link = container.append('g')
            .selectAll('line')
            .data(links)
            .join('line')
            .attr('class', 'link')
            .attr('marker-end', 'url(#arrow)');
        
        // Create link labels if enabled
        const showEdgeLabels = document.getElementById('showEdgeLabels').checked;
        const linkLabel = container.append('g')
            .selectAll('text')
            .data(links)
            .join('text')
            .attr('class', 'link-label')
            .text(d => d.type || d.label || '')
            .style('display', showEdgeLabels ? 'block' : 'none');
        
        // Create nodes
        const node = container.append('g')
            .selectAll('g')
            .data(nodes)
            .join('g')
            .attr('class', 'node')
            .call(drag(simulation));
        
        // Add circles to nodes
        node.append('circle')
            .attr('r', d => getNodeSize(d))
            .style('fill', d => getNodeColor(d));
        
        // Add labels to nodes
        const showLabels = document.getElementById('showLabels').checked;
        node.append('text')
            .text(d => d.label || d.id)
            .style('display', showLabels ? 'block' : 'none');
        
        // Tooltip
        const tooltip = d3.select('.tooltip');
        
        node.on('mouseover', (event, d) => {
            tooltip.transition()
                .duration(200)
                .style('opacity', .9);
            tooltip.html(getTooltipContent(d))
                .style('left', (event.pageX + 10) + 'px')
                .style('top', (event.pageY - 28) + 'px');
        })
        .on('mouseout', () => {
            tooltip.transition()
                .duration(500)
                .style('opacity', 0);
        })
        .on('click', (event, d) => {
            showNodeDetails(d);
        });
        
        // Update positions on tick
        if (simulation) {
            simulation.on('tick', () => {
                link
                    .attr('x1', d => d.source.x)
                    .attr('y1', d => d.source.y)
                    .attr('x2', d => d.target.x)
                    .attr('y2', d => d.target.y);
                
                linkLabel
                    .attr('x', d => (d.source.x + d.target.x) / 2)
                    .attr('y', d => (d.source.y + d.target.y) / 2);
                
                node.attr('transform', d => `translate(${d.x},${d.y})`);
            });
        }
    }
    
    // Get node size
    function getNodeSize(node) {
        const sizeBy = document.getElementById('sizeBy').value;
        
        switch(sizeBy) {
            case 'uniform':
                return graphData.type === 'statistics' ? 20 : 8;
                
            case 'degree':
                const degree = graphData.edges.filter(e => 
                    e.source === node.id || e.target === node.id ||
                    (e.source.id && (e.source.id === node.id || e.target.id === node.id))
                ).length;
                return Math.min(5 + degree * 2, 30);
                
            case 'property':
                const prop = document.getElementById('sizeProperty').value;
                if (prop && node.properties && node.properties[prop]) {
                    const value = parseFloat(node.properties[prop]);
                    if (!isNaN(value)) {
                        return Math.min(5 + value / 10, 30);
                    }
                }
                return 8;
                
            default:
                return node.size || 8;
        }
    }
    
    // Update color scale based on settings
    function updateColorScale() {
        const colorBy = document.getElementById('colorBy').value;
        
        if (colorBy === 'type') {
            // Create categorical scale for node types
            const types = [...new Set(graphData.nodes.map(n => n.type))];
            colorScale = d3.scaleOrdinal()
                .domain(types)
                .range(colorPalettes.categorical);
                
        } else if (colorBy === 'connectivity') {
            // Create sequential scale for connectivity
            const degrees = graphData.nodes.map(node => {
                return graphData.edges.filter(e => 
                    e.source === node.id || e.target === node.id
                ).length;
            });
            
            colorScale = d3.scaleSequential()
                .domain([0, d3.max(degrees)])
                .interpolator(colorPalettes.sequential);
                
        } else if (colorBy === 'custom') {
            // Use custom property
            const prop = document.getElementById('customProperty').value;
            if (prop) {
                const values = graphData.nodes
                    .map(n => n.properties && n.properties[prop])
                    .filter(v => v !== undefined);
                    
                if (values.length > 0) {
                    // Check if numeric
                    if (values.every(v => !isNaN(parseFloat(v)))) {
                        const numValues = values.map(v => parseFloat(v));
                        colorScale = d3.scaleSequential()
                            .domain(d3.extent(numValues))
                            .interpolator(colorPalettes.sequential);
                    } else {
                        // Categorical
                        const uniqueValues = [...new Set(values)];
                        colorScale = d3.scaleOrdinal()
                            .domain(uniqueValues)
                            .range(colorPalettes.categorical);
                    }
                }
            }
        }
    }
    
    // Get node color
    function getNodeColor(node) {
        const colorBy = document.getElementById('colorBy').value;
        
        switch(colorBy) {
            case 'type':
                return colorScale(node.type);
                
            case 'connectivity':
                const degree = graphData.edges.filter(e => 
                    e.source === node.id || e.target === node.id ||
                    (e.source.id && (e.source.id === node.id || e.target.id === node.id))
                ).length;
                return colorScale(degree);
                
            case 'custom':
                const prop = document.getElementById('customProperty').value;
                if (prop && node.properties && node.properties[prop] !== undefined) {
                    const value = node.properties[prop];
                    if (!isNaN(parseFloat(value))) {
                        return colorScale(parseFloat(value));
                    }
                    return colorScale(value);
                }
                return '#999';
                
            default:
                return '#69b3a2';
        }
    }
    
    // Get tooltip content
    function getTooltipContent(node) {
        let content = `<strong>${node.label || node.id}</strong><br/>`;
        content += `Type: ${node.type}<br/>`;
        
        if (node.properties) {
            const props = Object.entries(node.properties).slice(0, 5);
            props.forEach(([key, value]) => {
                content += `${key}: ${value}<br/>`;
            });
            
            if (Object.keys(node.properties).length > 5) {
                content += `... and ${Object.keys(node.properties).length - 5} more properties`;
            }
        }
        
        const degree = graphData.edges.filter(e => 
            e.source === node.id || e.target === node.id ||
            (e.source.id && (e.source.id === node.id || e.target.id === node.id))
        ).length;
        
        content += `<br/>Connections: ${degree}`;
        
        if (node.distance !== undefined) {
            content += `<br/>Distance from center: ${node.distance}`;
        }
        
        return content;
    }
    
    // Show detailed node information
    function showNodeDetails(node) {
        const nodeInfo = document.getElementById('nodeInfo');
        
        let html = `<h4>${node.label || node.id}</h4>`;
        html += `<div class="property-item">
            <span class="property-key">Type:</span>
            <span class="property-value">${node.type}</span>
        </div>`;
        
        if (node.properties) {
            Object.entries(node.properties).forEach(([key, value]) => {
                html += `<div class="property-item">
                    <span class="property-key">${key}:</span>
                    <span class="property-value">${value}</span>
                </div>`;
            });
        }
        
        // Show connections
        const outgoing = graphData.edges.filter(e => 
            e.source === node.id || (e.source.id && e.source.id === node.id)
        );
        const incoming = graphData.edges.filter(e => 
            e.target === node.id || (e.target.id && e.target.id === node.id)
        );
        
        html += `<div class="property-item">
            <span class="property-key">Outgoing:</span>
            <span class="property-value">${outgoing.length}</span>
        </div>`;
        html += `<div class="property-item">
            <span class="property-key">Incoming:</span>
            <span class="property-value">${incoming.length}</span>
        </div>`;
        
        nodeInfo.innerHTML = html;
        
        // Store selected node for neighborhood view
        selectedNode = node.id;
    }
    
    // Update graph statistics
    function updateGraphInfo() {
        if (graphData) {
            document.getElementById('nodeCount').textContent = graphData.nodes.length;
            document.getElementById('edgeCount').textContent = graphData.edges.length;
            document.getElementById('currentGraphType').textContent = currentGraphType;
        }
    }
    
    // Update legend
    function updateLegend() {
        const colorBy = document.getElementById('colorBy').value;
        const legendContent = document.getElementById('legendContent');
        legendContent.innerHTML = '';
        
        if (!colorScale) return;
        
        if (colorBy === 'type') {
            const types = [...new Set(graphData.nodes.map(n => n.type))];
            types.forEach(type => {
                const item = document.createElement('div');
                item.className = 'legend-item';
                item.innerHTML = `
                    <div class="legend-color" style="background: ${colorScale(type)}"></div>
                    <span>${type}</span>
                `;
                legendContent.appendChild(item);
            });
            
        } else if (colorBy === 'connectivity') {
            // Show gradient
            const item = document.createElement('div');
            item.innerHTML = `
                <div style="display: flex; align-items: center;">
                    <span style="margin-right: 10px;">Low</span>
                    <div style="width: 100px; height: 20px; background: linear-gradient(to right, ${colorScale(0)}, ${colorScale(20)});"></div>
                    <span style="margin-left: 10px;">High</span>
                </div>
            `;
            legendContent.appendChild(item);
            
        } else if (colorBy === 'custom') {
            const prop = document.getElementById('customProperty').value;
            if (prop && colorScale.domain) {
                const domain = colorScale.domain();
                if (domain.length <= 10) {
                    // Categorical
                    domain.forEach(value => {
                        const item = document.createElement('div');
                        item.className = 'legend-item';
                        item.innerHTML = `
                            <div class="legend-color" style="background: ${colorScale(value)}"></div>
                            <span>${value}</span>
                        `;
                        legendContent.appendChild(item);
                    });
                } else {
                    // Show gradient for numeric
                    const item = document.createElement('div');
                    item.innerHTML = `
                        <div style="display: flex; align-items: center;">
                            <span style="margin-right: 10px;">${domain[0]}</span>
                            <div style="width: 100px; height: 20px; background: linear-gradient(to right, ${colorScale(domain[0])}, ${colorScale(domain[1])});"></div>
                            <span style="margin-left: 10px;">${domain[1]}</span>
                        </div>
                    `;
                    legendContent.appendChild(item);
                }
            }
        }
    }
    
    // Drag functionality
    function drag(simulation) {
        function dragstarted(event, d) {
            if (!event.active && simulation) simulation.alphaTarget(0.3).restart();
            d.fx = d.x;
            d.fy = d.y;
        }
        
        function dragged(event, d) {
            d.fx = event.x;
            d.fy = event.y;
        }
        
        function dragended(event, d) {
            if (!event.active && simulation) simulation.alphaTarget(0);
            if (event.shiftKey) {
                // Keep fixed position if shift is held
                d.fx = d.x;
                d.fy = d.y;
            } else {
                // Release position
                d.fx = null;
                d.fy = null;
            }
        }
        
        return d3.drag()
            .on('start', dragstarted)
            .on('drag', dragged)
            .on('end', dragended);
    }
    
    // Node search functionality
    async function searchNodes(query) {
        if (!query || query.length < 2) {
            document.getElementById('searchResults').style.display = 'none';
            return;
        }
        
        try {
            const response = await fetch(`/api/search?q=${encodeURIComponent(query)}&limit=10`);
            if (!response.ok) throw new Error('Search failed');
            
            const results = await response.json();
            displaySearchResults(results);
            
        } catch (error) {
            console.error('Search error:', error);
        }
    }
    
    // Display search results
    function displaySearchResults(results) {
        const container = document.getElementById('searchResults');
        
        if (results.length === 0) {
            container.innerHTML = '<div class="search-result">No results found</div>';
            container.style.display = 'block';
            return;
        }
        
        container.innerHTML = results.map(result => `
            <div class="search-result" data-node-id="${result.id}">
                <div class="search-result-label">${result.display}</div>
                <div class="search-result-type">${result.labels.join(', ')}</div>
            </div>
        `).join('');
        
        container.style.display = 'block';
        
        // Add click handlers
        container.querySelectorAll('.search-result').forEach(el => {
            el.addEventListener('click', () => {
                const nodeId = el.dataset.nodeId;
                document.getElementById('nodeSearch').value = el.querySelector('.search-result-label').textContent;
                container.style.display = 'none';
                selectedNode = nodeId;
                
                // Switch to neighborhood view
                document.getElementById('graphType').value = 'neighborhood';
                loadGraphData();
            });
        });
    }
    
    // Setup event listeners
    function setupEventListeners() {
        // Graph type change
        document.getElementById('graphType').addEventListener('change', (e) => {
            // Show/hide relevant controls
            document.getElementById('focusedControls').style.display = 
                e.target.value === 'focused' ? 'block' : 'none';
            document.getElementById('neighborhoodControls').style.display = 
                e.target.value === 'neighborhood' ? 'block' : 'none';
            document.getElementById('filteredControls').style.display = 
                e.target.value === 'filtered' ? 'block' : 'none';
            
            loadGraphData();
        });
        
        // Focused view controls
        document.getElementById('nodeType').addEventListener('change', () => {
            if (document.getElementById('graphType').value === 'focused') {
                loadGraphData();
            }
        });
        
        document.getElementById('relationType').addEventListener('change', () => {
            if (document.getElementById('graphType').value === 'focused') {
                loadGraphData();
            }
        });
        
        // Node search
        let searchTimeout;
        document.getElementById('nodeSearch').addEventListener('input', (e) => {
            clearTimeout(searchTimeout);
            searchTimeout = setTimeout(() => {
                searchNodes(e.target.value);
            }, 300);
        });
        
        // Hide search results when clicking outside
        document.addEventListener('click', (e) => {
            if (!e.target.closest('#nodeSearch') && !e.target.closest('#searchResults')) {
                document.getElementById('searchResults').style.display = 'none';
            }
        });
        
        // Depth change for neighborhood view
        document.getElementById('depth').addEventListener('change', () => {
            if (document.getElementById('graphType').value === 'neighborhood' && selectedNode) {
                loadGraphData();
            }
        });
        
        // Color controls
        document.getElementById('colorBy').addEventListener('change', (e) => {
            document.getElementById('customPropertyControl').style.display = 
                e.target.value === 'custom' ? 'block' : 'none';
            
            if (e.target.value === 'custom') {
                updatePropertyDropdown('customProperty');
            }
            
            if (graphData) {
                renderGraph();
                updateLegend();
            }
        });
        
        document.getElementById('customProperty').addEventListener('change', () => {
            if (graphData) {
                renderGraph();
                updateLegend();
            }
        });
        
        // Size controls
        document.getElementById('sizeBy').addEventListener('change', (e) => {
            document.getElementById('sizePropertyControl').style.display = 
                e.target.value === 'property' ? 'block' : 'none';
            
            if (e.target.value === 'property') {
                updatePropertyDropdown('sizeProperty');
            }
            
            if (graphData) {
                renderGraph();
            }
        });
        
        document.getElementById('sizeProperty').addEventListener('change', () => {
            if (graphData) {
                renderGraph();
            }
        });
        
        // Layout change
        document.getElementById('layout').addEventListener('change', () => {
            if (graphData) {
                renderGraph();
            }
        });
        
        // Label toggles
        document.getElementById('showLabels').addEventListener('change', (e) => {
            d3.selectAll('.node text').style('display', e.target.checked ? 'block' : 'none');
        });
        
        document.getElementById('showEdgeLabels').addEventListener('change', (e) => {
            d3.selectAll('.link-label').style('display', e.target.checked ? 'block' : 'none');
        });
        
        // Buttons
        document.getElementById('resetView').addEventListener('click', () => {
            svg.transition()
                .duration(750)
                .call(zoom.transform, d3.zoomIdentity);
        });
        
        document.getElementById('exportData').addEventListener('click', () => {
            if (graphData) {
                const dataStr = JSON.stringify(graphData, null, 2);
                const dataBlob = new Blob([dataStr], {type: 'application/json'});
                const url = URL.createObjectURL(dataBlob);
                const link = document.createElement('a');
                link.href = url;
                link.download = `graph-${currentGraphType}-${new Date().toISOString()}.json`;
                link.click();
                URL.revokeObjectURL(url);
            }
        });
        
        document.getElementById('refreshData').addEventListener('click', () => {
            loadGraphData();
        });
        
        // Max nodes change
        document.getElementById('maxNodes').addEventListener('change', () => {
            const graphType = document.getElementById('graphType').value;
            if (graphType === 'overview' || graphType === 'filtered') {
                loadGraphData();
            }
        });
        
        // Filter controls
        document.addEventListener('change', (e) => {
            if ((e.target.name === 'nodeType' || e.target.name === 'relationType' || e.target.id === 'connectedOnly') 
                && document.getElementById('graphType').value === 'filtered') {
                loadGraphData();
            }
        });
    }
    
    // Update property dropdown based on current nodes
    function updatePropertyDropdown(selectId) {
        const select = document.getElementById(selectId);
        select.innerHTML = '<option value="">Select property...</option>';
        
        if (!graphData || !graphData.nodes) return;
        
        // Collect all unique properties
        const properties = new Set();
        graphData.nodes.forEach(node => {
            if (node.properties) {
                Object.keys(node.properties).forEach(prop => properties.add(prop));
            }
        });
        
        // Add to dropdown
        Array.from(properties).sort().forEach(prop => {
            const option = document.createElement('option');
            option.value = prop;
            option.textContent = prop;
            select.appendChild(option);
        });
    }
    
    // Show loading screen
    function showLoading() {
        document.getElementById('loading').style.display = 'block';
    }
    
    // Hide loading screen
    function hideLoading() {
        document.getElementById('loading').style.display = 'none';
    }
    
    // Show error message
    function showError(message) {
        const loading = document.getElementById('loading');
        loading.innerHTML = `<p style="color: red;">${message}</p>`;
        loading.style.display = 'block';
        
        setTimeout(() => {
            loading.style.display = 'none';
            loading.innerHTML = '<div class="spinner"></div><p>Loading graph data...</p>';
        }, 3000);
    }
    
    // Start the application
    document.addEventListener('DOMContentLoaded', init);
})();