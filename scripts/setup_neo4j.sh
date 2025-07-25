#!/bin/bash
# Setup script for Neo4j database

echo "=== Neo4j Setup for LAPACK Graph Database ==="
echo

# Check if docker is available
if command -v docker &> /dev/null; then
    echo "Docker found. Setting up Neo4j container..."
    
    # Run Neo4j in Docker
    docker run \
        --name lapack-neo4j \
        -p 7474:7474 -p 7687:7687 \
        -e NEO4J_AUTH=neo4j/lapack123 \
        -e NEO4J_PLUGINS='["apoc"]' \
        -v $PWD/neo4j/data:/data \
        -v $PWD/neo4j/logs:/logs \
        -v $PWD/neo4j/import:/var/lib/neo4j/import \
        -d neo4j:5-community
    
    echo "Neo4j container started!"
    echo "  Web interface: http://localhost:7474"
    echo "  Bolt URL: bolt://localhost:7687"
    echo "  Username: neo4j"
    echo "  Password: lapack123"
    echo
    echo "Wait about 30 seconds for Neo4j to fully start..."
    
else
    echo "Docker not found. Please install Neo4j manually:"
    echo "  1. Download from https://neo4j.com/download/"
    echo "  2. Or use your package manager (apt, brew, etc.)"
    echo "  3. Start Neo4j and note the connection details"
fi

echo
echo "Once Neo4j is running, you can load the LAPACK graph with:"
echo "  uvx lapack-to-neo4j . --neo4j-uri bolt://localhost:7687 --neo4j-password lapack123"