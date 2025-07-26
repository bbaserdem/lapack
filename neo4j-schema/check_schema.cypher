// Simple Schema Check for Neo4j 5.x

// Check Precision Nodes
MATCH (p:Precision)
RETURN 'Precision Nodes' as check, collect({symbol: p.symbol, name: p.name}) as result
ORDER BY p.symbol;

// Check if any Routine nodes exist
MATCH (r:Routine)
RETURN 'Routine Count' as check, count(r) as result;

// Check if any File nodes exist
MATCH (f:File)
RETURN 'File Count' as check, count(f) as result;

// Check if any Operation nodes exist
MATCH (o:Operation)
RETURN 'Operation Count' as check, count(o) as result;