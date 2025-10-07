# Agent-Zero Memory & Context Management Module

This module implements the Memory & Context Management component of the Agent-Zero system as specified in AGENT-ZERO-GENESIS.md Phase 7.

## Overview

The agentzero-memory module provides persistent memory capabilities with RocksDB backing store, integrating with OpenCog's AtomSpace and attention mechanisms for intelligent memory management.

## Components

### LongTermMemory (AZ-MEM-003) ✅ IMPLEMENTED

Persistent knowledge storage with the following features:

- **Persistent Storage**: Uses atomspace-rocks (RocksDB) for reliable persistence
- **Attention Integration**: Uses AttentionValue for importance-based retention
- **Memory Consolidation**: Automatic cleanup based on importance and access patterns  
- **Efficient Retrieval**: Context-based indexing and caching for fast access
- **Background Tasks**: Automatic consolidation and backup operations
- **Configuration**: Flexible configuration for different use cases

### EpisodicMemory (AZ-MEM-001) - Placeholder

Manages temporal sequences and experiences. Implementation planned for future task.

### WorkingMemory (AZ-MEM-002) - Placeholder  

Active context and short-term memory. Implementation planned for future task.

### ContextManager (AZ-CONTEXT-001) - Placeholder

Maintains relevant contextual information. Implementation planned for future task.

## Dependencies

- **cogutil**: Core OpenCog utilities
- **atomspace**: AtomSpace knowledge representation
- **atomspace-rocks**: RocksDB persistence backend  
- **attention**: Importance-based memory management

## Usage

### Basic Usage

```cpp
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/agentzero/memory/LongTermMemory.h>

// Create AtomSpace
auto atomspace = std::make_shared<AtomSpace>();

// Configure memory
MemoryConfig config;
config.persistence_directory = "./my_memory_data";
config.max_memory_count = 100000;

// Create and initialize LongTermMemory
auto ltm = std::make_unique<LongTermMemory>(atomspace, config);
ltm->initialize();

// Store memories
Handle concept = atomspace->add_node(CONCEPT_NODE, "ImportantConcept");
ltm->store(concept, MemoryImportance::HIGH, PersistenceLevel::LONG_TERM);

// Retrieve memories
Handle retrieved = ltm->retrieve(concept);

// Find by importance
auto important_memories = ltm->findByImportance(MemoryImportance::HIGH);

// Shutdown gracefully
ltm->shutdown();
```

### Memory Importance Levels

- **CRITICAL**: Must persist (system knowledge, learned skills)
- **HIGH**: Important experiences, successful patterns
- **MEDIUM**: Regular experiences, moderate success/failure
- **LOW**: Minor experiences, routine operations  
- **MINIMAL**: Temporary data, low-value information

### Persistence Levels

- **PERMANENT**: Never removed, always persisted
- **LONG_TERM**: Persisted to disk, subject to consolidation
- **MEDIUM_TERM**: In memory, persisted periodically
- **SHORT_TERM**: In memory only, cleared on restart
- **TEMPORARY**: Cleared automatically after timeout

### Context Types

Memories can be associated with different context types for efficient retrieval:

- **TEMPORAL**: Time-based context
- **SPATIAL**: Location-based context
- **TASK**: Task or goal context
- **SOCIAL**: Social interaction context
- **EMOTIONAL**: Emotional state context
- **ENVIRONMENTAL**: Environmental conditions
- **COGNITIVE**: Cognitive state and processes

## Building

The module is integrated with the OpenCog unified build system:

```bash
# Configure build
mkdir -p /tmp/opencog-build && cd /tmp/opencog-build
cmake /path/to/repository

# Build the memory module
make agentzero-memory

# Run tests (if CxxTest is available)
make test

# Build examples  
make ltm_basic_example
make ltm_persistence_example
```

## Examples

### Basic Example

Run the basic example to see core functionality:

```bash
./examples/ltm_basic_example
```

This demonstrates:
- Memory storage and retrieval
- Importance-based filtering
- Context-based organization
- System statistics and status

### Persistence Example

Run the persistence example to see cross-session memory:

```bash
./examples/ltm_persistence_example  
```

This demonstrates:
- Memory persistence across restarts
- Recovery of stored memories
- Backup and restore operations
- Importance-based retention

## Configuration

Key configuration parameters:

```cpp
MemoryConfig config;

// Retention parameters
config.min_retention_importance = MemoryImportance::LOW;
config.max_retention_period = std::chrono::hours(24 * 30);  // 30 days
config.max_memory_count = 100000;

// Consolidation parameters  
config.consolidation_strategy = ConsolidationStrategy::HYBRID;
config.consolidation_interval = std::chrono::hours(6);

// Persistence parameters
config.persistence_directory = "./memory_data";
config.enable_compression = true;
config.enable_incremental_backup = true;
config.backup_interval = std::chrono::hours(24);
```

## Integration with OpenCog

The LongTermMemory integrates seamlessly with OpenCog components:

- **AtomSpace**: All memories are stored as Atoms
- **AttentionBank**: Uses AttentionValue for importance calculation
- **RocksStorage**: Leverages existing RocksDB persistence infrastructure
- **CogUtil**: Uses OpenCog logging and utility functions

## Performance Targets

Based on AGENT-ZERO-GENESIS.md specifications:

- **Response Time**: < 100ms for routine memory operations
- **Memory Efficiency**: Linear scaling with knowledge base size
- **Scalability**: Support for 10M+ Atoms in knowledge base
- **Integration Overhead**: < 10% performance penalty vs. standalone systems

## Architecture Compliance

This implementation follows OpenCog architectural patterns:

- ✅ Uses AtomSpace for knowledge representation
- ✅ Integrates with attention mechanisms (ECAN)
- ✅ Uses standard OpenCog build system (CMake)
- ✅ Follows OpenCog coding standards and conventions
- ✅ Comprehensive error handling and logging
- ✅ Thread-safe operations with proper synchronization

## Future Development

Planned enhancements:

1. **EpisodicMemory Implementation** (AZ-MEM-001)
2. **WorkingMemory Implementation** (AZ-MEM-002)  
3. **ContextManager Implementation** (AZ-CONTEXT-001)
4. **PLN Integration**: Use PLN reasoning for memory consolidation
5. **Distributed Memory**: Support for distributed memory across multiple nodes
6. **Advanced Indexing**: More sophisticated indexing for faster retrieval

## Status

- ✅ **AZ-MEM-003**: LongTermMemory with persistence - COMPLETED
- ⏳ **AZ-MEM-001**: EpisodicMemory - Placeholder created
- ⏳ **AZ-MEM-002**: WorkingMemory - Placeholder created  
- ⏳ **AZ-CONTEXT-001**: ContextManager - Placeholder created

## Testing

Unit tests cover:
- Basic storage and retrieval operations
- Importance-based memory management
- Context-based organization
- Persistence across sessions
- Configuration management
- Statistics collection
- Error handling

Run tests with:
```bash
ctest -R LongTermMemoryUTest
```