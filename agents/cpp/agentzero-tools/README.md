# Agent-Zero Tools Module

This module implements the Tool Integration Framework for Agent-Zero integrated with OpenCog.

## Overview

The Agent-Zero Tools module provides a comprehensive catalog system for managing external tools and capabilities that Agent-Zero can utilize. It serves as the integration point for external-tools and ros-behavior-scripting components.

## Key Components

- **ToolRegistry**: Catalog of available tools and capabilities
  - Tool registration and discovery
  - Capability-based tool matching
  - Tool composition for complex tasks
  - Dynamic tool availability tracking
  - AtomSpace integration for tool metadata

## Features

### Tool Management
- Register and unregister tools dynamically
- Track tool status (available, busy, unavailable, error)
- Monitor tool reliability and usage statistics
- Support for tool categories (visualization, analysis, robotics, etc.)

### Capability Matching
- Query tools by capabilities (read-only, async execution, batch processing, etc.)
- Search tools by keywords
- Filter tools by category
- Validate tool dependencies

### Tool Composition
- Compose multiple tools into execution chains
- Validate tool compatibility
- Execute sequential tool pipelines
- Support for complex task decomposition

### Integration Points
- **external-tools**: Visualization, import/export, and utility tools
- **ros-behavior-scripting**: Robotics perception and motor control tools
- **AtomSpace**: Tool metadata representation and reasoning

## Dependencies

- cogutil
- atomspace
- external-tools (optional, for tool discovery)
- ros-behavior-scripting (optional, for robotics tools)

## Build Instructions

This module is built as part of the overall Agent-Zero system. To build separately:

```bash
mkdir build && cd build
cmake ..
make
make test  # Run tests (requires GTest)
make install
```

## Usage Example

```cpp
#include <opencog/agentzero/ToolRegistry.h>

// Create tool registry
AtomSpacePtr as = createAtomSpace();
ToolRegistry registry(as);

// Register a custom tool
ToolRegistry::ToolMetadata metadata;
metadata.name = "MyCustomTool";
metadata.description = "A custom analysis tool";
metadata.category = ToolRegistry::ToolCategory::ANALYSIS;
metadata.capabilities = {
    ToolRegistry::ToolCapability::READ_WRITE,
    ToolRegistry::ToolCapability::BATCH_PROCESSING
};

auto executor = [](const HandleSeq& args, AtomSpacePtr as) -> Handle {
    // Tool implementation
    return as->add_node(CONCEPT_NODE, "ToolResult");
};

Handle tool_atom = registry.registerTool(metadata, executor);

// Discover tools
auto all_tools = registry.getAllTools();
auto viz_tools = registry.getToolsByCategory(ToolRegistry::ToolCategory::VISUALIZATION);

// Search and execute
auto results = registry.searchTools("analyze");
if (!results.empty()) {
    HandleSeq args;
    Handle result = registry.executeTool(results[0], args);
}

// Tool composition
std::vector<std::string> chain = {"Tool1", "Tool2", "Tool3"};
HandleSeq initial_input;
Handle final_result = registry.executeToolChain(chain, initial_input);

// Get statistics
auto stats = registry.getToolStatistics();
for (const auto& [name, stat] : stats) {
    std::cout << name << ": " << stat.first << " uses, "
              << stat.second << " reliability\n";
}
```

## Tool Categories

- **VISUALIZATION**: AtomSpace visualization and display tools
- **ANALYSIS**: Data analysis and statistical tools
- **IMPORT_EXPORT**: Data import/export utilities
- **ROBOTICS**: Robotics integration and control
- **PERCEPTION**: Sensory input processing
- **MOTOR_CONTROL**: Motor and movement control
- **COMMUNICATION**: Communication interfaces
- **UTILITY**: General utility tools
- **CUSTOM**: User-defined tool types

## Tool Capabilities

- **READ_ONLY**: Tool only reads data
- **READ_WRITE**: Tool can modify data
- **ASYNC_EXECUTION**: Supports asynchronous execution
- **BATCH_PROCESSING**: Can process multiple items
- **REAL_TIME**: Operates in real-time
- **REQUIRES_ROS**: Requires ROS environment
- **NETWORK_ACCESS**: Requires network connectivity

## Integration with OpenCog

### AtomSpace Representation
- Tools represented as CONCEPT_NODE atoms
- Tool categories linked via MEMBER_LINK
- Tool reliability stored in TruthValues
- Tool relationships expressed through Links

### Tool Discovery
- Automatic discovery of external-tools components
- ROS tool detection when ROS is available
- Dynamic tool registration at runtime
- Dependency checking for tool availability

## Testing

Comprehensive unit tests are provided using Google Test:

```bash
make test
# Or run specific tests
./tests/ToolRegistryTest
```

Tests cover:
- Tool registration and unregistration
- Tool discovery and search
- Capability matching
- Tool execution and composition
- Reliability tracking
- Statistics management
- AtomSpace integration

## Development Status

✅ **Implemented** - ToolRegistry catalog system is complete and functional.

See [AGENT-ZERO-GENESIS.md](../../../AGENT-ZERO-GENESIS.md) for the complete development roadmap.

## Performance

- Tool lookup: O(1) for registered tools
- Category search: O(n) where n is tools in category
- Capability matching: O(n*m) where n is tools, m is capabilities
- Tool execution: Depends on tool implementation
- Reliability updates: O(1) with exponential moving average

## Future Enhancements

- PLN-based tool composition reasoning
- Machine learning for tool selection optimization
- Distributed tool execution
- Tool versioning and compatibility management
- Advanced dependency resolution
- Tool marketplace integration

## Related Components

- **agentzero-core**: Main agent orchestration
- **external-tools**: Visualization and utility tools
- **ros-behavior-scripting**: ROS integration tools
- **cogserver**: Server for tool management interface

## License

AGPL-3.0-or-later - See LICENSE file for details

## Contributors

OpenCog Foundation - Agent-Zero-Genesis Project
