# Agent-Zero Tools Module

## Overview

The Agent-Zero Tools module provides a unified interface for integrating external tools with the Agent-Zero cognitive architecture and OpenCog AtomSpace. This module implements **AZ-TOOL-002: ToolWrapper unified interface** from Phase 8 of the AGENT-ZERO-GENESIS project.

## Features

- **Unified Interface**: Single interface for multiple tool types (REST API, ROS, Python scripts, shell commands, AtomSpace queries, custom tools)
- **AtomSpace Integration**: Full integration with OpenCog's knowledge representation system
- **Execution Modes**: Support for both synchronous and asynchronous execution
- **Error Handling**: Comprehensive error handling with detailed error messages and status tracking
- **Performance Tracking**: Built-in execution statistics and performance monitoring
- **Resource Management**: Timeout management and resource constraint enforcement
- **Extensibility**: Easy to extend with custom tool types and executors

## Architecture

### Core Components

#### 1. ToolWrapper
Main class providing the unified interface for tool integration.

**Key Features:**
- Tool identification and configuration
- Multiple tool type support
- AtomSpace representation
- Execution statistics tracking
- Custom executor support

#### 2. ToolExecutionContext
Encapsulates the execution context for a tool.

**Provides:**
- Input parameters
- Configuration settings
- AtomSpace access
- Input atoms for processing
- Timeout and execution mode settings

#### 3. ToolResult
Encapsulates the result of tool execution.

**Contains:**
- Execution status
- Output data (string, atoms, structured data)
- Error information
- Execution metadata
- Performance metrics

### Supported Tool Types

1. **EXTERNAL_REST_API**: External tools accessible via REST API
2. **ROS_BEHAVIOR**: ROS behavior scripting and robot control
3. **PYTHON_SCRIPT**: Python script execution
4. **SHELL_COMMAND**: Shell command execution
5. **ATOMSPACE_QUERY**: Direct AtomSpace queries
6. **CUSTOM**: Custom tool implementations with user-defined executors

## Usage Examples

### Example 1: Custom Tool with Lambda Executor

```cpp
#include <opencog/agentzero/tools/ToolWrapper.h>

// Create AtomSpace
AtomSpacePtr atomspace = createAtomSpace();

// Create custom tool
auto tool = std::make_shared<ToolWrapper>("sentiment_analyzer", ToolType::CUSTOM, atomspace);
tool->setDescription("Analyzes sentiment of text");

// Set custom executor
tool->setCustomExecutor([](const ToolExecutionContext& context) {
    ToolResult result(ToolStatus::COMPLETED);
    std::string text = context.getParameter("text");
    
    // Process text...
    result.setOutput("Sentiment: positive");
    result.setMetadata("score", "0.8");
    
    return result;
});

// Execute tool
ToolExecutionContext context(atomspace);
context.setParameter("text", "This is great!");
ToolResult result = tool->execute(context);
```

### Example 2: REST API Tool

```cpp
// Create REST API tool
auto tool = std::make_shared<ToolWrapper>("face_detector", ToolType::EXTERNAL_REST_API, atomspace);
tool->setToolEndpoint("http://localhost:5000/api/detect_faces");
tool->setToolConfig("api_key", "my_key");

// Add required parameters
tool->addRequiredParameter("image_url");

// Execute
ToolExecutionContext context(atomspace);
context.setParameter("image_url", "http://example.com/image.jpg");
context.setTimeout(10000.0);
ToolResult result = tool->execute(context);
```

### Example 3: ROS Behavior Tool

```cpp
// Create ROS behavior tool
auto tool = std::make_shared<ToolWrapper>("robot_movement", ToolType::ROS_BEHAVIOR, atomspace);
tool->setToolEndpoint("/robot/move_to");

// Execute movement command
ToolExecutionContext context(atomspace);
context.setParameter("x", "1.5");
context.setParameter("y", "2.0");
ToolResult result = tool->execute(context);
```

### Example 4: AtomSpace Query Tool

```cpp
// Create query tool
auto tool = std::make_shared<ToolWrapper>("location_query", ToolType::ATOMSPACE_QUERY, atomspace);

// Add input atoms for query
ToolExecutionContext context(atomspace);
context.addInputAtom(person_atom);
context.addInputAtom(location_atom);

// Execute query
ToolResult result = tool->execute(context);

// Access AtomSpace results
const HandleSeq& results = result.getAtomSpaceResults();
```

## Integration with OpenCog

### AtomSpace Representation

Each ToolWrapper instance creates its representation in the AtomSpace:

```scheme
(ConceptNode "Tool_face_detector")
(InheritanceLink
    (ConceptNode "Tool_face_detector")
    (ConceptNode "ToolType"))
```

### Execution Tracking

Tool executions are recorded in the AtomSpace:

```scheme
(ConceptNode "Execution_face_detector_1")
(EvaluationLink
    (ConceptNode "Tool_face_detector")
    (ConceptNode "Execution_face_detector_1"))
```

## Dependencies

### Required
- **cogutil**: OpenCog utilities library
- **atomspace**: OpenCog AtomSpace library
- **Boost**: C++ Boost libraries (system, filesystem, thread)

### Optional (for specific tool types)
- **libcurl**: For REST API tools
- **ROS**: For ROS behavior tools
- **Python**: For Python script execution

## Building

### CMake Configuration

```bash
mkdir build && cd build
cmake -DBUILD_TESTING=ON -DBUILD_EXAMPLES=ON ..
make
```

### Build Options

- `BUILD_TESTING`: Build unit tests (default: ON)
- `BUILD_EXAMPLES`: Build example applications (default: ON)

### Running Tests

```bash
cd build
make test
# or
ctest --verbose
```

### Running Examples

```bash
./bin/examples/ToolWrapperDemo
```

## Performance

### Execution Statistics

ToolWrapper automatically tracks:
- Total execution count
- Success/failure counts
- Success rate
- Average execution time
- Total execution time

Access statistics:

```cpp
std::cout << "Executions: " << tool->getExecutionCount() << std::endl;
std::cout << "Success Rate: " << tool->getSuccessRate() << std::endl;
std::cout << "Avg Time: " << tool->getAverageExecutionTime() << "ms" << std::endl;
std::cout << "Statistics JSON: " << tool->getStatistics() << std::endl;
```

### Resource Management

- **Timeout Control**: Set maximum execution time
- **Async Execution**: Non-blocking tool execution (coming soon)
- **Memory Efficiency**: Minimal overhead per tool instance

## Error Handling

### Status Codes

- `NOT_STARTED`: Tool has not been executed
- `RUNNING`: Tool is currently executing
- `COMPLETED`: Execution completed successfully
- `FAILED`: Execution failed
- `TIMEOUT`: Execution exceeded timeout limit
- `CANCELLED`: Execution was cancelled

### Error Information

```cpp
ToolResult result = tool->execute(context);
if (!result.isSuccess()) {
    std::cerr << "Error: " << result.getErrorMessage() << std::endl;
    std::cerr << "Status: " << result.toString() << std::endl;
}
```

## Extensibility

### Creating Custom Tool Types

1. Extend ToolType enum (if needed)
2. Implement executor function
3. Register with ToolWrapper

```cpp
// Create custom tool
auto tool = std::make_shared<ToolWrapper>("my_tool", ToolType::CUSTOM);

// Set custom executor
tool->setCustomExecutor([](const ToolExecutionContext& context) {
    // Your implementation here
    ToolResult result(ToolStatus::COMPLETED);
    result.setOutput("Custom processing complete");
    return result;
});
```

## Integration with external-tools and ros-behavior-scripting

### External Tools Integration

The ToolWrapper provides a unified interface for tools in the `external-tools` repository:

- AtomSpace visualization tools
- Performance monitoring tools
- Diagnostic tools
- REST API-based tools

### ROS Behavior Scripting Integration

Integration with `ros-behavior-scripting` enables:

- Sensory input processing (vision, audio)
- Motor control and movement
- Behavior coordination
- ROS topic/service communication

## Testing

### Unit Tests

Comprehensive unit tests cover:
- ToolResult functionality
- ToolExecutionContext functionality
- ToolWrapper core features
- AtomSpace integration
- Error handling
- Statistics tracking

Run tests:
```bash
cd build
make test
```

### Integration Tests

Integration tests verify:
- OpenCog component compatibility
- AtomSpace operations
- Multi-tool coordination
- Performance benchmarks

## Documentation

### API Documentation

Generate API documentation with Doxygen:

```bash
cd build
cmake -DBUILD_DOCS=ON ..
make docs
```

### Code Examples

See `examples/` directory for complete working examples:
- `ToolWrapperDemo.cpp`: Comprehensive demonstration of all features

## Future Enhancements

### Planned Features (Phase 8 continuation)

- **AZ-TOOL-001**: ToolRegistry catalog for managing multiple tools
- **AZ-TOOL-003**: CapabilityComposer for combining tools
- **AZ-RESOURCE-001**: ResourceManager for optimization

### Implementation Roadmap

1. **Async Execution**: Full asynchronous execution support with callbacks
2. **Tool Discovery**: Automatic tool discovery and registration
3. **Capability Composition**: Combine multiple tools for complex tasks
4. **Performance Optimization**: Advanced caching and resource management
5. **Security**: Enhanced security for shell command and script execution

## Contributing

When contributing to the Agent-Zero Tools module:

1. Follow OpenCog coding standards
2. Add comprehensive tests for new features
3. Update documentation
4. Ensure AtomSpace integration
5. Maintain backward compatibility

## License

Copyright (C) 2024 OpenCog Foundation
SPDX-License-Identifier: AGPL-3.0-or-later

## Support

For questions or issues:
- OpenCog GitHub: https://github.com/opencog
- OpenCog Wiki: https://wiki.opencog.org
- Agent-Zero Genesis: See AGENT-ZERO-GENESIS.md

## References

- [AGENT-ZERO-GENESIS.md](../../AGENT-ZERO-GENESIS.md): Master blueprint
- [OpenCog AtomSpace](https://github.com/opencog/atomspace): Knowledge representation
- [external-tools](https://github.com/opencog/external-tools): External tool repository
- [ros-behavior-scripting](https://github.com/opencog/ros-behavior-scripting): ROS integration
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
