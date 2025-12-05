# Agent-Zero Tools Module

## Overview

The Agent-Zero Tools module provides the **CapabilityComposer** component, which enables intelligent composition and coordination of multiple tools and capabilities for complex task execution.

## Task ID: AZ-TOOL-003

**Phase**: 8 - Tool Integration  
**Status**: Implemented  
**Dependencies**: external-tools, ros-behavior-scripting

## CapabilityComposer

### Purpose

CapabilityComposer combines multiple tools and capabilities to accomplish complex tasks that require coordinated execution of multiple simpler capabilities. It provides:

- **Capability Registration**: Register tools and capabilities with their dependencies
- **Automatic Composition**: Automatically plan execution sequences based on task requirements
- **Dependency Resolution**: Resolve and order capabilities based on their dependencies
- **AtomSpace Integration**: Represent capabilities and plans in the AtomSpace
- **Execution Coordination**: Execute composed plans with proper ordering
- **Statistics Tracking**: Track execution success rates and performance metrics

### Key Features

1. **Intelligent Composition**
   - Automatic task decomposition
   - Dependency graph analysis
   - Topological sorting for execution order
   - Parallel execution opportunities (planned)

2. **Capability Management**
   - Dynamic capability registration/unregistration
   - Dependency validation
   - Provider lookup (find capabilities that provide specific outputs)
   - Execution statistics tracking

3. **Plan Management**
   - Automatic plan generation from task requirements
   - Plan validation
   - Plan caching and reuse
   - Success probability estimation

4. **OpenCog Integration**
   - AtomSpace representation of capabilities
   - AtomSpace representation of composition plans
   - Execution history tracking in AtomSpace
   - Integration with other OpenCog cognitive components

### Architecture

```
CapabilityComposer
├── Capability Registry
│   ├── Capability definitions
│   ├── Dependency graph
│   └── Provider index
├── Composition Planner
│   ├── Task requirements analysis
│   ├── Dependency resolution
│   ├── Plan generation
│   └── Plan validation
├── Execution Engine
│   ├── Capability execution
│   ├── Context management
│   └── Statistics tracking
└── AtomSpace Integration
    ├── Capability atoms
    ├── Plan atoms
    └── Execution history
```

### Usage Example

```cpp
#include <opencog/agentzero/tools/CapabilityComposer.h>

// Initialize
auto atomspace = std::make_shared<AtomSpace>();
auto composer = std::make_unique<CapabilityComposer>(atomspace);

// Register capabilities
composer->registerCapability(
    "sensor_read",
    "Sensor Reading",
    "Read data from sensors",
    sensorReadFunction,
    {}  // No dependencies
);

composer->registerCapability(
    "path_planning",
    "Path Planning",
    "Plan movement path",
    pathPlanningFunction,
    {"sensor_read"}  // Depends on sensor_read
);

// Compose and execute a task
CapabilityComposer::TaskRequirements requirements;
requirements.task_description = "Navigate to target";
requirements.required_outputs = {"path_planning"};

CapabilityComposer::ExecutionContext context;
context.atomspace = atomspace;

auto result = composer->composeAndExecute(requirements, context);
```

## Building

The module is built as part of the Agent-Zero C++ framework:

```bash
cd /home/runner/work/pycog0/pycog0
mkdir -p build && cd build
cmake ..
make agentzero-tools
```

## Testing

Run the test suite:

```bash
cd build
make test
# Or run specific test:
./agents/cpp/agentzero-tools/tests/CapabilityComposerSimpleTest
```

## Examples

Run the demonstration:

```bash
cd build
./agents/cpp/agentzero-tools/examples/CapabilityComposerDemo
```

This demonstrates a complex robotic manipulation task with automatic capability composition.

## API Reference

### Main Classes

#### CapabilityComposer

Main class for capability composition and execution.

**Key Methods:**
- `registerCapability()` - Register a new capability
- `composeForTask()` - Generate composition plan for task
- `executePlan()` - Execute a composition plan
- `composeAndExecute()` - Compose and execute in one step
- `getCapabilityStatistics()` - Get execution statistics

#### Capability

Structure representing a single capability or tool.

**Fields:**
- `capability_id` - Unique identifier
- `name` - Human-readable name
- `description` - What the capability does
- `required_capabilities` - Dependencies
- `provided_capabilities` - Outputs this provides
- `execute` - Execution function

#### CompositionPlan

Structure representing an execution plan.

**Fields:**
- `plan_id` - Unique identifier
- `capability_sequence` - Ordered execution sequence
- `dependency_graph` - Capability dependencies
- `estimated_success_probability` - Success estimate
- `is_valid` - Whether plan is valid

#### ExecutionContext

Context for capability execution.

**Fields:**
- `atomspace` - Shared AtomSpace
- `input_parameters` - Input data
- `output_results` - Output data
- `execution_log` - Execution history

## Integration with OpenCog Components

### external-tools
The CapabilityComposer can integrate with external tools by wrapping them as capabilities. External visualization, monitoring, and diagnostic tools can be registered as capabilities and composed into complex workflows.

### ros-behavior-scripting
ROS behavior scripts can be wrapped as capabilities, enabling composition of complex robotic behaviors. The dependency resolution ensures proper sequencing of sensor inputs, planning, and motor outputs.

### AtomSpace
All capabilities and plans are represented in the AtomSpace, enabling:
- Querying capabilities with the pattern matcher
- Learning from execution history
- Integration with PLN reasoning
- Persistence across sessions

## Performance Characteristics

- **Capability Registration**: O(1)
- **Dependency Resolution**: O(V + E) where V = capabilities, E = dependencies
- **Plan Composition**: O(V + E) + O(V log V) for topological sort
- **Plan Execution**: O(n) where n = number of capabilities in plan

## Configuration Options

```cpp
composer->setMaxCachedPlans(100);              // Maximum cached plans
composer->setCompositionTimeout(30.0);         // Composition timeout (seconds)
composer->enableAutomaticComposition(true);    // Auto-compose when needed
composer->enableParallelExecution(false);      // Parallel execution (future)
composer->setMaxCompositionDepth(10);          // Max dependency depth
```

## Future Enhancements

- [ ] Parallel execution of independent capabilities
- [ ] Cost-based composition optimization
- [ ] Learning from execution history
- [ ] Integration with MOSES for capability optimization
- [ ] Real-time capability discovery
- [ ] Distributed capability execution
- [ ] Capability versioning and updates

## References

- AGENT-ZERO-GENESIS.md - Overall project architecture
- AZ-TOOL-001: ToolRegistry (companion task)
- AZ-TOOL-002: ToolWrapper (companion task)
- AZ-RESOURCE-001: ResourceManager (companion task)

## License

AGPL-3.0-or-later

## Authors

OpenCog Foundation (2024)
