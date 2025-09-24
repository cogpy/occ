# Agent-Zero Learning Module - ExperienceManager

This module implements the ExperienceManager component as part of Phase 5 (Learning & Adaptation) of the AGENT-ZERO-GENESIS project.

## Overview

The ExperienceManager provides comprehensive experience management capabilities for Agent-Zero, integrating experiential learning with OpenCog's AtomSpace. It handles experience acquisition, storage, retrieval, analysis, and learning from past experiences.

## Key Features

### Experience Management
- **8 Experience Types**: ACTION_OUTCOME, INTERACTION, PROBLEM_SOLVING, SKILL_APPLICATION, GOAL_PURSUIT, UNEXPECTED, LEARNING_EPISODE, EMOTIONAL
- **6 Outcome Classifications**: SUCCESS, FAILURE, PARTIAL_SUCCESS, UNEXPECTED_OUTCOME, INCONCLUSIVE, LEARNING_OPPORTUNITY
- **5 Importance Levels**: CRITICAL (100), HIGH (75), MEDIUM (50), LOW (25), ROUTINE (10)

### AtomSpace Integration
- All experiences stored as AtomSpace atoms with proper truth values
- Hierarchical organization with inheritance links
- Temporal indexing using timestamps
- Context-aware relationships between experiences

### Pattern Discovery
- **Sequential Patterns**: Discovers action sequences that lead to consistent outcomes
- **Causal Patterns**: Identifies cause-effect relationships in experiences
- **Success/Failure Analysis**: Tracks pattern success rates for learning
- **Pattern Significance**: Filters patterns based on configurable thresholds

### MOSES Integration
- Policy optimization based on experience data
- Experience-to-policy mapping for reinforcement learning
- Policy variant generation and testing
- Integration with AtomSpace evolution (ASMOSES)

### Memory Management
- **Experience Consolidation**: Removes redundant and low-value experiences
- **Temporal Organization**: Time-based experience indexing and retrieval
- **Context Similarity**: Experience matching based on environmental and agent state
- **Retention Policies**: Configurable thresholds for experience pruning

## Architecture

```
ExperienceManager
├── Experience Storage (AtomSpace)
│   ├── Experience Atoms
│   ├── Pattern Library
│   └── Temporal Index
├── Pattern Discovery
│   ├── Sequential Patterns
│   ├── Causal Patterns
│   └── Success Analysis
├── MOSES Integration
│   ├── Policy Creation
│   ├── Policy Optimization
│   └── Experience Mapping
└── Memory Management
    ├── Consolidation
    ├── Pruning
    └── Statistics
```

## Usage Example

```cpp
#include <opencog/agentzero/learning/ExperienceManager.h>

// Create AtomSpace and Agent Core
AtomSpacePtr atomspace = std::make_shared<AtomSpace>();
AgentZeroCore* agent_core = /* initialize agent core */;

// Create ExperienceManager
ExperienceManager exp_manager(agent_core, atomspace);

// Record a successful experience
ExperienceManager::ExperienceContext context;
context.timestamp = std::chrono::system_clock::now();
context.confidence_level = 0.8;

// Add environmental state atoms
Handle env_state = atomspace->add_node(CONCEPT_NODE, "CleanEnvironment");
context.environmental_state.push_back(env_state);

// Record experience with detailed context
Handle exp_atom = exp_manager.recordExperience(
    "Successfully completed navigation task",
    ExperienceManager::ExperienceType::PROBLEM_SOLVING,
    ExperienceManager::ExperienceOutcome::SUCCESS,
    context,
    {action1, action2},  // actions taken
    {consequence1}       // observed consequences
);

// Discover patterns from accumulated experiences
size_t patterns_found = exp_manager.discoverExperiencePatterns();

// Retrieve similar experiences for current context
std::vector<Handle> current_context = {env_state};
std::vector<Handle> similar_exp = exp_manager.getSimilarExperiences(
    current_context, 
    ExperienceManager::ExperienceType::PROBLEM_SOLVING, 
    5
);

// Get successful patterns for decision making
std::vector<Handle> successful_patterns = exp_manager.getSuccessfulPatterns(
    current_context, 
    0.7  // minimum 70% success rate
);

// Process experience management (call periodically)
bool success = exp_manager.processExperienceManagement();
```

## Configuration

The ExperienceManager supports various configuration options:

```cpp
// Enable/disable features
exp_manager.enablePatternDiscovery(true);
exp_manager.enableMOSESIntegration(true);
exp_manager.enableTemporalModeling(true);

// Set thresholds
exp_manager.setExperienceRetentionThreshold(0.3);
exp_manager.setMaxRecentExperiences(100);
exp_manager.setPatternSignificanceThreshold(0.6);

// Get configuration status
std::string status = exp_manager.getConfigurationStatus();
std::cout << status << std::endl;
```

## Dependencies

### Required
- **cogutil**: OpenCog utility library
- **atomspace**: OpenCog AtomSpace for knowledge representation
- **Boost**: C++ libraries for various utilities

### Optional (for enhanced functionality)
- **moses**: Policy optimization and genetic programming
- **asmoses**: AtomSpace evolution and program synthesis
- **learn**: Additional learning algorithms
- **cogserver**: Server integration and monitoring

## Building

The learning module is automatically included in the main Agent-Zero build when dependencies are available:

```bash
cd /path/to/pycog0/agents/cpp
mkdir build && cd build
cmake ..
make agentzero-learning
```

Or using the unified OpenCog build system:

```bash
cd /path/to/pycog0
mkdir /tmp/opencog-build && cd /tmp/opencog-build
cmake /path/to/pycog0
make Foundation-layer  # Build cogutil first
make atomspace         # Build atomspace
# Then build agent-zero components
```

## Testing

Run the comprehensive test suite:

```bash
cd agents/cpp/agentzero-learning/build
make test
```

Or run the simple test directly:

```bash
./tests/experience_manager_simple_test
```

## Integration with Agent-Zero Core

The ExperienceManager integrates seamlessly with other Agent-Zero components:

- **TaskManager**: Records task execution experiences
- **KnowledgeIntegrator**: Shares experience-based knowledge
- **ReasoningEngine**: Uses experience patterns for inference
- **MetaPlanner**: Optimizes planning based on past experiences

## Performance Considerations

- **Memory Usage**: Scales linearly with experience count
- **Pattern Discovery**: O(n²) complexity for similarity calculations
- **Retrieval**: Optimized with temporal and type-based indexing
- **Consolidation**: Periodic cleanup prevents memory bloat

## Future Enhancements

- **Distributed Experience Sharing**: Multi-agent experience exchange
- **Hierarchical Pattern Mining**: Multi-level pattern abstraction
- **Emotional Experience Processing**: Integration with affective systems
- **Real-time Learning**: Online experience processing and adaptation

## Related Components

This module is part of the broader Agent-Zero architecture:

- **Phase 1**: Foundation Layer (AgentZeroCore, CognitiveLoop, TaskManager, KnowledgeIntegrator)
- **Phase 2**: Perception & Action
- **Phase 3**: Knowledge & Reasoning
- **Phase 4**: Planning & Goals
- **Phase 5**: Learning & Adaptation ← **This Module**
- **Phase 6**: Communication & NLP
- **Phase 7**: Memory & Context
- **Phase 8**: Tool Integration

See `AGENT-ZERO-GENESIS.md` for the complete roadmap and task dependencies.