# Agent-Zero Learning Module

**Comprehensive Learning & Adaptation with MOSES Policy Optimization and Advanced Experience Management**

This module implements comprehensive learning and adaptation capabilities for Agent-Zero, combining:
- **AZ-LEARN-003**: MOSES genetic programming for policy optimization
- **Advanced Experience Management**: Sophisticated experience classification and pattern discovery
- **Full AtomSpace Integration**: Native storage with hierarchical organization

## Overview

The Agent-Zero Learning Module provides a complete learning ecosystem:

- **PolicyOptimizer**: MOSES-based genetic programming for policy evolution
- **ExperienceManager**: Advanced experience memory with 8 experience types and 6 outcome classifications
- **SkillAcquisition**: Hierarchical skill learning framework
- **MetaLearning**: Learning how to learn more effectively
- **LearningUtils**: Comprehensive utility functions and common operations

## Key Features

### MOSES Policy Optimization Integration
- Seamless integration with MOSES genetic programming
- Custom fitness functions for domain-specific optimization
- Policy evolution with configurable parameters
- AtomSpace storage for evolved policies with metadata
- Continuous background optimization with threading support

### Advanced Experience Management
- **8 Experience Types**: ACTION_OUTCOME, INTERACTION, PROBLEM_SOLVING, SKILL_APPLICATION, GOAL_PURSUIT, UNEXPECTED, LEARNING_EPISODE, EMOTIONAL
- **6 Outcome Classifications**: SUCCESS, FAILURE, PARTIAL_SUCCESS, UNEXPECTED_OUTCOME, INCONCLUSIVE, LEARNING_OPPORTUNITY
- **5 Importance Levels**: CRITICAL (100), HIGH (75), MEDIUM (50), LOW (25), ROUTINE (10)
- **Pattern Discovery**: Sequential patterns, causal patterns, success/failure analysis
- **Experience Replay**: Both prioritized and uniform sampling strategies
- **Statistical Analysis**: Real-time reward distribution tracking

### AtomSpace Integration
- Native AtomSpace storage for policies and experiences
- Hierarchical organization with inheritance links
- Temporal indexing using timestamps
- Context-aware relationships between experiences
- Metadata preservation and rich querying capabilities
- Persistent storage across sessions
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

## Architecture

```
agentzero-learning/
├── include/agentzero/learning/
│   ├── LearningTypes.h          # Core types, experience classification
│   ├── PolicyOptimizer.h        # MOSES policy optimization
│   ├── ExperienceManager.h      # Advanced experience management
│   ├── SkillAcquisition.h       # Hierarchical skill learning
│   ├── MetaLearning.h          # Meta-learning capabilities
│   └── LearningUtils.h         # Utility functions
├── src/                        # Implementation files
├── tests/                      # Unit tests
├── examples/                   # Usage examples
└── cmake/                      # Build configuration

Experience Management Architecture:
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

## Dependencies

### Required
- **cogutil**: OpenCog utilities and logging
- **atomspace**: AtomSpace knowledge representation

### Optional (Enhanced Functionality)
- **moses**: MOSES genetic programming system
- **asmoses**: AtomSpace MOSES integration
- **learn**: Additional learning algorithms
- **miner**: Pattern mining capabilities  
- **ure**: Unified Rule Engine integration
- **cogserver**: Server integration and monitoring

## Usage Examples

### Advanced Experience Recording

```cpp
#include <agentzero/learning/ExperienceManager.h>
#include <agentzero/learning/LearningTypes.h>

// Create AtomSpace and experience manager
auto atomspace = std::make_shared<AtomSpace>();
ExperienceManager exp_manager(atomspace);

// Record a successful problem-solving experience
std::vector<Handle> actions = {
    atomspace->add_node(CONCEPT_NODE, "AnalyzeProblem"),
    atomspace->add_node(CONCEPT_NODE, "FormulateStrategy"),
    atomspace->add_node(CONCEPT_NODE, "ExecuteSolution")
};

std::vector<Handle> consequences = {
    atomspace->add_node(CONCEPT_NODE, "ProblemSolved")
};

ExperienceId exp_id = exp_manager.recordExperience(
    "Successfully solved navigation puzzle",
    ExperienceType::PROBLEM_SOLVING,
    ExperienceOutcome::SUCCESS,
    actions,
    consequences,
    0.9  // high confidence
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

// Get successful patterns for decision making
std::vector<Handle> current_context = {
    atomspace->add_node(CONCEPT_NODE, "SimilarPuzzle")
};

auto successful_patterns = exp_manager.getSuccessfulPatterns(current_context, 0.8);
```

### MOSES Policy Optimization

```cpp
#include <agentzero/learning/PolicyOptimizer.h>
#include <agentzero/learning/LearningUtils.h>

// Create policy optimizer with comprehensive configuration
LearningConfig config = utils::getDefaultConfig("thorough");
auto optimizer = std::make_unique<PolicyOptimizer>(atomspace, config);

// Create custom fitness function
class NavigationFitness : public PolicyFitnessFunction {
    double evaluate(const combo_tree& program, 
                   const std::map<std::string, Handle>& context = {}) override {
        // Evaluate navigation policy performance
        return evaluateNavigationSuccess(program);
    }
    
    std::string getName() const override { return "NavigationFitness"; }
    std::vector<std::string> getInputFeatures() const override { 
        return {"position_x", "position_y", "goal_x", "goal_y", "obstacles"}; 
    }
};

auto fitness_func = std::make_shared<NavigationFitness>();
optimizer->initialize(fitness_func);

// Evolve navigation policy
auto policy = optimizer->evolvePolicy("navigation_v1");
if (policy) {
    std::cout << "Evolved policy with fitness: " << policy->fitness_score << std::endl;
}
```

### Experience-Based Learning

```cpp
// Sample experiences for learning
auto recent_experiences = exp_manager.getRecentExperiences(50);
auto successful_experiences = exp_manager.getExperiencesByOutcome(ExperienceOutcome::SUCCESS);
auto problem_solving_experiences = exp_manager.getExperiencesByType(ExperienceType::PROBLEM_SOLVING);

// Prioritized experience replay for training
auto training_batch = exp_manager.sampleExperiences(32, true);

// Get experience statistics
auto stats = exp_manager.getExperienceStats();
auto reward_stats = exp_manager.getRewardStats();

std::cout << "Total experiences: " << stats["total_experiences"] << std::endl;
std::cout << "Average reward: " << reward_stats["mean"] << std::endl;
```

## Configuration

The learning module supports comprehensive configuration:

```cpp
LearningConfig config;

// MOSES parameters
config.max_evals = 50000;           // MOSES evaluations
config.max_gens = 5000;             // MOSES generations
config.population_size = 1000;      // Population size
config.diversity_pressure = 0.15;   // Diversity pressure

// Experience management
config.experience_buffer_size = 5000; // Experience buffer size
config.learning_rate = 0.01;        // Learning rate

// Pattern discovery thresholds
// (configured via ExperienceManager methods)
```

Preset configurations available:
- `"fast"`: Quick evolution for testing and development
- `"thorough"`: Comprehensive evolution for production use
- `"memory_efficient"`: Optimized for constrained memory environments
- `"default"`: Balanced configuration for general use

## Testing

Comprehensive unit tests for all components:

```bash
# Build with tests enabled
cmake -DBUILD_TESTING=ON ..
make

# Run all tests
make test

# Run specific test suites
make PolicyOptimizerUTest
make ExperienceManagerUTest
```

## Examples

Complete examples demonstrating real-world usage:

- `policy_optimization_example.cpp`: MOSES-based XOR problem solving
- `experience_management_example.cpp`: Advanced experience management with classification

```bash
# Build and run examples
make policy_optimization_example
./policy_optimization_example

make experience_management_example  
./experience_management_example
```

## Performance Considerations

- **Memory Usage**: Experience buffer size and pattern storage directly affect memory usage
- **Evolution Time**: MOSES parameters significantly impact evolution time
- **Pattern Discovery**: O(n²) complexity for similarity calculations, optimized with indexing
- **AtomSpace Storage**: Consider periodic cleanup for long-running systems
- **Thread Safety**: All components are thread-safe for concurrent access

## Integration with Agent-Zero

This module integrates seamlessly with the complete Agent-Zero architecture:

- **agentzero-core**: Provides foundational cognitive loop integration
- **agentzero-perception**: Supplies sensory input for experience generation  
- **agentzero-planning**: Uses evolved policies for action planning
- **agentzero-knowledge**: Leverages learned patterns for reasoning
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
- **Advanced prioritized experience replay algorithms**
- **Multi-objective policy optimization**
- **Transfer learning between related tasks**
- **Online skill discovery and composition**
- **Integration with deep learning frameworks**

## Building

The learning module integrates with the unified OpenCog build system:

```bash
# Using unified build system
cd /path/to/pycog0
mkdir /tmp/opencog-build && cd /tmp/opencog-build
cmake /path/to/pycog0
make Foundation-layer  # Build dependencies
make atomspace         # Build atomspace
make agents           # Build agent components including learning module

# Direct build
cd /path/to/pycog0/agents/cpp/agentzero-learning
mkdir build && cd build
cmake ..
make
```

## Contributing

When contributing to this comprehensive learning module:

1. Follow OpenCog coding standards and architectural patterns
2. Add comprehensive unit tests for new features
3. Update documentation and examples for new functionality
4. Ensure thread safety for concurrent components
5. Test integration with MOSES, AtomSpace, and other components
6. Consider both policy optimization and experience management aspects
7. Maintain compatibility with the broader Agent-Zero ecosystem

## License

This module is part of the Agent-Zero project and follows the same licensing terms as the parent project.

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
