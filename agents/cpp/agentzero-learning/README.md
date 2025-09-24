# Agent-Zero Learning Module

**Part of AZ-LEARN-003: MOSES Policy Optimization Integration**

This module implements comprehensive learning and adaptation capabilities for Agent-Zero, integrating MOSES genetic programming for policy optimization with the OpenCog AtomSpace.

## Overview

The Agent-Zero Learning Module provides:

- **PolicyOptimizer**: MOSES-based genetic programming for policy evolution
- **ExperienceManager**: Comprehensive experience memory management  
- **SkillAcquisition**: Hierarchical skill learning framework
- **MetaLearning**: Learning how to learn more effectively
- **LearningUtils**: Utility functions and common operations

## Key Features

### MOSES Integration
- Seamless integration with MOSES genetic programming
- Custom fitness functions for domain-specific optimization
- Policy evolution with configurable parameters
- AtomSpace storage for evolved policies

### Experience Management
- Experience replay buffer with configurable size limits
- Fast indexing by state, action, and reward ranges
- Prioritized and uniform sampling strategies
- Statistical analysis and pattern discovery

### AtomSpace Integration
- Native AtomSpace storage for policies and experiences
- Metadata preservation and rich querying capabilities
- Persistent storage across sessions
- Integration with existing OpenCog components

## Architecture

```
agentzero-learning/
├── include/agentzero/learning/
│   ├── LearningTypes.h          # Core types and structures
│   ├── PolicyOptimizer.h        # MOSES policy optimization
│   ├── ExperienceManager.h      # Experience memory management
│   ├── SkillAcquisition.h       # Hierarchical skill learning
│   ├── MetaLearning.h          # Meta-learning capabilities
│   └── LearningUtils.h         # Utility functions
├── src/                        # Implementation files
├── tests/                      # Unit tests
├── examples/                   # Usage examples
└── docs/                       # Documentation
```

## Dependencies

### Required
- **cogutil**: OpenCog utilities and logging
- **atomspace**: AtomSpace knowledge representation
- **moses**: MOSES genetic programming system
- **asmoses**: AtomSpace MOSES integration

### Optional
- **learn**: Additional learning algorithms
- **miner**: Pattern mining capabilities  
- **ure**: Unified Rule Engine integration

## Usage Examples

### Basic Policy Optimization

```cpp
#include <agentzero/learning/PolicyOptimizer.h>
#include <agentzero/learning/LearningUtils.h>

// Create AtomSpace and fitness function
auto atomspace = std::make_shared<AtomSpace>();
auto fitness_func = std::make_shared<MyFitnessFunction>();

// Create and initialize optimizer
LearningConfig config = utils::getDefaultConfig("fast");
auto optimizer = std::make_unique<PolicyOptimizer>(atomspace, config);
optimizer->initialize(fitness_func);

// Evolve a policy
auto policy = optimizer->evolvePolicy("my_policy");
if (policy) {
    std::cout << "Evolved policy with fitness: " << policy->fitness_score << std::endl;
}
```

### Experience Management

```cpp
#include <agentzero/learning/ExperienceManager.h>

// Create experience manager
auto manager = std::make_unique<ExperienceManager>(atomspace);

// Add experiences
Handle state = atomspace->add_node(CONCEPT_NODE, "State1");
Handle action = atomspace->add_node(CONCEPT_NODE, "Action1");  
Handle next_state = atomspace->add_node(CONCEPT_NODE, "State2");

ExperienceId exp_id = manager->addExperience(state, action, next_state, 1.0, false);

// Sample experiences for learning
auto sampled = manager->sampleExperiences(10, true); // Prioritized sampling
```

### Custom Fitness Functions

```cpp
class MyFitnessFunction : public PolicyFitnessFunction {
public:
    double evaluate(const combo_tree& program, 
                   const std::map<std::string, Handle>& context = {}) override {
        // Evaluate program fitness based on your domain
        return calculateFitness(program);
    }
    
    std::string getName() const override { return "MyFitness"; }
    std::vector<std::string> getInputFeatures() const override { 
        return {"feature1", "feature2"}; 
    }
};
```

## Configuration

The learning module supports flexible configuration:

```cpp
LearningConfig config;
config.max_evals = 10000;           // MOSES evaluations
config.max_gens = 1000;             // MOSES generations
config.population_size = 500;       // Population size
config.diversity_pressure = 0.1;    // Diversity pressure
config.experience_buffer_size = 1000; // Experience buffer size
config.learning_rate = 0.01;        // Learning rate
```

Preset configurations are available:
- `"fast"`: Quick evolution for testing
- `"thorough"`: Comprehensive evolution for production
- `"memory_efficient"`: Optimized for memory usage
- `"default"`: Balanced configuration

## Testing

Unit tests are provided for all major components:

```bash
# Build with tests enabled
cmake -DBUILD_TESTING=ON ..
make

# Run tests
make test
```

## Examples

Complete examples are provided in the `examples/` directory:

- `policy_optimization_example.cpp`: Demonstrates MOSES policy evolution
- `experience_management_example.cpp`: Shows experience storage and retrieval

Build and run examples:

```bash
make policy_optimization_example
./policy_optimization_example

make experience_management_example  
./experience_management_example
```

## Performance Considerations

- **Memory Usage**: Experience buffer size directly affects memory usage
- **Evolution Time**: MOSES parameters significantly impact evolution time
- **AtomSpace Storage**: Consider periodic cleanup for long-running systems
- **Thread Safety**: All components are thread-safe for concurrent access

## Integration with Agent-Zero

This module integrates seamlessly with other Agent-Zero components:

- **agentzero-core**: Provides foundational cognitive loop integration
- **agentzero-perception**: Supplies sensory input for experience generation  
- **agentzero-planning**: Uses evolved policies for action planning
- **agentzero-knowledge**: Leverages learned patterns for reasoning

## Future Enhancements

- Advanced prioritized experience replay algorithms
- Multi-objective policy optimization
- Transfer learning between related tasks
- Online skill discovery and composition
- Integration with deep learning frameworks

## Contributing

When contributing to this module:

1. Follow OpenCog coding standards
2. Add comprehensive unit tests for new features
3. Update documentation and examples
4. Ensure thread safety for concurrent components
5. Test integration with MOSES and AtomSpace

## License

This module is part of the Agent-Zero project and follows the same licensing terms as the parent project.