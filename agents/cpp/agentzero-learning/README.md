# Agent-Zero Learning & Adaptation Module

## Overview

The Agent-Zero Learning & Adaptation Module (agentzero-learning) implements Phase 5 of the Agent-Zero architecture, providing ASMOSES (AtomSpace MOSES) integration for evolutionary learning and adaptation within the OpenCog cognitive architecture.

## Features

### Core Components

- **ASMOSESIntegrator**: Main integration class providing the interface between Agent-Zero and ASMOSES evolutionary systems
- **AtomSpaceEvolver**: Core evolutionary algorithms specifically designed for AtomSpace program evolution
- **ExperienceManager**: Manages agent experiential memory for learning-based adaptation
- **PolicyOptimizer**: Uses evolutionary algorithms to optimize agent policies and behaviors
- **SkillAcquisition**: Learns new capabilities through experience analysis
- **MetaLearning**: Optimizes learning parameters and strategies

### Key Capabilities

- **Program Evolution**: Evolve Atomese programs directly within the AtomSpace
- **Policy Optimization**: Evolutionary optimization of agent policies using historical experience
- **Skill Learning**: Automatic acquisition of new skills from experiential data
- **AtomSpace Integration**: Deep integration with OpenCog's AtomSpace knowledge representation
- **Semantic Preservation**: Evolution that maintains semantic validity and type constraints
- **Experience-Driven Learning**: Learning guided by agent's experiential history

## Dependencies

### Required
- **cogutil**: OpenCog utilities library
- **atomspace**: OpenCog AtomSpace framework
- **moses** or **asmoses**: MOSES evolutionary algorithm library
- **Boost**: C++ libraries (system, filesystem, program_options, etc.)

### Optional
- **cogserver**: CogServer integration (enables additional features)
- **cxxtest**: Unit testing framework (for running tests)

## Building

The module uses CMake and integrates with the OpenCog unified build system:

```bash
# From the main repository root
mkdir -p /tmp/opencog-build && cd /tmp/opencog-build
cmake /path/to/repository
make agentzero-learning
```

Or build individually:

```bash
cd agents/cpp/agentzero-learning
mkdir build && cd build
cmake ..
make
```

## Usage

### Basic Integration

```cpp
#include "agentzero-learning/ASMOSESIntegrator.h"

// Create AtomSpace and integrator
auto atomspace = std::make_shared<AtomSpace>();
ASMOSESIntegrator integrator(atomspace, "my-agent");

// Initialize
if (!integrator.initialize()) {
    // Handle initialization error
}

// Evolve a program
Handle problem = atomspace->add_node(CONCEPT_NODE, "problem");
auto fitness_fn = [](const Handle& program) -> double {
    // Evaluate program fitness
    return evaluate_program(program);
};

Handle evolved_program = integrator.evolveProgram(problem, fitness_fn, 100);
```

### Policy Optimization

```cpp
// Optimize agent policy using experience
Handle policy = atomspace->add_node(CONCEPT_NODE, "current_policy");
std::vector<Handle> experiences = get_agent_experiences();

auto reward_fn = [](const Handle& policy, const std::vector<Handle>& exp) -> double {
    return calculate_policy_reward(policy, exp);
};

Handle optimized_policy = integrator.optimizePolicy(policy, experiences, reward_fn);
```

### Skill Learning

```cpp
// Learn new skills from experiences
std::vector<Handle> experience_data = get_learning_experiences();
auto learned_skills = integrator.learnSkills(experience_data);

for (const auto& skill : learned_skills) {
    std::cout << "Learned skill: " << skill->to_string() << std::endl;
}
```

## Examples

See the `examples/` directory for complete usage examples:

- `asmoses_evolution_example.cpp`: Demonstrates basic program evolution
- Additional examples showing policy optimization and skill learning

Run examples:

```bash
# After building
./build/examples/asmoses_evolution_example
```

## Testing

The module includes comprehensive unit tests using CxxTest:

```bash
# Build and run tests
cd build
make test
```

Individual test suites:
- `ASMOSESIntegratorTest`: Tests main integration functionality
- `AtomSpaceEvolverTest`: Tests evolutionary algorithms
- Additional component-specific tests

## Architecture Integration

### Agent-Zero Integration

This module integrates with the Agent-Zero core module (agentzero-core) to provide learning capabilities:

```cpp
// In Agent-Zero core initialization
if (moses_available) {
    _learning_module = std::make_unique<ASMOSESIntegrator>(_atomspace, _agent_name);
    _learning_module->initialize();
}
```

### OpenCog Integration

- **AtomSpace**: All evolved programs and learned knowledge stored as atoms
- **Type System**: Respects OpenCog type constraints during evolution
- **Truth Values**: Utilizes truth values for fitness and confidence measures
- **Pattern Matching**: Leverages pattern matching for experience analysis

## Configuration

### Evolution Parameters

```cpp
std::map<std::string, std::string> params;
params["max_generations"] = "100";
params["population_size"] = "500";
params["mutation_rate"] = "0.1";
params["crossover_rate"] = "0.8";
params["fitness_threshold"] = "0.95";

integrator.setEvolutionParams(params);
```

### Logging

```cpp
// Enable/disable detailed logging
integrator.setLogging(true);

// Set log level
logger().set_level(Logger::DEBUG);
```

## Performance Considerations

- **Population Size**: Larger populations provide better solutions but require more computation
- **Generation Limits**: Balance between solution quality and computation time  
- **Memory Usage**: Evolution maintains populations in memory; monitor usage for large problems
- **AtomSpace Size**: Large AtomSpaces may impact evolution performance

## Extension Points

The module is designed for extensibility:

- **Custom Fitness Functions**: Implement domain-specific evaluation functions
- **Evolution Operators**: Add specialized crossover and mutation operators
- **Experience Parsers**: Create parsers for different experience formats
- **Integration Modules**: Connect with additional OpenCog components

## License

This module is part of the OpenCog project and is licensed under the GNU Affero General Public License v3.

## Contributing

Contributions are welcome! Please follow OpenCog coding standards and include tests for new functionality.

## Documentation

- API documentation: Generated from source comments using Doxygen
- Architecture overview: See main Agent-Zero documentation
- OpenCog integration: See OpenCog documentation

## Support

For questions and support:
- OpenCog mailing list
- GitHub issues
- OpenCog forums