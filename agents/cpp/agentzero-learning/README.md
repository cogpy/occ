# OpenCog AgentZero Learning System

**AZ-LEARN-004 Implementation: MetaLearning Capabilities**

This module implements comprehensive meta-learning capabilities for the AgentZero system, providing the ability to learn how to learn more effectively through experience and adaptation.

## Overview

The AgentZero Learning System consists of four main components that work together to provide advanced learning capabilities:

1. **MetaLearning** - Core meta-learning engine
2. **ExperienceManager** - Experiential memory management
3. **SkillAcquisition** - Skill learning and development
4. **PolicyOptimizer** - Policy evolution and optimization

## Architecture

```
┌─── MetaLearning ────┐     ┌─── ExperienceManager ───┐
│  • Strategy Selection│────▶│  • Experience Storage   │
│  • Transfer Learning │     │  • Memory Consolidation │
│  • Curriculum Learning│    │  • Pattern Recognition  │
│  • Performance Monitor│     │  • Contextual Indexing  │
└─────────────────────┘     └────────────────────────┘
         │                              ▲
         ▼                              │
┌─── SkillAcquisition ─┐     ┌─── PolicyOptimizer ────┐
│  • Skill Discovery  │────▶│  • Population Evolution │
│  • Imitation Learning│     │  • MOSES Integration    │
│  • Practice & Refine │     │  • Fitness Evaluation   │
│  • Context Adaptation│     │  • Multi-objective Opt. │
└─────────────────────┘     └────────────────────────┘
```

## Components

### MetaLearning

The core meta-learning engine that orchestrates learning strategies and adaptation:

**Key Features:**
- **Strategy Selection**: Dynamically selects optimal learning strategies based on context
- **Transfer Learning**: Transfers knowledge across domains and contexts
- **Curriculum Learning**: Manages structured learning progressions
- **Performance Monitoring**: Tracks and analyzes learning effectiveness
- **Meta-Reflection**: Self-analyzes and optimizes learning processes

**Learning Strategies:**
- `SUPERVISED` - Learn from labeled examples
- `UNSUPERVISED` - Discover patterns without labels
- `REINFORCEMENT` - Learn through reward/punishment
- `IMITATION` - Learn by copying successful behaviors
- `EXPLORATION` - Learn through experimentation
- `HYBRID` - Adaptive combination of strategies
- `META_ADAPTIVE` - Learn which strategy to use when

### ExperienceManager

Manages the agent's experiential memory with efficient storage and retrieval:

**Key Features:**
- **Multi-Type Storage**: Stores learning, planning, execution, social, exploration experiences
- **Contextual Indexing**: Fast retrieval by context, task, type, and importance
- **Memory Consolidation**: Automatic pruning of low-importance experiences
- **Pattern Analysis**: Discovers patterns across experience history
- **Similarity Search**: Finds similar experiences for knowledge transfer

**Experience Types:**
- `LEARNING` - Learning-related experiences
- `PLANNING` - Planning and goal-related experiences  
- `EXECUTION` - Action execution experiences
- `SOCIAL` - Social interaction experiences
- `EXPLORATION` - Exploration and discovery experiences
- `REFLECTION` - Meta-cognitive reflection experiences

### SkillAcquisition

Learns and manages skills through various acquisition methods:

**Key Features:**
- **Imitation Learning**: Learn skills from demonstrations
- **Practice-Based Learning**: Develop skills through repeated practice
- **Context Adaptation**: Apply skills appropriately in different contexts
- **Proficiency Tracking**: Monitor skill development and proficiency levels
- **Skill Composition**: Combine simple skills into complex behaviors

**Skill Representation:**
- Preconditions, actions, and postconditions
- Proficiency and confidence levels
- Practice history and usage statistics
- Context applicability mapping

### PolicyOptimizer

Evolves and optimizes policies using evolutionary algorithms:

**Key Features:**
- **Population Evolution**: Maintains and evolves policy populations
- **MOSES Integration**: Advanced optimization using MOSES algorithms
- **Multi-Objective Optimization**: Optimize multiple objectives simultaneously
- **Fitness Evaluation**: Configurable fitness functions for policy assessment
- **Elite Preservation**: Maintains best-performing policies across generations

**Optimization Methods:**
- Tournament selection
- Crossover and mutation operations
- Elite preservation strategies
- Custom fitness functions

## Usage Examples

### Basic MetaLearning Usage

```cpp
#include "opencog/agentzero/MetaLearning.h"

// Create and initialize
AtomSpacePtr atomspace = std::make_shared<AtomSpace>();
MetaLearning meta_learning(atomspace);
meta_learning.initialize();

// Learn a task
Handle task = atomspace->add_node(CONCEPT_NODE, "NavigationTask");
Handle context = atomspace->add_node(CONCEPT_NODE, "IndoorEnvironment");
Handle outcome = meta_learning.learnTask(task, context);

// Adapt learning strategy
LearningStrategy strategy = meta_learning.adaptLearningStrategy(context);

// Transfer knowledge between domains
Handle source_domain = atomspace->add_node(CONCEPT_NODE, "Navigation");
Handle target_domain = atomspace->add_node(CONCEPT_NODE, "Manipulation");
double transfer_score = meta_learning.transferKnowledgeBetweenDomains(source_domain, target_domain);
```

### Experience Management

```cpp
#include "opencog/agentzero/ExperienceManager.h"

ExperienceManager experience_manager(atomspace);
experience_manager.initialize();

// Record experiences
Handle exp_id = experience_manager.recordExperience(
    ExperienceType::LEARNING, context, task, outcome, 0.8);

// Query experiences
ExperienceQuery query;
query.type_filter = ExperienceType::LEARNING;
query.min_importance = 0.5;
auto experiences = experience_manager.queryExperiences(query);

// Analyze patterns
Handle patterns = experience_manager.analyzeExperiencePatterns(std::chrono::hours(24));
```

### Skill Acquisition

```cpp
#include "opencog/agentzero/SkillAcquisition.h"

SkillAcquisition skill_acquisition(atomspace);
skill_acquisition.initialize();

// Learn from demonstration
Handle demonstration = atomspace->add_node(CONCEPT_NODE, "WalkingDemo");
Handle skill_id = skill_acquisition.learnSkillFromDemonstration(
    demonstration, context, "Walking");

// Practice skill
skill_acquisition.practiceSkill(skill_id, context);

// Execute skill
Handle result = skill_acquisition.executeSkill(skill_id, context);
```

### Policy Optimization

```cpp
#include "opencog/agentzero/PolicyOptimizer.h"

PolicyOptimizer policy_optimizer(atomspace);
policy_optimizer.initialize();

// Create policy
Handle conditions = atomspace->add_node(CONCEPT_NODE, "LowBattery");
Handle actions = atomspace->add_node(CONCEPT_NODE, "FindCharger");
Handle policy_id = policy_optimizer.createPolicy("ChargingPolicy", conditions, actions);

// Optimize policies
Handle objective = atomspace->add_node(CONCEPT_NODE, "MaximizeUptime");
Handle best_policy = policy_optimizer.optimizePolicies(context, objective, 50);
```

## Configuration

Each component provides extensive configuration options:

### MetaLearningConfig
- `meta_learning_rate` - Rate of meta-level adaptation
- `exploration_factor` - Balance between exploration/exploitation
- `max_experience_history` - Maximum experiences to retain
- `enable_transfer_learning` - Enable cross-domain transfer
- `enable_curriculum_learning` - Enable structured learning progression

### SkillAcquisitionConfig
- `min_proficiency_threshold` - Minimum proficiency to consider skill learned
- `learning_rate` - Rate of skill improvement
- `max_practice_attempts` - Maximum practice attempts per session
- `enable_imitation_learning` - Enable learning by imitation
- `skill_decay_period` - Time after which unused skills decay

### PolicyOptimizerConfig
- `population_size` - Size of policy population
- `max_generations` - Maximum generations for evolution
- `mutation_rate` - Rate of policy mutation
- `crossover_rate` - Rate of policy crossover
- `enable_moses_integration` - Enable MOSES-based optimization

## Integration with OpenCog

The learning system integrates deeply with OpenCog components:

- **AtomSpace**: All knowledge and experiences are stored as atoms
- **MOSES**: Policy optimization leverages MOSES algorithms
- **ASMoses**: AtomSpace-based evolution for complex policies
- **Learn**: Integration with existing learning components
- **PLN**: Probabilistic reasoning about learning effectiveness

## Performance Targets

- **Response Time**: < 10ms for strategy selection
- **Memory Efficiency**: Linear scaling with experience count
- **Learning Rate**: Demonstrable improvement within 100 interactions
- **Transfer Effectiveness**: > 70% knowledge retention across domains
- **Skill Proficiency**: > 80% success rate for practiced skills

## Testing

Comprehensive unit tests are provided for all components:

```bash
# Build tests (requires CxxTest)
mkdir build && cd build
cmake .. -DBUILD_TESTS=ON
make

# Run tests
./tests/MetaLearningUTest
./tests/ExperienceManagerUTest
./tests/SkillAcquisitionUTest
./tests/PolicyOptimizerUTest
```

## Dependencies

- **Required**: OpenCog AtomSpace, CogUtil
- **Optional**: MOSES, ASMoses, Learn components
- **Build**: CMake 3.16+, C++17 compiler
- **Test**: CxxTest (for unit tests)

## Building

The component is integrated with the OpenCog unified build system:

```bash
# Configure
mkdir build && cd build
cmake /path/to/opencog

# Build learning system
make agents

# Or build just the learning component
cd agents-build/cpp/agentzero-learning
make
```

## Future Enhancements

- **Deep Learning Integration**: Neural network-based meta-learning
- **Multi-Agent Learning**: Collaborative learning across agents
- **Continual Learning**: Lifelong learning without catastrophic forgetting
- **Causal Learning**: Understanding causal relationships in learning
- **Active Learning**: Intelligent selection of learning experiences

## Contributing

When contributing to the learning system:

1. Follow OpenCog coding standards
2. Add comprehensive unit tests
3. Update documentation for new features
4. Ensure AtomSpace integration consistency
5. Validate performance impact

## License

SPDX-License-Identifier: AGPL-3.0-or-later

Copyright (C) 2024 OpenCog Foundation