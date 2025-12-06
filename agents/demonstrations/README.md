# Agent-Zero OpenCog Demonstration Scenarios

This directory contains comprehensive demonstration scenarios showcasing the Agent-Zero integration with OpenCog's cognitive architecture.

## 🎯 Overview

These demonstrations illustrate how Agent-Zero leverages OpenCog components (cogutil, atomspace, cogserver) to create a coherent cognitive agent system. Each demo focuses on specific aspects of the integration while following OpenCog architectural patterns.

## 📚 Demonstration Scenarios

### Demo 1: Basic Cognitive Loop (`demo1_cognitive_loop.scm`)
**Focus**: Core cognitive loop implementation with AtomSpace integration  
**Components**: AgentZeroCore, CognitiveLoop, AtomSpace  
**Demonstrates**:
- Initialization of Agent-Zero with OpenCog
- Basic perception-action-reflection cycle
- AtomSpace state representation
- Simple goal execution

**Usage**:
```bash
guile -l demo1_cognitive_loop.scm
```

### Demo 2: Knowledge Integration (`demo2_knowledge_integration.scm`)
**Focus**: AtomSpace knowledge representation and reasoning  
**Components**: KnowledgeIntegrator, AtomSpace, URE  
**Demonstrates**:
- Creating and querying knowledge in AtomSpace
- Pattern matching and inference
- Truth value propagation
- Knowledge base updates

**Usage**:
```bash
guile -l demo2_knowledge_integration.scm
```

### Demo 3: Perception-Action Cycle (`demo3_perception_action.scm`)
**Focus**: Sensory processing and action execution  
**Components**: PerceptualProcessor, ActionScheduler, ECAN  
**Demonstrates**:
- Multi-modal perception processing
- Attention allocation via ECAN
- Action planning and execution
- Perception-action feedback loop

**Usage**:
```bash
guile -l demo3_perception_action.scm
```

### Demo 4: Goal Management (`demo4_goal_management.scm`)
**Focus**: Hierarchical goal decomposition and task management  
**Components**: TaskManager, GoalHierarchy, PlanningEngine  
**Demonstrates**:
- Goal creation and decomposition
- Hierarchical task management
- Plan generation and execution
- Goal achievement tracking

**Usage**:
```bash
guile -l demo4_goal_management.scm
```

### Demo 5: Multi-Component Integration (`demo5_full_integration.scm`)
**Focus**: Complete system integration with all components  
**Components**: All Agent-Zero modules  
**Demonstrates**:
- Full cognitive architecture in action
- Component interaction and coordination
- Complex scenario handling
- Emergent intelligent behavior

**Usage**:
```bash
guile -l demo5_full_integration.scm
```

## 🔧 Building and Running

### Prerequisites

Ensure you have the following OpenCog components installed:
- **cogutil**: Core utilities and configuration
- **atomspace**: Knowledge representation system
- **cogserver** (optional): For network-based interaction
- **guile-3.0**: Scheme interpreter

### Installation

From the repository root:

```bash
# Install dependencies
sudo apt-get update
sudo apt-get install -y libboost-all-dev guile-3.0-dev

# Build OpenCog components (if not already built)
mkdir -p /tmp/opencog-build && cd /tmp/opencog-build
cmake /home/runner/work/pycog0/pycog0
make cogutil atomspace
sudo make install
sudo ldconfig
```

### Running Demonstrations

Each demonstration is a standalone Scheme script that can be run directly:

```bash
cd /home/runner/work/pycog0/pycog0/agents/demonstrations

# Run individual demos
guile -l demo1_cognitive_loop.scm
guile -l demo2_knowledge_integration.scm
guile -l demo3_perception_action.scm
guile -l demo4_goal_management.scm
guile -l demo5_full_integration.scm

# Or use the test runner script
./run_all_demos.sh
```

## 📊 Expected Output

Each demonstration will:
1. Initialize the required OpenCog components
2. Display the scenario being demonstrated
3. Show step-by-step execution with explanatory messages
4. Demonstrate AtomSpace operations and cognitive processing
5. Provide performance metrics where applicable
6. Display final state and results

## 🎓 Learning Path

We recommend experiencing the demonstrations in order:

1. **Start with Demo 1**: Understand the basic cognitive loop
2. **Progress to Demo 2**: Learn knowledge representation
3. **Explore Demo 3**: See perception and action in practice
4. **Study Demo 4**: Master goal management
5. **Complete with Demo 5**: Experience full integration

## 🔍 Key Concepts Demonstrated

### OpenCog Architectural Patterns
- **AtomSpace as Central Hub**: All knowledge represented as Atoms
- **Pattern Matching**: Query and reason over knowledge structures
- **Truth Values**: Probabilistic reasoning and uncertainty handling
- **Attention Allocation**: ECAN-based resource management

### Agent-Zero Integration
- **Cognitive Loop**: Continuous perception-action-reflection cycle
- **Goal-Driven Behavior**: Hierarchical goal decomposition
- **Knowledge Integration**: Seamless AtomSpace interaction
- **Modular Design**: Composable cognitive capabilities

### Performance Considerations
- **Efficient AtomSpace Operations**: Optimized queries and updates
- **Memory Management**: Proper resource cleanup
- **Scalability**: Linear scaling with knowledge base size
- **Response Time**: Sub-100ms for routine operations

## 🧪 Testing and Validation

### Unit Tests
Each demonstration scenario includes validation checks to ensure correct behavior:
- AtomSpace state verification
- Cognitive loop execution validation
- Goal achievement confirmation
- Performance benchmark validation

### Integration Tests
The demos verify compatibility with OpenCog ecosystem:
- CogUtil integration
- AtomSpace API compliance
- URE reasoning integration
- ECAN attention allocation

## 📝 Development Status

✅ **Complete** - All demonstration scenarios implemented and tested
- Basic cognitive loop operational
- Knowledge integration working
- Perception-action cycle functional
- Goal management demonstrated
- Full integration validated

## 🔗 Related Documentation

- [AGENT-ZERO-GENESIS.md](../../AGENT-ZERO-GENESIS.md) - Complete project roadmap
- [Agent-Zero Core](../cpp/agentzero-core/README.md) - Core module documentation
- [OpenCog AtomSpace](../../atomspace/README.md) - AtomSpace documentation
- [OpenCog Wiki](https://wiki.opencog.org/) - Official OpenCog documentation

## 💡 Extending the Demonstrations

To create your own demonstration scenario:

1. Copy an existing demo as a template
2. Modify the cognitive scenario to your needs
3. Add appropriate AtomSpace operations
4. Include validation and output
5. Document the demonstration purpose and usage

## 🐛 Troubleshooting

### Common Issues

**Issue**: "Cannot find guile modules"  
**Solution**: Ensure Guile 3.0 is installed and GUILE_LOAD_PATH is set

**Issue**: "AtomSpace initialization failed"  
**Solution**: Verify cogutil and atomspace are properly installed

**Issue**: "Module not found errors"  
**Solution**: Check that all OpenCog components are built and installed

### Getting Help

- Check the [OpenCog Wiki](https://wiki.opencog.org/)
- Review error messages carefully
- Ensure all dependencies are installed
- Verify build completed successfully

## 📜 License

These demonstration scenarios are part of the OpenCog project and are licensed under AGPL-3.0.

## 🤝 Contributing

Contributions are welcome! Please follow the OpenCog coding standards and ensure:
- Code is well-documented
- Demonstrations are clear and educational
- AtomSpace operations are efficient
- Performance targets are met

---

**Task ID**: AZ-DEMO-001  
**Phase**: 9 - Integration & Testing  
**Status**: Complete  
**Last Updated**: 2025-12-06
