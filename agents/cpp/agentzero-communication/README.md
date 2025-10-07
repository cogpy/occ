# Agent-Zero Communication Module

## Overview

The Agent-Zero Communication Module provides natural language processing and dialogue management capabilities for the OpenCog Agent-Zero architecture. This module implements **AZ-NLP-002: Create DialogueManager for conversations** as part of Phase 6: Communication & NLP of the AGENT-ZERO-GENESIS project.

## 🎯 Key Features

- **Multi-turn Conversation Management**: Handle ongoing dialogues with context tracking
- **AtomSpace Integration**: Full integration with OpenCog's knowledge representation
- **Goal-Oriented Dialogue**: Support for pursuing conversational goals
- **Context Tracking**: Maintain conversation state and participant information
- **Message History**: Store and retrieve conversation history
- **Natural Language Processing**: Basic NLP with optional Link Grammar integration
- **Multi-Participant Support**: Handle group conversations

## 🏗️ Architecture

### Core Components

| Component | Purpose | Dependencies |
|-----------|---------|-------------|
| **DialogueManager** | Main conversation orchestration | AtomSpace, all other components |
| **ConversationState** | Individual conversation state management | AtomSpace |
| **LanguageProcessor** | Natural language processing | AtomSpace, lg-atomese (optional) |
| **MessageHandler** | Message routing and processing | AtomSpace |

### Key Classes

#### DialogueManager
The main class that orchestrates all conversation-related activities:
- Manages multiple concurrent conversations
- Provides conversation context and state tracking
- Integrates with OpenCog's AtomSpace for knowledge representation
- Supports goal-oriented dialogue management

#### ConversationState
Manages the state of individual conversations:
- Participant tracking
- Context variable management
- Topic tracking
- Activity monitoring

#### LanguageProcessor
Handles natural language processing tasks:
- Text parsing and understanding
- Response generation
- Intent detection
- Entity extraction

#### MessageHandler
Manages message processing and routing:
- Message validation
- AtomSpace representation
- Message history queries
- Callback management

## 🚀 Quick Start

### Basic Usage

```cpp
#include <opencog/atomspace/AtomSpace.h>
#include "opencog/agentzero/DialogueManager.h"

using namespace opencog;
using namespace opencog::agentzero;

// Create AtomSpace and DialogueManager
AtomSpacePtr atomspace = createAtomSpace();
DialogueManager dialogue_manager(atomspace, "MyAgent");

// Start a conversation
std::vector<std::string> participants = {"MyAgent", "Human"};
dialogue_manager.startConversation("conv1", participants);

// Process a message
std::string response = dialogue_manager.processMessage("conv1", "Human", "Hello!");
std::cout << "Agent response: " << response << std::endl;

// Set conversation context
dialogue_manager.setConversationTopic("conv1", "AI Research");
dialogue_manager.setConversationContext("conv1", "mood", "helpful");
```

### Advanced Features

```cpp
// Goal-oriented dialogue
Handle goal_atom = atomspace->add_node(CONCEPT_NODE, "ProvideAssistance");
dialogue_manager.addConversationGoal("conv1", goal_atom);

// Message history
auto history = dialogue_manager.getConversationHistory("conv1", 10);
for (const auto& message : history) {
    std::cout << message.sender_id << ": " << message.content << std::endl;
}

// Search messages
auto search_results = dialogue_manager.searchMessageHistory("conv1", "research");
```

## 🔧 Building

### Dependencies

**Required:**
- cogutil
- atomspace
- Boost libraries

**Optional:**
- lg-atomese (for enhanced language processing)
- link-grammar (for grammatical parsing)
- opencog (for additional NLP features)

### Build Instructions

```bash
# From the repository root
mkdir -p build && cd build
cmake ..
make agentzero-communication

# Install (optional)
sudo make install
sudo ldconfig
```

### Build with Tests

```bash
cmake -DBUILD_TESTING=ON ..
make
make test
```

## 🧪 Testing

The module includes comprehensive unit tests using CxxTest:

```bash
# Run all communication tests
cd build
make test

# Run specific test
./tests/DialogueManagerUTest
```

### Test Coverage

- Basic conversation management (start/end)
- Message processing and response generation
- Context and state management
- Goal-oriented dialogue features
- Message history and search
- AtomSpace integration
- Multi-conversation handling

## 📋 Examples

### Running the Example

```bash
cd build
make dialogue_manager_example
./examples/dialogue_manager_example
```

The example demonstrates:
- Basic conversation flow
- Multiple concurrent conversations
- Context tracking and management
- AtomSpace integration
- Goal-oriented dialogue

## 🔧 Configuration

### DialogueManager Settings

```cpp
// Set maximum conversation history
dialogue_manager.setMaxConversationHistory(1000);

// Set conversation timeout
dialogue_manager.setConversationTimeout(std::chrono::minutes(30));

// Enable/disable features
dialogue_manager.setContextTracking(true);
dialogue_manager.setGoalOrientedDialogue(true);
```

### Language Processing Options

```cpp
// Configure language processor
auto* lang_processor = dialogue_manager.getLanguageProcessor();
lang_processor->setUseLinks(true);  // Enable Link Grammar if available
lang_processor->setLanguageModel("/path/to/model");
```

## 📊 AtomSpace Integration

The module creates and maintains several types of atoms:

### Core Atoms
- **Agent Self Atom**: Represents the dialogue agent
- **Conversation Atoms**: Represent individual conversations
- **Message Atoms**: Represent individual messages
- **Context Atoms**: Store conversation context

### Relationship Links
- **Member Links**: Connect components to conversations
- **Evaluation Links**: Store properties and relationships
- **Ordered Links**: Represent message sequences

### Example AtomSpace Structure

```
(ConceptNode "MyAgent")
(ConceptNode "Conversation:conv1")
(ConceptNode "Message:msg_123")
(EvaluationLink
    (PredicateNode "manages")
    (ListLink
        (ConceptNode "MyAgent")
        (ConceptNode "Conversation:conv1")))
```

## 🔄 Integration with Other Modules

### Agent-Zero Core Integration

```cpp
// In AgentZeroCore
#include "opencog/agentzero/DialogueManager.h"

class AgentZeroCore {
private:
    std::unique_ptr<DialogueManager> _dialogue_manager;
    
public:
    void initializeCommunication() {
        _dialogue_manager = std::make_unique<DialogueManager>(
            _atomspace, _agent_name);
    }
    
    std::string processUserMessage(const std::string& message) {
        return _dialogue_manager->processMessage(
            "user_session", "Human", message);
    }
};
```

### Task Manager Integration

The DialogueManager can work with TaskManager to:
- Create tasks based on conversation goals
- Report task completion in dialogue
- Track conversational objectives

## 📈 Performance Considerations

- **Memory Usage**: Scales with conversation history and active conversations
- **Response Time**: Basic responses < 10ms, NLP processing varies
- **Concurrency**: Thread-safe for multiple concurrent conversations
- **AtomSpace Efficiency**: Uses efficient atom creation and querying

### Optimization Tips

1. **History Management**: Set appropriate `max_conversation_history`
2. **Context Pruning**: Remove unused context variables
3. **Goal Cleanup**: Remove completed conversational goals
4. **Inactive Conversations**: Use timeout to clean up old conversations

## 🔮 Future Enhancements

- **Enhanced NLP**: Integration with modern transformer models
- **Emotion Recognition**: Detect and respond to emotional states
- **Multi-Language Support**: Support for multiple languages
- **Voice Integration**: Speech-to-text and text-to-speech
- **Learning Capabilities**: Adaptive responses based on conversation history
- **Advanced Context**: Semantic context understanding

## 📚 API Reference

### DialogueManager Public Methods

#### Conversation Management
- `bool startConversation(id, participants)`
- `bool endConversation(id)`
- `bool isConversationActive(id)`
- `vector<string> getActiveConversations()`

#### Message Processing
- `string processMessage(conv_id, sender, content)`
- `bool sendMessage(conv_id, recipient, content)`
- `vector<Message> getConversationHistory(conv_id, limit=0)`
- `vector<Message> searchMessageHistory(conv_id, search_term)`

#### Context Management
- `void setConversationContext(conv_id, key, value)`
- `string getConversationContext(conv_id, key)`
- `void setConversationTopic(conv_id, topic)`
- `string getConversationTopic(conv_id)`

#### Goal Management
- `void addConversationGoal(conv_id, goal_atom)`
- `void removeConversationGoal(conv_id, goal_atom)`
- `vector<Handle> getConversationGoals(conv_id)`

#### AtomSpace Integration
- `AtomSpacePtr getAtomSpace()`
- `Handle conversationToAtom(conv_id)`
- `void updateDialogueAtoms()`

## 🛠️ Troubleshooting

### Common Issues

1. **Build Errors**
   - Ensure all dependencies are installed
   - Check pkg-config paths for OpenCog components

2. **Missing NLP Features**
   - Install lg-atomese for enhanced language processing
   - Install link-grammar for grammatical parsing

3. **Memory Issues**
   - Reduce `max_conversation_history`
   - Clean up inactive conversations regularly

4. **AtomSpace Errors**
   - Verify AtomSpace is properly initialized
   - Check atom creation and querying

### Debug Mode

```cpp
// Enable detailed logging
logger().set_level(Logger::DEBUG);
logger().set_print_to_stdout_flag(true);
```

## 🤝 Contributing

This module is part of the Agent-Zero project. Contributions should:

1. Follow OpenCog coding standards
2. Include comprehensive tests
3. Update documentation
4. Maintain AtomSpace integration
5. Consider performance implications

## 📄 License

SPDX-License-Identifier: AGPL-3.0-or-later

## 🔗 Related Documentation

- [AGENT-ZERO-GENESIS.md](../../../AGENT-ZERO-GENESIS.md): Complete project roadmap
- [Agent-Zero Core Module](../agentzero-core/README.md): Core orchestration engine
- [OpenCog AtomSpace Documentation](https://wiki.opencog.org/w/AtomSpace)
- [Link Grammar](https://www.abisource.com/projects/link-grammar/): Grammatical parsing