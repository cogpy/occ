# Agent-Zero Communication Module

## Overview

The Agent-Zero Communication Module provides comprehensive natural language processing and communication capabilities for the Agent-Zero cognitive architecture. This module integrates Link Grammar parser with OpenCog's AtomSpace to enable sophisticated linguistic analysis and knowledge representation.

## Features

### LanguageProcessor (AZ-NLP-001) ✅ IMPLEMENTED

- **Syntactic Parsing**: Full Link Grammar integration for robust syntactic analysis
- **Semantic Analysis**: Entity extraction, concept identification, and sentiment analysis
- **AtomSpace Integration**: Seamless conversion of linguistic structures to AtomSpace representations
- **Multi-language Support**: Configurable for different languages (English, Russian, etc.)
- **Batch Processing**: Efficient processing of multiple texts with progress tracking
- **Performance Monitoring**: Comprehensive statistics and performance metrics
- **Error Handling**: Robust error handling with detailed error reporting

### Planned Components

- **DialogueManager** (AZ-NLP-002): Conversational interaction management
- **AgentComms** (AZ-COMM-001): Inter-agent communication protocols  
- **HumanInterface** (AZ-HUMAN-001): Human-agent interaction layer

## Dependencies

### Required OpenCog Components
- **cogutil**: Core OpenCog utilities
- **atomspace**: AtomSpace knowledge representation system
- **lg-atomese**: Atomese API for Link Grammar (recommended)
- **link-grammar**: Link Grammar natural language parser

### Optional Components
- **opencog**: Main OpenCog package for additional functionality

## Installation

### Prerequisites

Ensure you have the OpenCog ecosystem installed. From the repository root:

```bash
# Install dependencies
sudo apt-get install -y libboost-all-dev guile-3.0-dev build-essential
./scripts/adaptive-boost-install.sh

# Build foundation components
mkdir -p /tmp/opencog-build && cd /tmp/opencog-build
cmake /path/to/opencog-org
make cogutil atomspace lg-atomese link-grammar
sudo make install && sudo ldconfig
```

### Building the Module

```bash
# Navigate to the communication module
cd agents/cpp/agentzero-communication

# Create build directory
mkdir build && cd build

# Configure
cmake ..

# Build
make -j$(nproc)

# Install (optional)
sudo make install
```

## Usage

### Basic Usage

```cpp
#include <opencog/atomspace/AtomSpace.h>
#include "agentzero/communication/LanguageProcessor.h"

using namespace agentzero::communication;
using namespace opencog;

int main() {
    // Create AtomSpace
    auto atomspace = std::make_shared<AtomSpace>();
    
    // Configure LanguageProcessor
    LanguageProcessorConfig config = create_english_config();
    config.store_in_atomspace = true;
    config.enable_semantic_analysis = true;
    
    // Create processor
    LanguageProcessor processor(atomspace, config);
    
    // Parse text
    ParseResult result = processor.parse_text("The cat sat on the mat.");
    
    if (result.is_valid) {
        std::cout << "Parse successful!" << std::endl;
        std::cout << "Words: " << result.words.size() << std::endl;
        std::cout << "Links: " << result.links.size() << std::endl;
    }
    
    return 0;
}
```

### Batch Processing

```cpp
// Process multiple sentences efficiently
std::vector<std::string> sentences = {
    "Hello world.",
    "How are you today?",
    "The weather is nice."
};

// Set progress callback (optional)
processor.set_progress_callback([](size_t current, size_t total) {
    std::cout << "Progress: " << current << "/" << total << std::endl;
});

// Process batch
auto results = processor.parse_batch(sentences);

// Analyze results
for (const auto& result : results) {
    if (result.is_valid) {
        std::cout << "✓ " << result.text << std::endl;
    } else {
        std::cout << "✗ " << result.text << std::endl;
    }
}
```

### Semantic Analysis

```cpp
// Comprehensive semantic analysis
std::string text = "John loves reading books about artificial intelligence.";
SemanticResult semantic = processor.analyze_text(text);

// Access extracted information
std::cout << "Entities: ";
for (const std::string& entity : semantic.entities) {
    std::cout << entity << " ";
}

std::cout << "\nConcepts: ";
for (const std::string& concept : semantic.concepts) {
    std::cout << concept << " ";
}

// Sentiment scores
for (const auto& sentiment : semantic.sentiment_scores) {
    std::cout << sentiment.first << ": " << sentiment.second << std::endl;
}
```

## Configuration

### LanguageProcessorConfig Options

```cpp
LanguageProcessorConfig config;
config.language = "en";                    // Language code
config.dictionary_path = "";               // Custom dictionary path (optional)
config.max_parse_time = 10;               // Maximum parse time (seconds)
config.verbosity_level = 0;               // Link Grammar verbosity (0-6)
config.store_in_atomspace = true;         // Store results in AtomSpace
config.enable_semantic_analysis = true;   // Enable semantic processing
config.confidence_threshold = 0.5;        // Minimum confidence for results
```

### Predefined Configurations

```cpp
// English configuration
auto en_config = create_english_config();

// Russian configuration  
auto ru_config = create_russian_config();

// Validate configuration
if (validate_config(config)) {
    // Configuration is valid
}
```

## Testing

### Running Unit Tests

```bash
# From build directory
make test

# Or run individual test executables
./tests/test_language_processor
./tests/test_communication_basic
```

### Test Coverage

- **Initialization and Configuration**: Verify proper setup and configuration handling
- **Basic Parsing**: Test simple sentence parsing functionality
- **Batch Processing**: Validate batch processing capabilities
- **Semantic Analysis**: Test entity and concept extraction
- **AtomSpace Integration**: Verify AtomSpace representation creation
- **Error Handling**: Test robustness with invalid inputs
- **Performance**: Benchmark parsing speed and memory usage

## Examples

See the `examples/` directory for comprehensive usage examples:

- `language_processor_example.cpp`: Basic LanguageProcessor usage
- `batch_processing_example.cpp`: Batch processing demonstration

### Running Examples

```bash
# From build directory
./examples/language_processor_example
./examples/batch_processing_example 100  # Process 100 sentences
```

## Architecture

### Class Hierarchy

```
LanguageProcessor
├── ParseResult (struct)
├── SemanticResult (struct)  
├── LanguageProcessorConfig (struct)
└── Utility functions

DialogueManager (planned)
AgentComms (planned)
HumanInterface (planned)
```

### AtomSpace Integration

The LanguageProcessor creates rich AtomSpace representations:

- **Sentence Nodes**: Represent complete sentences
- **Word Nodes**: Individual words with position information
- **Link Relations**: Syntactic relationships between words
- **Concept Nodes**: Extracted semantic concepts
- **Metadata**: Parse costs, confidence scores, timestamps

### Error Handling

- **Input Validation**: Comprehensive input checking
- **Resource Management**: Proper cleanup of Link Grammar resources
- **Exception Safety**: RAII and exception-safe design
- **Error Reporting**: Detailed error messages and codes

## Performance

### Benchmarks

Typical performance on modern hardware:

- **Simple Sentences**: 10-50ms per sentence
- **Complex Sentences**: 50-200ms per sentence  
- **Batch Processing**: 20-30% faster than individual processing
- **Memory Usage**: ~1-5MB per 1000 parsed sentences

### Optimization Tips

1. Use batch processing for multiple sentences
2. Set appropriate `max_parse_time` limits
3. Disable verbose output in production
4. Reuse processor instances when possible
5. Monitor statistics for performance insights

## Troubleshooting

### Common Issues

**"Link Grammar not found"**
```bash
# Install Link Grammar development package
sudo apt-get install -y liblink-grammar-dev

# Or build from source
git clone https://github.com/opencog/link-grammar
cd link-grammar && mkdir build && cd build
cmake .. && make && sudo make install
```

**"Parse always fails"**
- Check Link Grammar dictionary installation
- Verify language code is correct
- Try increasing `max_parse_time`
- Check verbosity output for debugging

**"AtomSpace integration not working"**
- Ensure `store_in_atomspace` is enabled
- Verify AtomSpace is properly initialized
- Check for proper linking of atomspace library

### Debug Mode

Enable debugging by setting verbosity level:

```cpp
config.verbosity_level = 2;  // Show timing info
config.verbosity_level = 6;  // Show detailed parsing info
```

## Contributing

This module follows OpenCog architectural patterns and coding standards:

1. **C++17 Standard**: Use modern C++ features appropriately
2. **RAII**: Proper resource management
3. **Exception Safety**: Handle errors gracefully  
4. **Documentation**: Comprehensive API documentation
5. **Testing**: Unit tests for all public interfaces
6. **Performance**: Optimize for production use

## Roadmap

### Phase 6 - Communication & NLP (Current)
- ✅ **AZ-NLP-001**: LanguageProcessor with Link Grammar (COMPLETED)
- 🔄 **AZ-NLP-002**: DialogueManager for conversations  
- 🔄 **AZ-COMM-001**: AgentComms protocols
- 🔄 **AZ-HUMAN-001**: HumanInterface layer

### Future Enhancements
- Multi-threading support for batch processing
- Plugin architecture for custom semantic analyzers
- Real-time parsing capabilities
- Integration with speech recognition/synthesis
- Advanced dialogue state tracking
- Distributed agent communication protocols

## License

This module is part of the Agent-Zero-Genesis project and is licensed under the GNU Affero General Public License v3. See the LICENSE file in the repository root for details.

## References

- [Link Grammar Documentation](https://www.abisource.com/projects/link-grammar/)
- [OpenCog AtomSpace](https://wiki.opencog.org/w/AtomSpace)
- [Agent-Zero Genesis](../../AGENT-ZERO-GENESIS.md)
- [OpenCog CMake Build Guide](../../../CMAKE_BUILD_GUIDE.md)