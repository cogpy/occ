# Agent-Zero Perception Component

This component implements the MultiModalSensor interface for Agent-Zero's perception subsystem, providing unified access to various sensory inputs with deep OpenCog AtomSpace integration.

## Overview

The MultiModalSensor interface enables Agent-Zero to process and integrate multi-modal sensory data including visual, auditory, textual, tactile, temporal, and spatial inputs. All sensor data is automatically converted into AtomSpace representations for seamless integration with OpenCog's reasoning and learning systems.

## Key Features

- **Multi-Modal Integration**: Support for multiple sensory modalities with unified processing
- **AtomSpace Integration**: Automatic conversion of sensor data to AtomSpace atoms and values
- **Attention Management**: ECAN-compatible attention allocation for sensor data
- **Performance Monitoring**: Real-time quality metrics and performance tracking
- **Extensible Architecture**: Easy to add custom sensor types and processing modes
- **Thread-Safe Operations**: Concurrent sensor data processing and queue management

## Architecture

### Core Classes

- **MultiModalSensor**: Abstract base class defining the sensor interface
- **TextualSensor**: Concrete implementation for text-based input processing
- **SensorMetrics**: Data structure for tracking sensor quality and performance

### Processing Modes

The TextualSensor supports four processing modes:

1. **Words Mode**: Tokenizes text into individual words, creating WORD_NODE atoms
2. **Sentences Mode**: Splits text into sentences, creating SENTENCE_NODE atoms  
3. **Documents Mode**: Treats input as complete documents with metadata
4. **Stream Mode**: Processes text as continuous stream chunks with timestamps

## Usage

### Basic Example

```cpp
#include <opencog/agentzero/perception/TextualSensor.h>
#include <opencog/atomspace/AtomSpace.h>

// Create AtomSpace and sensor
AtomSpace* as = new AtomSpace();
auto sensor = std::make_shared<TextualSensor>(as, "my_sensor", "input_source");

// Initialize and start
sensor->initialize();
sensor->start();

// Process text input
sensor->set_text_mode("sentences");
sensor->add_text_input("Hello world! This is a test sentence.");
Handle result = sensor->read_data();

// Check metrics
SensorMetrics metrics = sensor->get_metrics();
std::cout << "Confidence: " << metrics.confidence << std::endl;

sensor->stop();
```

### Multi-Modal Fusion

```cpp
// Create multiple sensor readings
sensor->add_text_input("First input");
Handle data1 = sensor->read_data();

sensor->add_text_input("Second input");  
Handle data2 = sensor->read_data();

// Fuse multiple modalities
std::vector<Handle> sensor_data = {data1, data2};
Handle fused = sensor->fuse_modalities(sensor_data, "weighted_average");
```

## Building

The component requires OpenCog's cogutil and atomspace libraries:

```bash
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
make
make test
make install
```

## Dependencies

- OpenCog CogUtil (>= 2.0.3)
- OpenCog AtomSpace (>= 5.0.4) 
- C++17 compatible compiler
- CMake 3.16+

## Testing

The component includes comprehensive unit tests:

```bash
# Run all tests
make test

# Run specific test suites
./MultiModalSensorUTest
./TextualSensorUTest
```

## Examples

See the `examples/` directory for:

- `basic_sensor_example.cpp`: Basic MultiModalSensor usage
- `textual_sensor_example.cpp`: Advanced TextualSensor features

## Performance Targets

- **Response Time**: < 50ms for routine text processing
- **Memory Efficiency**: Linear scaling with input size
- **Thread Safety**: Full concurrent access support
- **AtomSpace Integration**: < 5% overhead vs. direct processing

## Integration with Agent-Zero

This component implements the **AZ-PERC-001** task from the Agent-Zero Genesis project:

- Multi-modal sensory input → AtomSpace representation  
- ECAN attention allocation → Active context selection
- Quality metrics → Performance monitoring
- Extensible architecture → Custom sensor support

## Future Enhancements

- Visual sensor implementation (camera/image processing)
- Auditory sensor implementation (microphone/audio processing)
- Spatial sensor implementation (GPS/location data)
- Real-time streaming optimizations
- Advanced fusion algorithms
- Deep learning integration

## License

This software is licensed under the GNU Affero General Public License v3.0.

## Contributing

See the main OpenCog repository for contribution guidelines. This component follows OpenCog coding standards and architectural patterns.