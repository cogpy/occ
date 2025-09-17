/*
 * textual_sensor_example.cpp
 *
 * Advanced example demonstrating TextualSensor capabilities
 * Copyright (C) 2024 OpenCog Foundation
 */

#include <opencog/agentzero/perception/TextualSensor.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/util/Logger.h>
#include <iostream>
#include <memory>
#include <string>
#include <thread>
#include <chrono>
#include <iomanip>

using namespace opencog;
using namespace opencog::agentzero;

void demonstrate_text_processing(TextualSensor& sensor)
{
    std::cout << "\n=== Text Processing Demonstration ===" << std::endl;
    
    std::vector<std::string> sample_texts = {
        "The quick brown fox jumps over the lazy dog.",
        "OpenCog is a cognitive architecture framework! How exciting?",
        "Multi-modal sensors can process various types of input data streams.",
        "Agent-Zero integrates with AtomSpace for knowledge representation."
    };

    for (const auto& text : sample_texts) {
        std::cout << "\nProcessing: \"" << text << "\"" << std::endl;
        
        Handle result = sensor.process_text_line(text, false);
        if (result != Handle::UNDEFINED) {
            std::cout << "Created atoms: " << result->to_short_string() << std::endl;
            
            // Show metrics
            SensorMetrics metrics = sensor.get_metrics();
            std::cout << "Quality metrics - Signal: " << std::fixed << std::setprecision(3) 
                      << metrics.signal_strength << ", Confidence: " << metrics.confidence 
                      << ", Size: " << metrics.data_size << " bytes" << std::endl;
        }
    }
}

void demonstrate_streaming(TextualSensor& sensor)
{
    std::cout << "\n=== Streaming Mode Demonstration ===" << std::endl;
    
    sensor.set_text_mode("stream");
    
    // Simulate streaming text input
    std::vector<std::string> stream_chunks = {
        "This is chunk 1 of streaming data.",
        "Here comes chunk 2 with more information.",
        "Final chunk 3 completes the stream.",
        "Additional data chunk for demonstration."
    };

    for (size_t i = 0; i < stream_chunks.size(); ++i) {
        std::cout << "Adding stream chunk " << (i + 1) << ": " 
                  << stream_chunks[i] << std::endl;
        
        sensor.add_text_input(stream_chunks[i]);
        
        // Simulate processing delay
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
        
        Handle result = sensor.read_data(false);
        if (result != Handle::UNDEFINED) {
            std::cout << "Processed stream chunk: " << result->to_short_string() << std::endl;
        }
        
        std::cout << "Queue size: " << sensor.get_queue_size() << std::endl;
    }
}

void demonstrate_configuration(TextualSensor& sensor)
{
    std::cout << "\n=== Configuration Demonstration ===" << std::endl;
    
    // Set custom parameters
    sensor.set_parameter("custom_param", createStringValue(std::vector<std::string>{"example_value"}));
    sensor.set_parameter("threshold", createFloatValue(std::vector<double>{0.75}));
    // Note: BoolValue may not be available, using FloatValue instead
    sensor.set_parameter("enable_feature", createFloatValue(std::vector<double>{1.0}));
    
    // Retrieve parameters
    ValuePtr param1 = sensor.get_parameter("custom_param");
    ValuePtr param2 = sensor.get_parameter("threshold");
    ValuePtr param3 = sensor.get_parameter("nonexistent");
    
    std::cout << "Custom parameter: " << (param1 ? param1->to_string() : "not found") << std::endl;
    std::cout << "Threshold parameter: " << (param2 ? param2->to_string() : "not found") << std::endl;
    std::cout << "Nonexistent parameter: " << (param3 ? param3->to_string() : "not found") << std::endl;
    
    // Show current text mode
    std::cout << "Current text mode: " << sensor.get_text_mode() << std::endl;
    
    // Test mode switching
    std::vector<std::string> modes = {"words", "sentences", "documents"};
    for (const auto& mode : modes) {
        sensor.set_text_mode(mode);
        std::cout << "Switched to mode: " << sensor.get_text_mode() << std::endl;
    }
}

void demonstrate_atomspace_integration(TextualSensor& sensor, AtomSpace* as)
{
    std::cout << "\n=== AtomSpace Integration Demonstration ===" << std::endl;
    
    size_t initial_atom_count = as->get_size();
    std::cout << "Initial AtomSpace size: " << initial_atom_count << std::endl;
    
    // Process some text and see AtomSpace changes
    sensor.set_text_mode("words");
    sensor.add_text_input("integration test with multiple words for atomspace");
    Handle result = sensor.read_data(false);
    
    size_t final_atom_count = as->get_size();
    std::cout << "Final AtomSpace size: " << final_atom_count << std::endl;
    std::cout << "Atoms created: " << (final_atom_count - initial_atom_count) << std::endl;
    
    // Show sensor node
    Handle sensor_node = sensor.get_sensor_node();
    if (sensor_node != Handle::UNDEFINED) {
        std::cout << "Sensor node: " << sensor_node->to_short_string() << std::endl;
        
        // Show attached values
        Handle signal_key = as->add_node(CONCEPT_NODE, "signal_strength");
        ValuePtr signal_val = sensor_node->getValue(signal_key);
        if (signal_val) {
            std::cout << "Signal strength value: " << signal_val->to_string() << std::endl;
        }
    }
}

void demonstrate_error_handling(TextualSensor& sensor)
{
    std::cout << "\n=== Error Handling Demonstration ===" << std::endl;
    
    // Test with empty input
    bool result1 = sensor.add_text_input("");
    std::cout << "Adding empty text: " << (result1 ? "success" : "failed (expected)") << std::endl;
    
    // Test with very long input (should be handled by validation)
    std::string long_text(10000, 'x');
    bool result2 = sensor.add_text_input(long_text);
    std::cout << "Adding very long text: " << (result2 ? "success" : "failed") << std::endl;
    
    // Test invalid mode setting
    std::string old_mode = sensor.get_text_mode();
    sensor.set_text_mode("invalid_mode");
    std::cout << "After setting invalid mode, current mode: " << sensor.get_text_mode() << std::endl;
    std::cout << "Mode should remain: " << old_mode << std::endl;
}

int main()
{
    // Configure logging
    logger().set_level(Logger::INFO);
    logger().set_print_to_stdout_flag(true);

    std::cout << "=== Agent-Zero TextualSensor Advanced Example ===" << std::endl;

    try {
        // Create AtomSpace
        AtomSpace* as = new AtomSpace();
        
        // Create and configure TextualSensor
        auto sensor = std::make_unique<TextualSensor>(as, "advanced_text_sensor", "demo_source");
        
        // Initialize and start
        if (!sensor->initialize() || !sensor->start()) {
            std::cerr << "Failed to initialize/start sensor" << std::endl;
            delete as;
            return 1;
        }
        
        std::cout << "TextualSensor initialized and started" << std::endl;
        std::cout << sensor->describe() << std::endl;

        // Run demonstrations
        demonstrate_text_processing(*sensor);
        demonstrate_streaming(*sensor);
        demonstrate_configuration(*sensor);
        demonstrate_atomspace_integration(*sensor, as);
        demonstrate_error_handling(*sensor);

        // Final status report
        std::cout << "\n=== Final Status Report ===" << std::endl;
        std::cout << sensor->get_status() << std::endl;
        
        // Cleanup
        sensor->stop();
        delete as;
        
        std::cout << "\nAdvanced example completed successfully" << std::endl;

    } catch (const std::exception& e) {
        std::cerr << "Exception: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}