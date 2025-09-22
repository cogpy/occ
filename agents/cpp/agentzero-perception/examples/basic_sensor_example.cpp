/*
 * basic_sensor_example.cpp
 *
 * Basic example demonstrating MultiModalSensor interface usage
 * Copyright (C) 2024 OpenCog Foundation
 */

#include <opencog/agentzero/perception/TextualSensor.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/util/Logger.h>
#include <iostream>
#include <memory>

using namespace opencog;
using namespace opencog::agentzero;

int main()
{
    // Configure logging
    logger().set_level(Logger::INFO);
    logger().set_print_to_stdout_flag(true);

    std::cout << "=== Agent-Zero MultiModalSensor Basic Example ===" << std::endl;

    try {
        // Create AtomSpace
        AtomSpace* as = new AtomSpace();
        std::cout << "Created AtomSpace" << std::endl;

        // Create TextualSensor
        auto sensor = std::make_shared<TextualSensor>(as, "example_sensor", "console_input");
        std::cout << "Created TextualSensor: " << sensor->get_sensor_id() << std::endl;

        // Initialize sensor
        if (!sensor->initialize()) {
            std::cerr << "Failed to initialize sensor" << std::endl;
            delete as;
            return 1;
        }
        std::cout << "Sensor initialized successfully" << std::endl;

        // Start sensor
        if (!sensor->start()) {
            std::cerr << "Failed to start sensor" << std::endl;
            delete as;
            return 1;
        }
        std::cout << "Sensor started successfully" << std::endl;

        // Test different text processing modes
        std::vector<std::string> test_modes = {"words", "sentences", "documents", "stream"};
        std::string test_text = "Hello world! This is a test sentence. How are you today?";

        for (const auto& mode : test_modes) {
            std::cout << "\n--- Testing mode: " << mode << " ---" << std::endl;
            
            sensor->set_text_mode(mode);
            sensor->add_text_input(test_text);
            
            Handle result = sensor->read_data(false);
            if (result != Handle::UNDEFINED) {
                std::cout << "Successfully processed text in " << mode << " mode" << std::endl;
                std::cout << "Result atom: " << result->to_short_string() << std::endl;
            } else {
                std::cout << "No data processed for " << mode << " mode" << std::endl;
            }

            // Show sensor metrics
            SensorMetrics metrics = sensor->get_metrics();
            std::cout << "Metrics - Signal: " << metrics.signal_strength 
                      << ", Noise: " << metrics.noise_level 
                      << ", Confidence: " << metrics.confidence << std::endl;
        }

        // Test multi-modality support
        std::cout << "\n--- Testing Multi-Modality ---" << std::endl;
        sensor->add_modality(SensorModality::TEMPORAL);
        
        auto modalities = sensor->get_modalities();
        std::cout << "Supported modalities: ";
        for (size_t i = 0; i < modalities.size(); ++i) {
            if (i > 0) std::cout << ", ";
            // Note: This would require implementing modality_to_string as public
            std::cout << static_cast<int>(modalities[i]);
        }
        std::cout << std::endl;

        // Test sensor fusion
        std::vector<Handle> sensor_data;
        sensor->add_text_input("First sensor input");
        Handle data1 = sensor->read_data(true);
        if (data1 != Handle::UNDEFINED) sensor_data.push_back(data1);
        
        sensor->add_text_input("Second sensor input");
        Handle data2 = sensor->read_data(true);
        if (data2 != Handle::UNDEFINED) sensor_data.push_back(data2);

        if (!sensor_data.empty()) {
            Handle fused = sensor->fuse_modalities(sensor_data, "weighted_average");
            if (fused != Handle::UNDEFINED) {
                std::cout << "Successfully fused " << sensor_data.size() << " data sources" << std::endl;
                std::cout << "Fused result: " << fused->to_short_string() << std::endl;
            }
        }

        // Display sensor status
        std::cout << "\n--- Sensor Status ---" << std::endl;
        std::cout << sensor->get_status() << std::endl;

        // Display sensor description  
        std::cout << "\n--- Sensor Description ---" << std::endl;
        std::cout << sensor->describe() << std::endl;

        // Stop sensor
        sensor->stop();
        std::cout << "\nSensor stopped" << std::endl;

        // Show AtomSpace contents
        std::cout << "\n--- AtomSpace Contents ---" << std::endl;
        std::cout << "Total atoms: " << as->get_size() << std::endl;

        // Cleanup
        delete as;
        std::cout << "\nExample completed successfully" << std::endl;

    } catch (const std::exception& e) {
        std::cerr << "Exception: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}