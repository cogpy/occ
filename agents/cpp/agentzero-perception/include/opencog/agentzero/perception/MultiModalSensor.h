/*
 * opencog/agentzero/perception/MultiModalSensor.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_MULTIMODAL_SENSOR_H
#define _OPENCOG_MULTIMODAL_SENSOR_H

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/sensory/SensoryNode.h>
#include <string>
#include <vector>
#include <memory>
#include <chrono>

namespace opencog { namespace agentzero {

/** \addtogroup grp_agentzero_perception
 *  @{
 */

/**
 * Sensory modality types supported by MultiModalSensor
 */
enum class SensorModality {
    VISUAL,     // Vision/camera input
    AUDITORY,   // Audio/microphone input  
    TEXTUAL,    // Text/linguistic input
    TACTILE,    // Touch/pressure sensors
    TEMPORAL,   // Time-based data streams
    SPATIAL,    // Location/position data
    CUSTOM      // User-defined modality
};

/**
 * Sensor data quality metrics
 */
struct SensorMetrics {
    double signal_strength;     // Signal strength (0.0 to 1.0)
    double noise_level;         // Noise level (0.0 to 1.0)
    double confidence;          // Data confidence (0.0 to 1.0)
    std::chrono::high_resolution_clock::time_point timestamp;
    size_t data_size;          // Size of raw data in bytes
};

/**
 * MultiModalSensor provides a unified interface for various sensory inputs
 * that integrates seamlessly with OpenCog's AtomSpace and sensory framework.
 * 
 * This class follows OpenCog architectural patterns:
 * - Uses AtomSpace for knowledge representation
 * - Integrates with existing sensory infrastructure 
 * - Supports ECAN attention allocation
 * - Provides proper error handling and monitoring
 *
 * Key Features:
 * - Multi-modal sensor fusion capabilities
 * - AtomSpace integration for semantic representation
 * - Attention-based processing prioritization
 * - Performance monitoring and quality metrics
 * - Extensible architecture for custom sensor types
 */
class MultiModalSensor
{
public:
    /**
     * Constructor
     * @param as AtomSpace instance for storing sensor data
     * @param sensor_id Unique identifier for this sensor
     * @param modality Primary modality of this sensor
     */
    MultiModalSensor(AtomSpace* as, 
                     const std::string& sensor_id,
                     SensorModality modality);

    /**
     * Virtual destructor for proper inheritance
     */
    virtual ~MultiModalSensor();

    // === Core Sensor Interface ===

    /**
     * Initialize the sensor and establish connections
     * @return true if initialization successful, false otherwise
     */
    virtual bool initialize() = 0;

    /**
     * Start sensor data acquisition
     * @return true if started successfully, false otherwise
     */
    virtual bool start() = 0;

    /**
     * Stop sensor data acquisition
     * @return true if stopped successfully, false otherwise
     */
    virtual bool stop() = 0;

    /**
     * Check if sensor is currently active
     * @return true if sensor is active, false otherwise
     */
    virtual bool is_active() const = 0;

    // === Data Acquisition ===

    /**
     * Read latest sensor data and convert to AtomSpace representation
     * @param silent If true, suppress status messages
     * @return Handle to AtomSpace representation of sensor data
     */
    virtual Handle read_data(bool silent = false) = 0;

    /**
     * Process raw sensor data into structured AtomSpace atoms
     * @param raw_data Raw sensor data as Value
     * @param silent If true, suppress processing messages
     * @return Handle to processed AtomSpace representation
     */
    virtual Handle process_data(const ValuePtr& raw_data, bool silent = false) = 0;

    // === Multi-Modal Integration ===

    /**
     * Fuse data from multiple modalities
     * @param sensor_data Vector of sensor data handles from different modalities
     * @param fusion_method Method for combining multi-modal data
     * @return Handle to fused multi-modal representation
     */
    virtual Handle fuse_modalities(const std::vector<Handle>& sensor_data,
                                   const std::string& fusion_method = "weighted_average");

    /**
     * Add a secondary modality to this sensor
     * @param modality Additional sensory modality to support
     * @return true if modality added successfully
     */
    virtual bool add_modality(SensorModality modality);

    /**
     * Get list of supported modalities for this sensor
     * @return Vector of supported sensor modalities
     */
    virtual std::vector<SensorModality> get_modalities() const;

    // === AtomSpace Integration ===

    /**
     * Create AtomSpace nodes and links for sensor configuration
     * @return Handle to sensor configuration atoms
     */
    virtual Handle create_sensor_atoms();

    /**
     * Update AtomSpace with latest sensor readings
     * @param data_handle Handle to sensor data atoms
     * @return true if update successful
     */
    virtual bool update_atomspace(const Handle& data_handle);

    /**
     * Get the SensoryNode representing this sensor in the AtomSpace
     * @return Handle to the sensor's SensoryNode
     */
    Handle get_sensor_node() const { return _sensor_node; }

    // === Attention Integration ===

    /**
     * Set attention value for sensor data based on importance
     * @param data_handle Handle to sensor data atoms
     * @param importance Importance value (0.0 to 1.0)
     * @param urgency Urgency value (0.0 to 1.0)  
     * @return true if attention values set successfully
     */
    virtual bool set_attention_values(const Handle& data_handle,
                                      double importance,
                                      double urgency);

    /**
     * Calculate dynamic attention allocation based on sensor metrics
     * @param metrics Current sensor quality metrics
     * @return Calculated attention value (0.0 to 1.0)
     */
    virtual double calculate_attention(const SensorMetrics& metrics);

    // === Monitoring and Diagnostics ===

    /**
     * Get current sensor performance metrics
     * @return Current sensor quality and performance metrics
     */
    virtual SensorMetrics get_metrics() const;

    /**
     * Get detailed sensor status information
     * @return Status string with sensor diagnostics
     */
    virtual std::string get_status() const;

    /**
     * Get sensor configuration description
     * @return Detailed description of sensor configuration
     */
    virtual std::string describe() const;

    // === Configuration ===

    /**
     * Set sensor configuration parameter
     * @param param_name Parameter name
     * @param param_value Parameter value  
     * @return true if parameter set successfully
     */
    virtual bool set_parameter(const std::string& param_name, 
                               const ValuePtr& param_value);

    /**
     * Get sensor configuration parameter
     * @param param_name Parameter name
     * @return Parameter value, or nullptr if not found
     */
    virtual ValuePtr get_parameter(const std::string& param_name) const;

    // === Getters ===
    const std::string& get_sensor_id() const { return _sensor_id; }
    SensorModality get_primary_modality() const { return _primary_modality; }
    AtomSpace* get_atomspace() const { return _atomspace; }

protected:
    // === Protected Members ===
    AtomSpace* _atomspace;              // AtomSpace for storing sensor data
    std::string _sensor_id;             // Unique sensor identifier
    SensorModality _primary_modality;   // Primary sensor modality
    std::vector<SensorModality> _modalities;  // All supported modalities
    Handle _sensor_node;                // SensoryNode in AtomSpace
    Handle _config_atoms;               // Configuration atoms
    SensorMetrics _metrics;             // Current sensor metrics
    
    // Configuration parameters
    std::map<std::string, ValuePtr> _parameters;
    
    // === Protected Helper Methods ===
    
    /**
     * Create modality-specific AtomSpace representation
     * @param modality Sensor modality
     * @param data Raw sensor data
     * @return Handle to modality-specific atoms
     */
    virtual Handle create_modality_atoms(SensorModality modality, 
                                         const ValuePtr& data);

    /**
     * Update sensor metrics based on latest data
     * @param data Latest sensor data
     */
    virtual void update_metrics(const ValuePtr& data);

    /**
     * Validate sensor configuration
     * @return true if configuration is valid
     */
    virtual bool validate_config();

    /**
     * Convert modality enum to string
     * @param modality Sensor modality
     * @return String representation of modality
     */
    static std::string modality_to_string(SensorModality modality);

    /**
     * Convert string to modality enum
     * @param modality_str String representation of modality
     * @return Sensor modality enum
     */
    static SensorModality string_to_modality(const std::string& modality_str);

private:
    // === Private Helper Methods ===
    
    /**
     * Calculate weighted confidence from multiple sensor data sources
     * @param sensor_data Vector of sensor data handles
     * @return Combined confidence value
     */
    double calculate_weighted_confidence(const std::vector<Handle>& sensor_data);
    
    /**
     * Calculate maximum confidence from multiple sensor data sources
     * @param sensor_data Vector of sensor data handles
     * @return Maximum confidence value
     */
    double calculate_max_confidence(const std::vector<Handle>& sensor_data);
    
    /**
     * Update sensor metrics atoms in AtomSpace
     */
    void update_sensor_metrics_atoms();
};

// Convenient type aliases
using MultiModalSensorPtr = std::shared_ptr<MultiModalSensor>;
using SensorVector = std::vector<MultiModalSensorPtr>;

/** @}*/
} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_MULTIMODAL_SENSOR_H