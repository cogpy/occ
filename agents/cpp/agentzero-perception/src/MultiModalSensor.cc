/*
 * opencog/agentzero/perception/MultiModalSensor.cc
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

#include <opencog/agentzero/perception/MultiModalSensor.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/util/Logger.h>
#include <sstream>
#include <iomanip>

using namespace opencog;
using namespace opencog::agentzero;

// =============================================================================
// Constructor and Destructor
// =============================================================================

MultiModalSensor::MultiModalSensor(AtomSpace* as, 
                                   const std::string& sensor_id,
                                   SensorModality modality)
    : _atomspace(as), _sensor_id(sensor_id), _primary_modality(modality)
{
    if (!_atomspace) {
        throw std::invalid_argument("MultiModalSensor requires valid AtomSpace");
    }
    
    if (_sensor_id.empty()) {
        throw std::invalid_argument("MultiModalSensor requires non-empty sensor_id");
    }

    // Initialize with primary modality
    _modalities.push_back(_primary_modality);
    
    // Initialize metrics
    _metrics.signal_strength = 0.0;
    _metrics.noise_level = 0.0;
    _metrics.confidence = 0.0;
    _metrics.timestamp = std::chrono::high_resolution_clock::now();
    _metrics.data_size = 0;

    logger().info("MultiModalSensor created: id=%s, modality=%s", 
                  _sensor_id.c_str(), 
                  modality_to_string(_primary_modality).c_str());
}

MultiModalSensor::~MultiModalSensor()
{
    logger().info("MultiModalSensor destroyed: id=%s", _sensor_id.c_str());
}

// =============================================================================
// Multi-Modal Integration
// =============================================================================

Handle MultiModalSensor::fuse_modalities(const std::vector<Handle>& sensor_data,
                                          const std::string& fusion_method)
{
    if (sensor_data.empty()) {
        logger().warn("MultiModalSensor::fuse_modalities: No sensor data provided");
        return Handle::UNDEFINED;
    }

    logger().debug("Fusing %zu sensor modalities using method: %s", 
                   sensor_data.size(), fusion_method.c_str());

    // Create a fusion link to represent the combined sensor data
    HandleSeq fusion_outgoing;
    
    // Add fusion method as a concept node
    Handle fusion_concept = _atomspace->add_node(CONCEPT_NODE, "fusion:" + fusion_method);
    fusion_outgoing.push_back(fusion_concept);
    
    // Add all sensor data
    for (const Handle& data : sensor_data) {
        if (data != Handle::UNDEFINED) {
            fusion_outgoing.push_back(data);
        }
    }

    // Create the fused representation
    Handle fused_data = _atomspace->add_link(LIST_LINK, std::move(fusion_outgoing));

    // Calculate combined confidence based on fusion method
    double combined_confidence = 0.0;
    if (fusion_method == "weighted_average") {
        combined_confidence = calculate_weighted_confidence(sensor_data);
    } else if (fusion_method == "max_confidence") {
        combined_confidence = calculate_max_confidence(sensor_data);
    } else {
        combined_confidence = 0.5; // Default moderate confidence
    }

    // Set attention values based on combined confidence
    set_attention_values(fused_data, combined_confidence, combined_confidence);

    logger().debug("MultiModalSensor: Fused data created with confidence=%f", 
                   combined_confidence);

    return fused_data;
}

bool MultiModalSensor::add_modality(SensorModality modality)
{
    // Check if modality already exists
    for (const auto& existing : _modalities) {
        if (existing == modality) {
            logger().warn("MultiModalSensor: Modality %s already exists", 
                          modality_to_string(modality).c_str());
            return false;
        }
    }

    _modalities.push_back(modality);
    logger().info("MultiModalSensor: Added modality %s", 
                  modality_to_string(modality).c_str());
    return true;
}

std::vector<SensorModality> MultiModalSensor::get_modalities() const
{
    return _modalities;
}

// =============================================================================
// AtomSpace Integration
// =============================================================================

Handle MultiModalSensor::create_sensor_atoms()
{
    // Create the main sensor node
    std::string sensor_name = "sensor:" + _sensor_id;
    _sensor_node = _atomspace->add_node(SENSORY_NODE, std::move(sensor_name));

    // Create configuration atoms
    HandleSeq config_outgoing;
    config_outgoing.push_back(_sensor_node);

    // Add modality information
    for (const auto& modality : _modalities) {
        std::string modality_name = "modality:" + modality_to_string(modality);
        Handle modality_node = _atomspace->add_node(CONCEPT_NODE, std::move(modality_name));
        Handle modality_link = _atomspace->add_link(MEMBER_LINK, {_sensor_node, modality_node});
        config_outgoing.push_back(modality_link);
    }

    // Create sensor type information
    std::string sensor_type = "sensor_type:MultiModalSensor";
    Handle type_node = _atomspace->add_node(CONCEPT_NODE, std::move(sensor_type));
    Handle type_link = _atomspace->add_link(INHERITANCE_LINK, {_sensor_node, type_node});
    config_outgoing.push_back(type_link);

    // Create the configuration link
    _config_atoms = _atomspace->add_link(LIST_LINK, std::move(config_outgoing));

    logger().debug("MultiModalSensor: Created sensor atoms for %s", _sensor_id.c_str());
    return _config_atoms;
}

bool MultiModalSensor::update_atomspace(const Handle& data_handle)
{
    if (data_handle == Handle::UNDEFINED) {
        logger().warn("MultiModalSensor::update_atomspace: Invalid data handle");
        return false;
    }

    if (_sensor_node == Handle::UNDEFINED) {
        create_sensor_atoms();
    }

    // Create a timestamp link
    auto timestamp = std::chrono::duration_cast<std::chrono::milliseconds>(
        _metrics.timestamp.time_since_epoch()).count();
    
    Handle timestamp_node = _atomspace->add_node(NUMBER_NODE, std::to_string(timestamp));
    Handle timestamped_data = _atomspace->add_link(LIST_LINK, HandleSeq{data_handle, timestamp_node});

    // Link the data to the sensor
    Handle sensor_data_link = _atomspace->add_link(EVALUATION_LINK, HandleSeq{
        _atomspace->add_node(PREDICATE_NODE, "sensor_data"),
        _atomspace->add_link(LIST_LINK, HandleSeq{_sensor_node, timestamped_data})
    });

    // Update sensor metrics in AtomSpace
    update_sensor_metrics_atoms();

    logger().debug("MultiModalSensor: Updated AtomSpace with sensor data");
    return true;
}

// =============================================================================
// Attention Integration
// =============================================================================

bool MultiModalSensor::set_attention_values(const Handle& data_handle,
                                             double importance,
                                             double urgency)
{
    if (data_handle == Handle::UNDEFINED) {
        return false;
    }

    // Clamp values to valid range
    importance = std::max(0.0, std::min(1.0, importance));
    urgency = std::max(0.0, std::min(1.0, urgency));

    // Set attention value (simplified - in full implementation would use AttentionValue)
    ValuePtr attention_value = createFloatValue(std::vector<double>{importance, urgency});
    data_handle->setValue(_atomspace->add_node(CONCEPT_NODE, "attention"), attention_value);

    logger().debug("MultiModalSensor: Set attention values importance=%f, urgency=%f", 
                   importance, urgency);
    return true;
}

double MultiModalSensor::calculate_attention(const SensorMetrics& metrics)
{
    // Calculate attention based on signal quality and confidence
    double signal_factor = metrics.signal_strength * (1.0 - metrics.noise_level);
    double confidence_factor = metrics.confidence;
    
    // Weighted combination
    double attention = 0.6 * signal_factor + 0.4 * confidence_factor;
    
    // Ensure valid range
    attention = std::max(0.0, std::min(1.0, attention));
    
    logger().debug("MultiModalSensor: Calculated attention=%f (signal=%f, confidence=%f)", 
                   attention, signal_factor, confidence_factor);
    
    return attention;
}

// =============================================================================
// Monitoring and Diagnostics
// =============================================================================

SensorMetrics MultiModalSensor::get_metrics() const
{
    return _metrics;
}

std::string MultiModalSensor::get_status() const
{
    std::ostringstream status;
    status << "MultiModalSensor Status:\n";
    status << "  ID: " << _sensor_id << "\n";
    status << "  Primary Modality: " << modality_to_string(_primary_modality) << "\n";
    status << "  Active: " << (is_active() ? "Yes" : "No") << "\n";
    status << "  Modalities: ";
    for (size_t i = 0; i < _modalities.size(); ++i) {
        if (i > 0) status << ", ";
        status << modality_to_string(_modalities[i]);
    }
    status << "\n";
    status << "  Signal Strength: " << _metrics.signal_strength << "\n";
    status << "  Noise Level: " << _metrics.noise_level << "\n";
    status << "  Confidence: " << _metrics.confidence << "\n";
    status << "  Data Size: " << _metrics.data_size << " bytes\n";
    
    return status.str();
}

std::string MultiModalSensor::describe() const
{
    std::ostringstream desc;
    desc << "MultiModalSensor '" << _sensor_id << "' ";
    desc << "supporting " << _modalities.size() << " modality(ies): ";
    for (size_t i = 0; i < _modalities.size(); ++i) {
        if (i > 0) desc << ", ";
        desc << modality_to_string(_modalities[i]);
    }
    desc << ". Integrated with AtomSpace for semantic representation.";
    return desc.str();
}

// =============================================================================
// Configuration
// =============================================================================

bool MultiModalSensor::set_parameter(const std::string& param_name, 
                                      const ValuePtr& param_value)
{
    if (param_name.empty() || !param_value) {
        return false;
    }

    _parameters[param_name] = param_value;
    logger().debug("MultiModalSensor: Set parameter %s", param_name.c_str());
    return true;
}

ValuePtr MultiModalSensor::get_parameter(const std::string& param_name) const
{
    auto it = _parameters.find(param_name);
    if (it != _parameters.end()) {
        return it->second;
    }
    return nullptr;
}

// =============================================================================
// Protected Helper Methods
// =============================================================================

Handle MultiModalSensor::create_modality_atoms(SensorModality modality, 
                                                const ValuePtr& data)
{
    std::string modality_str = modality_to_string(modality);
    
    // Create modality-specific representation
    Handle modality_node = _atomspace->add_node(CONCEPT_NODE, "modality:" + modality_str);
    Handle data_node = _atomspace->add_node(CONCEPT_NODE, "data:" + _sensor_id + ":" + modality_str);
    
    // Attach the data value to the data node
    data_node->setValue(_atomspace->add_node(CONCEPT_NODE, "raw_data"), data);
    
    // Create the modality data link
    Handle modality_data = _atomspace->add_link(LIST_LINK, HandleSeq{modality_node, data_node});
    
    return modality_data;
}

void MultiModalSensor::update_metrics(const ValuePtr& data)
{
    _metrics.timestamp = std::chrono::high_resolution_clock::now();
    
    if (data) {
        // Estimate data size (simplified)
        _metrics.data_size = data->to_string().length();
        
        // Update confidence based on data validity
        _metrics.confidence = data ? 0.8 : 0.0;
        
        // Simulate signal strength (would be sensor-specific in real implementation)
        _metrics.signal_strength = 0.7 + 0.3 * _metrics.confidence;
        _metrics.noise_level = 0.1 + 0.1 * (1.0 - _metrics.confidence);
    }
}

bool MultiModalSensor::validate_config()
{
    if (!_atomspace) {
        logger().error("MultiModalSensor: Invalid AtomSpace");
        return false;
    }
    
    if (_sensor_id.empty()) {
        logger().error("MultiModalSensor: Empty sensor ID");
        return false;
    }
    
    if (_modalities.empty()) {
        logger().error("MultiModalSensor: No modalities configured");
        return false;
    }
    
    return true;
}

// =============================================================================
// Static Helper Methods
// =============================================================================

std::string MultiModalSensor::modality_to_string(SensorModality modality)
{
    switch (modality) {
        case SensorModality::VISUAL:    return "visual";
        case SensorModality::AUDITORY:  return "auditory";
        case SensorModality::TEXTUAL:   return "textual";
        case SensorModality::TACTILE:   return "tactile";
        case SensorModality::TEMPORAL:  return "temporal";
        case SensorModality::SPATIAL:   return "spatial";
        case SensorModality::CUSTOM:    return "custom";
        default:                        return "unknown";
    }
}

SensorModality MultiModalSensor::string_to_modality(const std::string& modality_str)
{
    if (modality_str == "visual")    return SensorModality::VISUAL;
    if (modality_str == "auditory")  return SensorModality::AUDITORY;
    if (modality_str == "textual")   return SensorModality::TEXTUAL;
    if (modality_str == "tactile")   return SensorModality::TACTILE;
    if (modality_str == "temporal")  return SensorModality::TEMPORAL;
    if (modality_str == "spatial")   return SensorModality::SPATIAL;
    if (modality_str == "custom")    return SensorModality::CUSTOM;
    return SensorModality::CUSTOM; // Default fallback
}

// =============================================================================
// Private Helper Methods
// =============================================================================

double MultiModalSensor::calculate_weighted_confidence(const std::vector<Handle>& sensor_data)
{
    if (sensor_data.empty()) return 0.0;
    
    double total_confidence = 0.0;
    double total_weight = 0.0;
    
    for (const Handle& data : sensor_data) {
        if (data != Handle::UNDEFINED) {
            // Get attention value as weight (simplified)
            ValuePtr attention = data->getValue(_atomspace->add_node(CONCEPT_NODE, "attention"));
            double weight = 1.0; // Default weight
            if (attention && attention->get_type() == FLOAT_VALUE) {
                auto float_val = FloatValueCast(attention);
                if (float_val && float_val->size() > 0) {
                    weight = float_val->value()[0];
                }
            }
            
            total_confidence += weight * 0.8; // Assume 80% base confidence
            total_weight += weight;
        }
    }
    
    return total_weight > 0.0 ? total_confidence / total_weight : 0.0;
}

double MultiModalSensor::calculate_max_confidence(const std::vector<Handle>& sensor_data)
{
    double max_confidence = 0.0;
    
    for (const Handle& data : sensor_data) {
        if (data != Handle::UNDEFINED) {
            // Get confidence from attention value (simplified)
            ValuePtr attention = data->getValue(_atomspace->add_node(CONCEPT_NODE, "attention"));
            double confidence = 0.5; // Default confidence
            if (attention && attention->get_type() == FLOAT_VALUE) {
                auto float_val = FloatValueCast(attention);
                if (float_val && float_val->size() > 0) {
                    confidence = float_val->value()[0];
                }
            }
            max_confidence = std::max(max_confidence, confidence);
        }
    }
    
    return max_confidence;
}

void MultiModalSensor::update_sensor_metrics_atoms()
{
    if (_sensor_node == Handle::UNDEFINED) return;
    
    // Create metrics as Values attached to the sensor node
    ValuePtr signal_value = createFloatValue(std::vector<double>{_metrics.signal_strength});
    ValuePtr noise_value = createFloatValue(std::vector<double>{_metrics.noise_level});
    ValuePtr confidence_value = createFloatValue(std::vector<double>{_metrics.confidence});
    
    _sensor_node->setValue(_atomspace->add_node(CONCEPT_NODE, "signal_strength"), signal_value);
    _sensor_node->setValue(_atomspace->add_node(CONCEPT_NODE, "noise_level"), noise_value);
    _sensor_node->setValue(_atomspace->add_node(CONCEPT_NODE, "confidence"), confidence_value);
}