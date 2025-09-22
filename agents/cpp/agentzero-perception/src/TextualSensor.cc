/*
 * opencog/agentzero/perception/TextualSensor.cc
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

#include <opencog/agentzero/perception/TextualSensor.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/util/Logger.h>
#include <sstream>
#include <algorithm>
#include <cctype>
#include <thread>
#include <chrono>
#include <iomanip>

using namespace opencog;
using namespace opencog::agentzero;

// =============================================================================
// Constructor and Destructor
// =============================================================================

TextualSensor::TextualSensor(AtomSpace* as, 
                             const std::string& sensor_id,
                             const std::string& input_source)
    : MultiModalSensor(as, sensor_id, SensorModality::TEXTUAL),
      _input_source(input_source),
      _text_mode("sentences"),
      _is_active(false)
{
    logger().info("TextualSensor created: id=%s, source=%s", 
                  sensor_id.c_str(), input_source.c_str());
}

TextualSensor::~TextualSensor()
{
    stop();
    logger().info("TextualSensor destroyed: id=%s", _sensor_id.c_str());
}

// =============================================================================
// Core Sensor Interface Implementation
// =============================================================================

bool TextualSensor::initialize()
{
    if (!validate_config()) {
        logger().error("TextualSensor::initialize: Invalid configuration");
        return false;
    }

    // Create sensor atoms in AtomSpace
    create_sensor_atoms();

    // Initialize the input stream
    if (!initialize_stream()) {
        logger().error("TextualSensor::initialize: Failed to initialize stream");
        return false;
    }

    // Set default parameters
    set_parameter("buffer_size", createFloatValue(std::vector<double>{1024.0}));
    set_parameter("max_line_length", createFloatValue(std::vector<double>{4096.0}));
    set_parameter("encoding", createStringValue(std::vector<std::string>{"utf-8"}));

    logger().info("TextualSensor initialized: %s", _sensor_id.c_str());
    return true;
}

bool TextualSensor::start()
{
    if (_is_active) {
        logger().warn("TextualSensor::start: Already active");
        return true;
    }

    _is_active = true;
    logger().info("TextualSensor started: %s", _sensor_id.c_str());
    return true;
}

bool TextualSensor::stop()
{
    if (!_is_active) {
        return true;
    }

    _is_active = false;
    
    // Clear input queue
    {
        std::lock_guard<std::mutex> lock(_queue_mutex);
        while (!_input_queue.empty()) {
            _input_queue.pop();
        }
    }

    logger().info("TextualSensor stopped: %s", _sensor_id.c_str());
    return true;
}

bool TextualSensor::is_active() const
{
    return _is_active;
}

// =============================================================================
// Data Acquisition Implementation
// =============================================================================

Handle TextualSensor::read_data(bool silent)
{
    if (!_is_active) {
        if (!silent) {
            logger().warn("TextualSensor::read_data: Sensor not active");
        }
        return Handle::UNDEFINED;
    }

    std::string text_data;
    {
        std::lock_guard<std::mutex> lock(_queue_mutex);
        if (_input_queue.empty()) {
            if (!silent) {
                logger().debug("TextualSensor::read_data: No data available");
            }
            return Handle::UNDEFINED;
        }
        
        text_data = _input_queue.front();
        _input_queue.pop();
    }

    // Create string value for the raw text data
    ValuePtr raw_data = createStringValue(std::vector<std::string>{text_data});
    
    // Process the data and return the result
    Handle processed_data = process_data(raw_data, silent);
    
    // Update AtomSpace with the new data
    if (processed_data != Handle::UNDEFINED) {
        update_atomspace(processed_data);
    }

    if (!silent) {
        logger().debug("TextualSensor::read_data: Processed text of length %zu", 
                       text_data.length());
    }

    return processed_data;
}

Handle TextualSensor::process_data(const ValuePtr& raw_data, bool silent)
{
    if (!raw_data || raw_data->get_type() != STRING_VALUE) {
        if (!silent) {
            logger().error("TextualSensor::process_data: Invalid raw data");
        }
        return Handle::UNDEFINED;
    }

    auto string_val = StringValueCast(raw_data);
    if (!string_val || string_val->size() == 0) {
        if (!silent) {
            logger().error("TextualSensor::process_data: Empty string data");
        }
        return Handle::UNDEFINED;
    }

    std::string text = string_val->value()[0];
    
    // Validate text input
    if (!validate_text_input(text)) {
        if (!silent) {
            logger().warn("TextualSensor::process_data: Invalid text input");
        }
        return Handle::UNDEFINED;
    }

    // Update metrics
    _metrics = calculate_text_metrics(text);
    update_metrics(raw_data);

    // Create AtomSpace representation
    Handle processed_atoms = create_text_atoms(text, _text_mode);

    // Set attention values based on text quality
    double attention = calculate_attention(_metrics);
    set_attention_values(processed_atoms, attention, attention * 0.8);

    if (!silent) {
        logger().debug("TextualSensor::process_data: Created atoms for text mode '%s'", 
                       _text_mode.c_str());
    }

    return processed_atoms;
}

// =============================================================================
// Textual-Specific Methods
// =============================================================================

Handle TextualSensor::process_text_line(const std::string& text_line, bool silent)
{
    ValuePtr text_value = createStringValue(std::vector<std::string>{text_line});
    return process_data(text_value, silent);
}

void TextualSensor::set_text_mode(const std::string& mode)
{
    if (mode == "words" || mode == "sentences" || mode == "documents" || mode == "stream") {
        _text_mode = mode;
        logger().info("TextualSensor: Set text mode to '%s'", mode.c_str());
    } else {
        logger().warn("TextualSensor: Invalid text mode '%s', keeping '%s'", 
                      mode.c_str(), _text_mode.c_str());
    }
}

std::string TextualSensor::get_text_mode() const
{
    return _text_mode;
}

bool TextualSensor::add_text_input(const std::string& text)
{
    if (text.empty()) {
        return false;
    }

    {
        std::lock_guard<std::mutex> lock(_queue_mutex);
        _input_queue.push(text);
    }

    logger().debug("TextualSensor: Added text input of length %zu", text.length());
    return true;
}

size_t TextualSensor::get_queue_size() const
{
    std::lock_guard<std::mutex> lock(_queue_mutex);
    return _input_queue.size();
}

// =============================================================================
// Protected Methods
// =============================================================================

bool TextualSensor::initialize_stream()
{
    // For now, we don't create an actual stream connection
    // In a full implementation, this would connect to files, sockets, etc.
    logger().debug("TextualSensor: Stream initialized for source '%s'", 
                   _input_source.c_str());
    return true;
}

Handle TextualSensor::create_text_atoms(const std::string& text, const std::string& mode)
{
    if (mode == "words") {
        return process_words_mode(text);
    } else if (mode == "sentences") {
        return process_sentences_mode(text);
    } else if (mode == "documents") {
        return process_documents_mode(text);
    } else if (mode == "stream") {
        return process_stream_mode(text);
    } else {
        logger().warn("TextualSensor: Unknown text mode '%s', using sentences", mode.c_str());
        return process_sentences_mode(text);
    }
}

Handle TextualSensor::process_words_mode(const std::string& text)
{
    HandleSeq word_atoms;
    std::istringstream iss(text);
    std::string word;
    
    while (iss >> word) {
        // Clean word of punctuation (simplified)
        word.erase(std::remove_if(word.begin(), word.end(), 
                                  [](char c) { return std::ispunct(c); }), 
                   word.end());
        
        if (!word.empty()) {
            Handle word_node = _atomspace->add_node(CONCEPT_NODE, "word:" + word);
            word_atoms.push_back(word_node);
        }
    }

    if (word_atoms.empty()) {
        return Handle::UNDEFINED;
    }

    // Create a word list
    Handle word_list = _atomspace->add_link(LIST_LINK, std::move(word_atoms));
    
    // Link to sensor
    Handle sensor_word_data = _atomspace->add_link(EVALUATION_LINK, HandleSeq{
        _atomspace->add_node(PREDICATE_NODE, "textual_words"),
        _atomspace->add_link(LIST_LINK, HandleSeq{get_sensor_node(), word_list})
    });

    logger().debug("TextualSensor: Created %zu word atoms", word_atoms.size());
    return sensor_word_data;
}

Handle TextualSensor::process_sentences_mode(const std::string& text)
{
    HandleSeq sentence_atoms;
    
    // Simple sentence splitting on periods, exclamation marks, question marks
    std::string sentence;
    for (char c : text) {
        sentence += c;
        if (c == '.' || c == '!' || c == '?') {
            // Trim whitespace
            sentence.erase(0, sentence.find_first_not_of(" \t\n\r"));
            sentence.erase(sentence.find_last_not_of(" \t\n\r") + 1);
            
            if (!sentence.empty()) {
                Handle sentence_node = _atomspace->add_node(CONCEPT_NODE, "sentence:" + sentence);
                sentence_atoms.push_back(sentence_node);
            }
            sentence.clear();
        }
    }

    // Handle remaining text as a sentence if it doesn't end with punctuation
    if (!sentence.empty()) {
        sentence.erase(0, sentence.find_first_not_of(" \t\n\r"));
        sentence.erase(sentence.find_last_not_of(" \t\n\r") + 1);
        if (!sentence.empty()) {
            Handle sentence_node = _atomspace->add_node(CONCEPT_NODE, "sentence:" + sentence);
            sentence_atoms.push_back(sentence_node);
        }
    }

    if (sentence_atoms.empty()) {
        return Handle::UNDEFINED;
    }

    // Create a sentence list
    Handle sentence_list = _atomspace->add_link(LIST_LINK, std::move(sentence_atoms));
    
    // Link to sensor
    Handle sensor_sentence_data = _atomspace->add_link(EVALUATION_LINK, HandleSeq{
        _atomspace->add_node(PREDICATE_NODE, "textual_sentences"),
        _atomspace->add_link(LIST_LINK, HandleSeq{get_sensor_node(), sentence_list})
    });

    logger().debug("TextualSensor: Created %zu sentence atoms", sentence_atoms.size());
    return sensor_sentence_data;
}

Handle TextualSensor::process_documents_mode(const std::string& text)
{
    // Treat entire text as a document
    Handle document_node = _atomspace->add_node(CONCEPT_NODE, "document:" + _sensor_id);
    
    // Attach text content as a value
    ValuePtr text_content = createStringValue(std::vector<std::string>{text});
    document_node->setValue(_atomspace->add_node(CONCEPT_NODE, "content"), text_content);
    
    // Link to sensor
    Handle sensor_document_data = _atomspace->add_link(EVALUATION_LINK, HandleSeq{
        _atomspace->add_node(PREDICATE_NODE, "textual_document"),
        _atomspace->add_link(LIST_LINK, HandleSeq{get_sensor_node(), document_node})
    });

    logger().debug("TextualSensor: Created document atom with %zu characters", text.length());
    return sensor_document_data;
}

Handle TextualSensor::process_stream_mode(const std::string& text)
{
    // Create a stream chunk node
    auto timestamp = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::high_resolution_clock::now().time_since_epoch()).count();
    
    std::string chunk_id = "stream_chunk:" + _sensor_id + ":" + std::to_string(timestamp);
    Handle chunk_node = _atomspace->add_node(CONCEPT_NODE, std::move(chunk_id));
    
    // Attach text content and metadata
    ValuePtr text_content = createStringValue(std::vector<std::string>{text});
    ValuePtr timestamp_value = createFloatValue(std::vector<double>{static_cast<double>(timestamp)});
    
    chunk_node->setValue(_atomspace->add_node(CONCEPT_NODE, "content"), text_content);
    chunk_node->setValue(_atomspace->add_node(CONCEPT_NODE, "timestamp"), timestamp_value);
    
    // Link to sensor
    Handle sensor_stream_data = _atomspace->add_link(EVALUATION_LINK, HandleSeq{
        _atomspace->add_node(PREDICATE_NODE, "textual_stream"),
        _atomspace->add_link(LIST_LINK, HandleSeq{get_sensor_node(), chunk_node})
    });

    logger().debug("TextualSensor: Created stream chunk with %zu characters", text.length());
    return sensor_stream_data;
}

SensorMetrics TextualSensor::calculate_text_metrics(const std::string& text)
{
    SensorMetrics metrics = _metrics; // Start with current metrics
    
    metrics.timestamp = std::chrono::high_resolution_clock::now();
    metrics.data_size = text.length();
    
    // Calculate signal strength based on text length and content
    if (text.empty()) {
        metrics.signal_strength = 0.0;
        metrics.confidence = 0.0;
    } else {
        // Signal strength based on text length (normalized)
        double length_factor = std::min(1.0, text.length() / 100.0);
        
        // Count alphabetic characters as quality indicator
        size_t alpha_count = std::count_if(text.begin(), text.end(), 
                                           [](char c) { return std::isalpha(c); });
        double alpha_ratio = text.empty() ? 0.0 : static_cast<double>(alpha_count) / text.length();
        
        metrics.signal_strength = 0.5 * length_factor + 0.5 * alpha_ratio;
        
        // Confidence based on character variety and structure
        metrics.confidence = alpha_ratio * 0.8 + 0.2; // Base confidence of 0.2
        
        // Noise level inversely related to alphabetic content
        metrics.noise_level = 1.0 - alpha_ratio;
    }
    
    // Clamp values to valid ranges
    metrics.signal_strength = std::max(0.0, std::min(1.0, metrics.signal_strength));
    metrics.confidence = std::max(0.0, std::min(1.0, metrics.confidence));
    metrics.noise_level = std::max(0.0, std::min(1.0, metrics.noise_level));
    
    return metrics;
}

bool TextualSensor::validate_text_input(const std::string& text)
{
    // Basic validation
    if (text.empty()) {
        return false;
    }

    // Check for maximum line length parameter
    ValuePtr max_length_param = get_parameter("max_line_length");
    if (max_length_param && max_length_param->get_type() == FLOAT_VALUE) {
        auto float_val = FloatValueCast(max_length_param);
        if (float_val && float_val->size() > 0) {
            double max_length = float_val->value()[0];
            if (text.length() > static_cast<size_t>(max_length)) {
                logger().warn("TextualSensor: Text length %zu exceeds maximum %f", 
                              text.length(), max_length);
                return false;
            }
        }
    }

    // Check for valid UTF-8 encoding (simplified check)
    for (char c : text) {
        if (static_cast<unsigned char>(c) > 127) {
            // Non-ASCII character - in full implementation would validate UTF-8
            break;
        }
    }

    return true;
}