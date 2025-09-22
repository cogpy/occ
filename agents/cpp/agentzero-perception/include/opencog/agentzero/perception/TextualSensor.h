/*
 * opencog/agentzero/perception/TextualSensor.h
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

#ifndef _OPENCOG_TEXTUAL_SENSOR_H
#define _OPENCOG_TEXTUAL_SENSOR_H

#include <opencog/agentzero/perception/MultiModalSensor.h>
#include <opencog/atoms/sensory/OutputStream.h>
#include <queue>
#include <mutex>
#include <memory>

namespace opencog { namespace agentzero {

/** \addtogroup grp_agentzero_perception
 *  @{
 */

/**
 * TextualSensor provides a concrete implementation of MultiModalSensor
 * for processing textual input streams such as chat, file content, or
 * command-line input.
 *
 * This implementation demonstrates:
 * - Integration with OpenCog sensory framework
 * - Stream-based data processing
 * - AtomSpace representation of textual data
 * - Natural language processing integration points
 */
class TextualSensor : public MultiModalSensor
{
public:
    /**
     * Constructor
     * @param as AtomSpace instance for storing sensor data
     * @param sensor_id Unique identifier for this sensor
     * @param input_source Input source (file path, stream name, etc.)
     */
    TextualSensor(AtomSpace* as, 
                  const std::string& sensor_id,
                  const std::string& input_source);

    /**
     * Destructor
     */
    virtual ~TextualSensor();

    // === Core Sensor Interface Implementation ===

    bool initialize() override;
    bool start() override;
    bool stop() override;
    bool is_active() const override;

    // === Data Acquisition Implementation ===

    Handle read_data(bool silent = false) override;
    Handle process_data(const ValuePtr& raw_data, bool silent = false) override;

    // === Textual-Specific Methods ===

    /**
     * Process a single line of text input
     * @param text_line Input text line
     * @param silent If true, suppress processing messages
     * @return Handle to processed text atoms
     */
    virtual Handle process_text_line(const std::string& text_line, bool silent = false);

    /**
     * Set text processing mode
     * @param mode Processing mode ("words", "sentences", "documents", "stream")
     */
    virtual void set_text_mode(const std::string& mode);

    /**
     * Get current text processing mode
     * @return Current processing mode string
     */
    virtual std::string get_text_mode() const;

    /**
     * Add text input to the processing queue
     * @param text Text to add to queue
     * @return true if text added successfully
     */
    virtual bool add_text_input(const std::string& text);

    /**
     * Get number of queued text inputs
     * @return Number of items in input queue
     */
    virtual size_t get_queue_size() const;

protected:
    // === Protected Members ===
    std::string _input_source;          // Input source identifier
    std::string _text_mode;             // Text processing mode
    bool _is_active;                    // Sensor active state
    
    // Thread-safe input queue
    mutable std::mutex _queue_mutex;
    std::queue<std::string> _input_queue;
    
    // Stream handling
    std::unique_ptr<OutputStream> _output_stream;
    
    // === Protected Methods ===
    
    /**
     * Initialize input stream based on source type
     * @return true if stream initialized successfully
     */
    virtual bool initialize_stream();

    /**
     * Create AtomSpace representation for text data
     * @param text Input text
     * @param mode Processing mode
     * @return Handle to text atoms
     */
    virtual Handle create_text_atoms(const std::string& text, const std::string& mode);

    /**
     * Process text in "words" mode
     * @param text Input text
     * @return Handle to word-based atoms
     */
    virtual Handle process_words_mode(const std::string& text);

    /**
     * Process text in "sentences" mode  
     * @param text Input text
     * @return Handle to sentence-based atoms
     */
    virtual Handle process_sentences_mode(const std::string& text);

    /**
     * Process text in "documents" mode
     * @param text Input text  
     * @return Handle to document-based atoms
     */
    virtual Handle process_documents_mode(const std::string& text);

    /**
     * Process text in "stream" mode
     * @param text Input text
     * @return Handle to stream-based atoms
     */
    virtual Handle process_stream_mode(const std::string& text);

    /**
     * Calculate text quality metrics
     * @param text Input text
     * @return Updated sensor metrics
     */
    virtual SensorMetrics calculate_text_metrics(const std::string& text);

    /**
     * Validate text input
     * @param text Input text to validate
     * @return true if text is valid for processing
     */
    virtual bool validate_text_input(const std::string& text);
};

// Convenient type alias
using TextualSensorPtr = std::shared_ptr<TextualSensor>;

/** @}*/
} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_TEXTUAL_SENSOR_H