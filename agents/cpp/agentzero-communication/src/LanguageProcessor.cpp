/*
 * src/LanguageProcessor.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * LanguageProcessor Implementation
 * Handles natural language processing tasks with OpenCog integration
 * Part of the AGENT-ZERO-GENESIS project Phase 6: Communication & NLP
 */

#include <algorithm>
#include <sstream>
#include <regex>

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/util/Logger.h>

#include "opencog/agentzero/LanguageProcessor.h"

#ifdef HAVE_LG_ATOMESE
// Include lg-atomese headers when available
// #include <opencog/lg-atomese/LGAtomese.h>
#endif

#ifdef HAVE_LINK_GRAMMAR
// Include link-grammar headers when available
// #include <link-grammar/link-includes.h>
#endif

using namespace opencog;
using namespace opencog::agentzero;
using opencog::HandleSeq;

LanguageProcessor::LanguageProcessor(AtomSpacePtr atomspace)
    : _atomspace(atomspace)
    , _use_link_grammar(false)
    , _use_lg_atomese(false)
{
#ifdef HAVE_LG_ATOMESE
    _use_lg_atomese = true;
    logger().info("LanguageProcessor initialized with lg-atomese support");
#endif

#ifdef HAVE_LINK_GRAMMAR
    _use_link_grammar = true;
    logger().info("LanguageProcessor initialized with link-grammar support");
#endif

    if (!_use_lg_atomese && !_use_link_grammar) {
        logger().info("LanguageProcessor initialized with basic NLP only");
    }
}

LanguageProcessor::~LanguageProcessor() {
    logger().debug("LanguageProcessor destroyed");
}

ParseResult LanguageProcessor::parseText(const std::string& text) {
    ParseResult result;
    result.original_text = text;
    
    if (text.empty()) {
        result.success = false;
        return result;
    }
    
    try {
        // Basic parsing implementation
        // In a full implementation, this would use lg-atomese or link-grammar
        
        // For now, implement basic text analysis
        result.intent = detectIntent(text);
        result.detected_entities = extractEntities(text);
        
        // Create basic AtomSpace representation
        Handle text_atom = textToAtoms(text);
        if (text_atom != Handle::UNDEFINED) {
            result.parsed_atoms.push_back(text_atom);
        }
        
        // Simple confidence calculation based on text properties
        result.confidence = calculateConfidence(text);
        result.success = true;
        
    } catch (const std::exception& e) {
        logger().error("Error parsing text: %s", e.what());
        result.success = false;
        result.confidence = 0.0;
    }
    
    return result;
}

std::string LanguageProcessor::generateResponse(const std::string& input_text, const std::string& context) {
    if (input_text.empty()) {
        return "";
    }
    
    // Simple response generation
    // In a full implementation, this would use sophisticated NLP
    
    std::string input_lower = input_text;
    std::transform(input_lower.begin(), input_lower.end(), input_lower.begin(), ::tolower);
    
    // Context-aware responses
    if (!context.empty()) {
        std::string context_lower = context;
        std::transform(context_lower.begin(), context_lower.end(), context_lower.begin(), ::tolower);
        
        if (context_lower.find("topic:") != std::string::npos) {
            // Extract topic and provide topic-aware response
            size_t topic_start = context_lower.find("topic:") + 6;
            std::string topic = context.substr(topic_start);
            return "Regarding " + topic + ", I think " + generateTopicResponse(input_text, topic);
        }
    }
    
    // Intent-based responses
    std::string intent = detectIntent(input_text);
    
    if (intent == "greeting") {
        return "Hello! How can I assist you today?";
    } else if (intent == "question") {
        return generateQuestionResponse(input_text);
    } else if (intent == "farewell") {
        return "Goodbye! It was nice talking with you.";
    } else if (intent == "request") {
        return "I'll do my best to help you with that.";
    } else {
        return generateDefaultResponse(input_text);
    }
}

std::string LanguageProcessor::detectIntent(const std::string& text) {
    if (text.empty()) {
        return "unknown";
    }
    
    std::string text_lower = text;
    std::transform(text_lower.begin(), text_lower.end(), text_lower.begin(), ::tolower);
    
    // Simple rule-based intent detection
    if (std::regex_search(text_lower, std::regex(R"(\b(hello|hi|hey|greetings)\b)"))) {
        return "greeting";
    } else if (std::regex_search(text_lower, std::regex(R"(\b(bye|goodbye|farewell|see you)\b)"))) {
        return "farewell";
    } else if (text_lower.find("?") != std::string::npos || 
               std::regex_search(text_lower, std::regex(R"(\b(what|how|when|where|why|who|which)\b)"))) {
        return "question";
    } else if (std::regex_search(text_lower, std::regex(R"(\b(please|could you|can you|would you)\b)"))) {
        return "request";
    } else if (std::regex_search(text_lower, std::regex(R"(\b(thank|thanks|appreciate)\b)"))) {
        return "gratitude";
    } else {
        return "statement";
    }
}

std::vector<std::string> LanguageProcessor::extractEntities(const std::string& text) {
    std::vector<std::string> entities;
    
    // Simple entity extraction using regex patterns
    // In a full implementation, this would use NER (Named Entity Recognition)
    
    // Extract potential names (capitalized words)
    std::regex name_regex(R"(\b[A-Z][a-z]+\b)");
    std::sregex_iterator names_begin(text.begin(), text.end(), name_regex);
    std::sregex_iterator names_end;
    
    for (std::sregex_iterator i = names_begin; i != names_end; ++i) {
        entities.push_back(i->str());
    }
    
    // Extract potential numbers
    std::regex number_regex(R"(\b\d+\b)");
    std::sregex_iterator numbers_begin(text.begin(), text.end(), number_regex);
    std::sregex_iterator numbers_end;
    
    for (std::sregex_iterator i = numbers_begin; i != numbers_end; ++i) {
        entities.push_back("NUMBER:" + i->str());
    }
    
    return entities;
}

Handle LanguageProcessor::textToAtoms(const std::string& text) {
    if (text.empty()) {
        return Handle::UNDEFINED;
    }
    
    // Create a basic AtomSpace representation of the text
    Handle text_atom = _atomspace->add_node(CONCEPT_NODE, std::string("Text:" + text));
    
    // Add word nodes for each word
    std::istringstream iss(text);
    std::string word;
    std::vector<Handle> word_atoms;
    
    while (iss >> word) {
        // Remove punctuation for word atoms
        word.erase(std::remove_if(word.begin(), word.end(), 
                                 [](char c) { return !std::isalnum(c); }), word.end());
        
        if (!word.empty()) {
            Handle word_atom = _atomspace->add_node(CONCEPT_NODE, std::string("Word:" + word));
            word_atoms.push_back(word_atom);
            
            // Link word to text
            _atomspace->add_link(MEMBER_LINK, HandleSeq{word_atom, text_atom});
        }
    }
    
    // Create sequence link if we have multiple words
    if (word_atoms.size() > 1) {
        _atomspace->add_link(ORDERED_LINK, word_atoms);
    }
    
    return text_atom;
}

std::string LanguageProcessor::atomsToText(const std::vector<Handle>& atoms) {
    if (atoms.empty()) {
        return "";
    }
    
    std::stringstream ss;
    for (size_t i = 0; i < atoms.size(); ++i) {
        if (i > 0) ss << " ";
        
        // Extract text from atom name
        std::string atom_name = atoms[i]->get_name();
        if (atom_name.find("Text:") == 0) {
            ss << atom_name.substr(5); // Remove "Text:" prefix
        } else if (atom_name.find("Word:") == 0) {
            ss << atom_name.substr(5); // Remove "Word:" prefix
        } else {
            ss << atom_name;
        }
    }
    
    return ss.str();
}

void LanguageProcessor::setUseLinks(bool use_links) {
    _use_link_grammar = use_links && _use_link_grammar;
    logger().info("Link Grammar usage %s", _use_link_grammar ? "enabled" : "disabled");
}

void LanguageProcessor::setLanguageModel(const std::string& model_path) {
    _language_model = model_path;
    logger().info("Language model set to: %s", model_path.c_str());
}

// Private helper methods

double LanguageProcessor::calculateConfidence(const std::string& text) const {
    // Simple confidence calculation based on text properties
    double confidence = 0.5; // Base confidence
    
    // Increase confidence for longer, well-formed text
    if (text.length() > 10) confidence += 0.2;
    if (text.find(' ') != std::string::npos) confidence += 0.1; // Multiple words
    if (std::regex_search(text, std::regex(R"([.!?]$)"))) confidence += 0.1; // Proper ending
    
    // Decrease confidence for very short or unclear text
    if (text.length() < 3) confidence -= 0.3;
    if (std::regex_search(text, std::regex(R"(^\W+$)"))) confidence -= 0.4; // Only punctuation
    
    return std::max(0.0, std::min(1.0, confidence));
}

std::string LanguageProcessor::generateQuestionResponse(const std::string& question) const {
    std::string question_lower = question;
    std::transform(question_lower.begin(), question_lower.end(), question_lower.begin(), ::tolower);
    
    if (question_lower.find("what") != std::string::npos) {
        return "That's a good question about definitions or identity. Let me think about that.";
    } else if (question_lower.find("how") != std::string::npos) {
        return "That's asking about process or method. I can help explain that.";
    } else if (question_lower.find("why") != std::string::npos) {
        return "You're asking about reasons or causes. That's an important question.";
    } else if (question_lower.find("when") != std::string::npos) {
        return "That's a question about timing. Let me consider the temporal aspects.";
    } else if (question_lower.find("where") != std::string::npos) {
        return "You're asking about location or position. I'll try to help with that.";
    } else if (question_lower.find("who") != std::string::npos) {
        return "That's about identity or people involved. Let me think about who might be relevant.";
    } else {
        return "That's an interesting question. Could you provide more context?";
    }
}

std::string LanguageProcessor::generateTopicResponse(const std::string& input, const std::string& topic) const {
    return "this relates to our discussion about " + topic + ". What specifically would you like to know?";
}

std::string LanguageProcessor::generateDefaultResponse(const std::string& input) const {
    // Analyze input for appropriate response
    if (input.find("!") != std::string::npos) {
        return "I can sense your enthusiasm! Please tell me more.";
    } else if (input.length() > 100) {
        return "Thank you for that detailed information. Let me process what you've shared.";
    } else {
        return "I understand. Could you elaborate on that?";
    }
}