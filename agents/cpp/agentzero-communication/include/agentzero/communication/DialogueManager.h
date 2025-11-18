/*
 * DialogueManager.h
 *
 * Copyright (C) 2024 Agent-Zero-Genesis Project
 * 
 * Conversational interaction management for Agent-Zero
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 */

#ifndef _AGENTZERO_DIALOGUE_MANAGER_H
#define _AGENTZERO_DIALOGUE_MANAGER_H

#include <string>
#include <vector>
#include <memory>

#include <opencog/atomspace/AtomSpace.h>

namespace agentzero {
namespace communication { 

/**
 * DialogueManager manages conversational interactions and context
 * 
 * TODO: Implementation planned for AZ-NLP-002
 */
class DialogueManager
{
private:
    opencog::AtomSpacePtr _atomspace;

public:
    explicit DialogueManager(opencog::AtomSpacePtr atomspace);
    ~DialogueManager() = default;
    
    // Placeholder methods - to be implemented in AZ-NLP-002
    std::string process_dialogue(const std::string& input);
    void reset_context();
};

} // namespace communication
} // namespace agentzero

#endif // _AGENTZERO_DIALOGUE_MANAGER_H