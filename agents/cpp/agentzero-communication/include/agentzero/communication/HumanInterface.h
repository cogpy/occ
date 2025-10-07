/*
 * HumanInterface.h
 *
 * Copyright (C) 2024 Agent-Zero-Genesis Project
 * 
 * Human-agent interaction layer for Agent-Zero
 */

#ifndef _AGENTZERO_HUMAN_INTERFACE_H
#define _AGENTZERO_HUMAN_INTERFACE_H

#include <string>
#include <opencog/atomspace/AtomSpace.h>

namespace agentzero {
namespace communication {

/**
 * HumanInterface manages human-agent interactions
 * 
 * TODO: Implementation planned for AZ-HUMAN-001
 */
class HumanInterface
{
private:
    opencog::AtomSpacePtr _atomspace;

public:
    explicit HumanInterface(opencog::AtomSpacePtr atomspace);
    ~HumanInterface() = default;
    
    // Placeholder methods - to be implemented in AZ-HUMAN-001
    std::string process_human_input(const std::string& input);
    std::string generate_response(const std::string& context);
};

} // namespace communication
} // namespace agentzero

#endif // _AGENTZERO_HUMAN_INTERFACE_H