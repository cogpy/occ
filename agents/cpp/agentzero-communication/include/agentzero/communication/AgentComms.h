/*
 * AgentComms.h
 *
 * Copyright (C) 2024 Agent-Zero-Genesis Project
 * 
 * Inter-agent communication protocols for Agent-Zero
 */

#ifndef _AGENTZERO_AGENT_COMMS_H
#define _AGENTZERO_AGENT_COMMS_H

#include <string>
#include <opencog/atomspace/AtomSpace.h>

namespace agentzero {
namespace communication {

/**
 * AgentComms handles inter-agent communication protocols
 * 
 * TODO: Implementation planned for AZ-COMM-001
 */
class AgentComms
{
private:
    opencog::AtomSpacePtr _atomspace;

public:
    explicit AgentComms(opencog::AtomSpacePtr atomspace);
    ~AgentComms() = default;
    
    // Placeholder methods - to be implemented in AZ-COMM-001
    bool send_message(const std::string& agent_id, const std::string& message);
    std::vector<std::string> receive_messages();
};

} // namespace communication
} // namespace agentzero

#endif // _AGENTZERO_AGENT_COMMS_H