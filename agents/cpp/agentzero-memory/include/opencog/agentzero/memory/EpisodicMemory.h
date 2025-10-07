/*
 * opencog/agentzero/memory/EpisodicMemory.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * EpisodicMemory - Manages temporal sequences and experiences
 * Part of Agent-Zero Memory & Context Management module
 * Part of the AGENT-ZERO-GENESIS project - AZ-MEM-001
 */

#ifndef _OPENCOG_AGENTZERO_MEMORY_EPISODIC_MEMORY_H
#define _OPENCOG_AGENTZERO_MEMORY_EPISODIC_MEMORY_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include "MemoryTypes.h"

namespace opencog {
namespace agentzero {
namespace memory {

/**
 * EpisodicMemory - Manages temporal sequences and experiences
 * 
 * Placeholder implementation for AZ-MEM-001
 * This will be implemented in a future task
 */
class EpisodicMemory
{
private:
    AtomSpacePtr _atomspace;

public:
    explicit EpisodicMemory(AtomSpacePtr atomspace) : _atomspace(atomspace) {}
    ~EpisodicMemory() = default;
    
    bool initialize() { return true; }
    bool shutdown() { return true; }
};

} // namespace memory
} // namespace agentzero  
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_MEMORY_EPISODIC_MEMORY_H