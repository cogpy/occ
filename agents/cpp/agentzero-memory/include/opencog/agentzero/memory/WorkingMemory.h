/*
 * opencog/agentzero/memory/WorkingMemory.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * WorkingMemory - Active context and short-term memory
 * Part of Agent-Zero Memory & Context Management module
 * Part of the AGENT-ZERO-GENESIS project - AZ-MEM-002
 */

#ifndef _OPENCOG_AGENTZERO_MEMORY_WORKING_MEMORY_H
#define _OPENCOG_AGENTZERO_MEMORY_WORKING_MEMORY_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include "MemoryTypes.h"

namespace opencog {
namespace agentzero {
namespace memory {

/**
 * WorkingMemory - Active context and short-term memory
 * 
 * Placeholder implementation for AZ-MEM-002
 * This will be implemented in a future task
 */
class WorkingMemory
{
private:
    AtomSpacePtr _atomspace;

public:
    explicit WorkingMemory(AtomSpacePtr atomspace) : _atomspace(atomspace) {}
    ~WorkingMemory() = default;
    
    bool initialize() { return true; }
    bool shutdown() { return true; }
};

} // namespace memory
} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_MEMORY_WORKING_MEMORY_H