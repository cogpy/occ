/**
 * ExperienceManager.h
 *
 * Manages agent experiential memory for learning
 * Part of Agent-Zero Learning & Adaptation Phase 5
 *
 * Copyright (C) 2024 OpenCog Foundation
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 */

#ifndef _AGENTZERO_EXPERIENCE_MANAGER_H
#define _AGENTZERO_EXPERIENCE_MANAGER_H

#include <memory>
#include <vector>
#include <map>
#include <string>

#include <opencog/util/Logger.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog { namespace agentzero {

struct Experience {
    Handle context_atom;
    Handle action_atom;
    Handle outcome_atom;
    double reward;
    long timestamp;
    
    Experience() : reward(0.0), timestamp(0) {}
};

class ExperienceManager
{
public:
    explicit ExperienceManager(AtomSpacePtr atomspace);
    virtual ~ExperienceManager();

    bool initialize();
    
    void recordExperience(const Experience& exp);
    std::vector<Experience> getExperiences(const Handle& context = Handle::UNDEFINED);
    void updateExperienceValue(const Handle& experience_atom, double new_value);
    
protected:
    AtomSpacePtr _atomspace;
    bool _initialized;
};

} // namespace agentzero
} // namespace opencog

#endif // _AGENTZERO_EXPERIENCE_MANAGER_H