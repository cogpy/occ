/**
 * SkillAcquisition.h
 *
 * Learns new capabilities through experience
 * Part of Agent-Zero Learning & Adaptation Phase 5
 *
 * Copyright (C) 2024 OpenCog Foundation
 */

#ifndef _AGENTZERO_SKILL_ACQUISITION_H
#define _AGENTZERO_SKILL_ACQUISITION_H

#include <memory>
#include <vector>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog { namespace agentzero {

class SkillAcquisition
{
public:
    explicit SkillAcquisition(AtomSpacePtr atomspace);
    virtual ~SkillAcquisition();

    bool initialize();
    std::vector<Handle> learnSkills(const std::vector<Handle>& experiences);
    
protected:
    AtomSpacePtr _atomspace;
    bool _initialized;
};

} // namespace agentzero
} // namespace opencog

#endif // _AGENTZERO_SKILL_ACQUISITION_H