/**
 * SkillAcquisition.cpp
 *
 * Learns new capabilities through experience
 * Part of Agent-Zero Learning & Adaptation Phase 5
 *
 * Copyright (C) 2024 OpenCog Foundation
 */

#include "agentzero-learning/SkillAcquisition.h"
#include <opencog/util/Logger.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/atom_types/atom_types.h>

using namespace opencog;
using namespace opencog::agentzero;

SkillAcquisition::SkillAcquisition(AtomSpacePtr atomspace)
    : _atomspace(atomspace), _initialized(false)
{
    logger().info() << "[SkillAcquisition] Creating skill acquisition module";
}

SkillAcquisition::~SkillAcquisition()
{
    logger().info() << "[SkillAcquisition] Destroyed skill acquisition module";
}

bool SkillAcquisition::initialize()
{
    if (_initialized) {
        return true;
    }

    if (!_atomspace) {
        logger().error() << "[SkillAcquisition] AtomSpace is null";
        return false;
    }

    _initialized = true;
    logger().info() << "[SkillAcquisition] Skill acquisition initialized";
    return true;
}

std::vector<Handle> SkillAcquisition::learnSkills(const std::vector<Handle>& experiences)
{
    std::vector<Handle> learned_skills;
    
    if (!_initialized) {
        logger().error() << "[SkillAcquisition] Not initialized";
        return learned_skills;
    }

    try {
        // Simple skill learning implementation
        for (const auto& experience : experiences) {
            if (experience != Handle::UNDEFINED) {
                // Create a skill based on the experience
                Handle skill = _atomspace->add_node(CONCEPT_NODE, 
                    "learned_skill_from_" + experience->get_name());
                learned_skills.push_back(skill);
            }
        }

        logger().info() << "[SkillAcquisition] Learned " << learned_skills.size() << " skills";
    }
    catch (const std::exception& e) {
        logger().error() << "[SkillAcquisition] Error learning skills: " << e.what();
    }

    return learned_skills;
}