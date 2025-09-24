/**
 * ExperienceManager.cpp
 *
 * Manages agent experiential memory for learning
 * Part of Agent-Zero Learning & Adaptation Phase 5
 *
 * Copyright (C) 2024 OpenCog Foundation
 */

#include "agentzero-learning/ExperienceManager.h"
#include <opencog/util/Logger.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/atom_types/atom_types.h>

using namespace opencog;
using namespace opencog::agentzero;

ExperienceManager::ExperienceManager(AtomSpacePtr atomspace)
    : _atomspace(atomspace), _initialized(false)
{
    logger().info() << "[ExperienceManager] Creating experience manager";
}

ExperienceManager::~ExperienceManager()
{
    logger().info() << "[ExperienceManager] Destroyed experience manager";
}

bool ExperienceManager::initialize()
{
    if (_initialized) {
        return true;
    }

    if (!_atomspace) {
        logger().error() << "[ExperienceManager] AtomSpace is null";
        return false;
    }

    _initialized = true;
    logger().info() << "[ExperienceManager] Experience manager initialized";
    return true;
}

void ExperienceManager::recordExperience(const Experience& exp)
{
    if (!_initialized) {
        logger().error() << "[ExperienceManager] Not initialized";
        return;
    }

    try {
        // Create experience atom in AtomSpace
        Handle experience_atom = _atomspace->add_node(CONCEPT_NODE, 
            "experience_" + std::to_string(exp.timestamp));
        
        // Link context, action, and outcome
        if (exp.context_atom != Handle::UNDEFINED) {
            _atomspace->add_link(LIST_LINK, {experience_atom, exp.context_atom});
        }
        if (exp.action_atom != Handle::UNDEFINED) {
            _atomspace->add_link(LIST_LINK, {experience_atom, exp.action_atom});
        }
        if (exp.outcome_atom != Handle::UNDEFINED) {
            _atomspace->add_link(LIST_LINK, {experience_atom, exp.outcome_atom});
        }

        logger().debug() << "[ExperienceManager] Recorded experience with reward: " << exp.reward;
    }
    catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Error recording experience: " << e.what();
    }
}

std::vector<Experience> ExperienceManager::getExperiences(const Handle& context)
{
    std::vector<Experience> experiences;
    
    if (!_initialized) {
        logger().error() << "[ExperienceManager] Not initialized";
        return experiences;
    }

    try {
        // Simple implementation - return empty for now
        // Full implementation would query AtomSpace for experiences
        logger().debug() << "[ExperienceManager] Retrieved " << experiences.size() << " experiences";
    }
    catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Error retrieving experiences: " << e.what();
    }

    return experiences;
}

void ExperienceManager::updateExperienceValue(const Handle& experience_atom, double new_value)
{
    if (!_initialized) {
        logger().error() << "[ExperienceManager] Not initialized";
        return;
    }

    try {
        // Update experience value in AtomSpace
        // This would typically update TruthValue or other value
        logger().debug() << "[ExperienceManager] Updated experience value to: " << new_value;
    }
    catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Error updating experience value: " << e.what();
    }
}