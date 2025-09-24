/*
 * src/ExperienceManager.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * ExperienceManager Implementation
 * Part of the Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 */

#include <algorithm>
#include <sstream>
#include <stdexcept>

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>

#include "opencog/agentzero/ExperienceManager.h"

using namespace opencog;
using namespace opencog::agentzero;

ExperienceManager::ExperienceManager(AtomSpacePtr atomspace)
    : _atomspace(atomspace)
    , _experience_base(Handle::UNDEFINED)
    , _experience_timeline(Handle::UNDEFINED)
    , _max_experiences(10000)
    , _enable_experience_compression(true)
{
    if (!_atomspace) {
        throw std::runtime_error("ExperienceManager requires valid AtomSpace");
    }

    logger().info() << "[ExperienceManager] Initializing experience management system";
    initializeExperienceBase();
    logger().info() << "[ExperienceManager] Experience management system initialized";
}

ExperienceManager::~ExperienceManager()
{
    logger().info() << "[ExperienceManager] Shutting down experience management system";
}

Handle ExperienceManager::recordExperience(ExperienceType type,
                                          Handle experience_data,
                                          const std::vector<Handle>& context_atoms)
{
    if (experience_data == Handle::UNDEFINED) {
        logger().error() << "[ExperienceManager] Cannot record undefined experience data";
        return Handle::UNDEFINED;
    }

    logger().debug() << "[ExperienceManager] Recording experience of type " << static_cast<int>(type);

    // Create experience atom
    Handle experience_atom = createExperienceAtom(type, experience_data, context_atoms);

    // Add to type-specific collection
    _experiences_by_type[type].push_back(experience_atom);

    // Add to timeline
    _atomspace->add_link(INHERITANCE_LINK, {experience_atom, _experience_timeline});

    // Manage memory if needed
    if (_enable_experience_compression && getTotalExperiences() > _max_experiences) {
        compressExperiences(_max_experiences * 0.8); // Keep 80% of max
    }

    logger().debug() << "[ExperienceManager] Experience recorded successfully";
    return experience_atom;
}

std::vector<Handle> ExperienceManager::getExperiencesByType(ExperienceType type, size_t limit) const
{
    auto it = _experiences_by_type.find(type);
    if (it == _experiences_by_type.end()) {
        return {};
    }

    const std::vector<Handle>& experiences = it->second;
    if (limit == 0 || limit >= experiences.size()) {
        return experiences;
    }

    // Return most recent experiences
    return std::vector<Handle>(experiences.end() - limit, experiences.end());
}

std::vector<Handle> ExperienceManager::getRecentExperiences(size_t count) const
{
    std::vector<Handle> all_experiences;
    
    // Collect all experiences
    for (const auto& type_pair : _experiences_by_type) {
        const std::vector<Handle>& experiences = type_pair.second;
        all_experiences.insert(all_experiences.end(), experiences.begin(), experiences.end());
    }

    // Sort by creation time (simplified - in practice would use actual timestamps)
    std::sort(all_experiences.begin(), all_experiences.end(),
              [](Handle a, Handle b) { return a.value() < b.value(); });

    // Return most recent
    if (count >= all_experiences.size()) {
        return all_experiences;
    }

    return std::vector<Handle>(all_experiences.end() - count, all_experiences.end());
}

std::vector<Handle> ExperienceManager::findSimilarExperiences(Handle reference_experience,
                                                             double similarity_threshold) const
{
    std::vector<Handle> similar_experiences;

    for (const auto& type_pair : _experiences_by_type) {
        for (Handle experience : type_pair.second) {
            if (experience != reference_experience) {
                double similarity = calculateExperienceSimilarity(reference_experience, experience);
                if (similarity >= similarity_threshold) {
                    similar_experiences.push_back(experience);
                }
            }
        }
    }

    // Sort by similarity (highest first)
    std::sort(similar_experiences.begin(), similar_experiences.end(),
              [this, reference_experience](Handle a, Handle b) {
                  return calculateExperienceSimilarity(reference_experience, a) >
                         calculateExperienceSimilarity(reference_experience, b);
              });

    return similar_experiences;
}

size_t ExperienceManager::getTotalExperiences() const
{
    size_t total = 0;
    for (const auto& type_pair : _experiences_by_type) {
        total += type_pair.second.size();
    }
    return total;
}

void ExperienceManager::compressExperiences(size_t keep_count)
{
    logger().info() << "[ExperienceManager] Compressing experiences, keeping " << keep_count;

    size_t total_experiences = getTotalExperiences();
    if (total_experiences <= keep_count) {
        return; // No compression needed
    }

    size_t to_remove = total_experiences - keep_count;
    size_t removed = 0;

    // Remove oldest experiences from each type proportionally
    for (auto& type_pair : _experiences_by_type) {
        std::vector<Handle>& experiences = type_pair.second;
        if (experiences.empty()) continue;

        // Calculate how many to remove from this type
        size_t type_remove = (to_remove * experiences.size()) / total_experiences;
        type_remove = std::min(type_remove, experiences.size());

        // Remove from beginning (oldest)
        experiences.erase(experiences.begin(), experiences.begin() + type_remove);
        removed += type_remove;
    }

    logger().info() << "[ExperienceManager] Compression complete, removed " << removed << " experiences";
}

void ExperienceManager::setMaxExperiences(size_t max_count)
{
    _max_experiences = max_count;
    logger().debug() << "[ExperienceManager] Maximum experiences set to " << max_count;

    // Compress if current count exceeds new limit
    if (getTotalExperiences() > _max_experiences) {
        compressExperiences(_max_experiences);
    }
}

// Private methods

void ExperienceManager::initializeExperienceBase()
{
    _experience_base = _atomspace->add_node(CONCEPT_NODE, "ExperienceBase");
    _experience_timeline = _atomspace->add_node(CONCEPT_NODE, "ExperienceTimeline");
    
    _atomspace->add_link(INHERITANCE_LINK, {_experience_timeline, _experience_base});
    
    logger().debug() << "[ExperienceManager] Experience base initialized in AtomSpace";
}

Handle ExperienceManager::createExperienceAtom(ExperienceType type,
                                              Handle data,
                                              const std::vector<Handle>& context)
{
    // Create unique experience name
    std::string experience_name = "Experience_" + std::to_string(data.value()) + "_" + 
                                 std::to_string(static_cast<int>(type));
    
    Handle experience_atom = _atomspace->add_node(CONCEPT_NODE, experience_name);

    // Link to experience base
    _atomspace->add_link(INHERITANCE_LINK, {experience_atom, _experience_base});

    // Add type information
    Handle type_node = _atomspace->add_node(CONCEPT_NODE, 
                                           "ExperienceType_" + std::to_string(static_cast<int>(type)));
    _atomspace->add_link(EVALUATION_LINK, {
        _atomspace->add_node(PREDICATE_NODE, "hasExperienceType"),
        _atomspace->add_link(LIST_LINK, {experience_atom, type_node})
    });

    // Link to experience data
    _atomspace->add_link(EVALUATION_LINK, {
        _atomspace->add_node(PREDICATE_NODE, "hasExperienceData"),
        _atomspace->add_link(LIST_LINK, {experience_atom, data})
    });

    // Add context information
    if (!context.empty()) {
        Handle context_link = _atomspace->add_link(LIST_LINK, context);
        _atomspace->add_link(EVALUATION_LINK, {
            _atomspace->add_node(PREDICATE_NODE, "hasExperienceContext"),
            _atomspace->add_link(LIST_LINK, {experience_atom, context_link})
        });
    }

    // Set initial truth value
    experience_atom->setTruthValue(SimpleTruthValue::createTV(1.0, 0.9));

    return experience_atom;
}

double ExperienceManager::calculateExperienceSimilarity(Handle exp1, Handle exp2) const
{
    if (exp1 == exp2) {
        return 1.0;
    }

    // Simplified similarity calculation
    // In practice, this would involve sophisticated comparison of:
    // - Experience types
    // - Experience data content
    // - Context similarity
    // - Temporal proximity
    
    // For now, return a basic similarity based on atom types and structure
    if (exp1->get_type() == exp2->get_type()) {
        return 0.5; // Same type gives base similarity
    }
    
    return 0.1; // Different types have low similarity
}