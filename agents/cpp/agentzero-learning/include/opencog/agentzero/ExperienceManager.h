/*
 * opencog/agentzero/ExperienceManager.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * ExperienceManager - Manages agent's experiential memory
 * Part of the Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 */

#ifndef _OPENCOG_AGENTZERO_EXPERIENCE_MANAGER_H
#define _OPENCOG_AGENTZERO_EXPERIENCE_MANAGER_H

#include <memory>
#include <string>
#include <vector>
#include <map>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/util/Logger.h>

namespace opencog {
namespace agentzero {

/**
 * ExperienceManager - Manages agent's experiential memory in AtomSpace
 *
 * This class handles the storage, retrieval, and organization of experiences
 * that agents accumulate during their operation. Experiences are stored in
 * the AtomSpace and can be used for learning, reflection, and skill acquisition.
 */
class ExperienceManager
{
public:
    /**
     * Types of experiences that can be managed
     */
    enum class ExperienceType {
        ACTION_OUTCOME,    // Results of actions taken
        OBSERVATION,       // Sensory observations
        INTERACTION,       // Social interactions
        PROBLEM_SOLVING,   // Problem-solving episodes
        SUCCESS,           // Successful task completions
        FAILURE,           // Failed attempts and errors
        DISCOVERY          // New knowledge discoveries
    };

private:
    AtomSpacePtr _atomspace;
    Handle _experience_base;
    Handle _experience_timeline;
    std::map<ExperienceType, std::vector<Handle>> _experiences_by_type;
    size_t _max_experiences;
    bool _enable_experience_compression;

public:
    /**
     * Constructor
     * @param atomspace AtomSpace for experience storage
     */
    explicit ExperienceManager(AtomSpacePtr atomspace);

    /**
     * Destructor
     */
    ~ExperienceManager();

    /**
     * Record a new experience
     * @param type Type of experience
     * @param experience_data Handle representing the experience
     * @param context_atoms Additional context information
     * @return Handle to the recorded experience
     */
    Handle recordExperience(ExperienceType type,
                           Handle experience_data,
                           const std::vector<Handle>& context_atoms = {});

    /**
     * Get experiences by type
     * @param type Type of experiences to retrieve
     * @param limit Maximum number to return (0 = no limit)
     * @return Vector of experience handles
     */
    std::vector<Handle> getExperiencesByType(ExperienceType type, size_t limit = 0) const;

    /**
     * Get recent experiences
     * @param count Number of recent experiences to retrieve
     * @return Vector of recent experience handles
     */
    std::vector<Handle> getRecentExperiences(size_t count) const;

    /**
     * Find similar experiences
     * @param reference_experience Experience to match against
     * @param similarity_threshold Minimum similarity score
     * @return Vector of similar experience handles
     */
    std::vector<Handle> findSimilarExperiences(Handle reference_experience,
                                              double similarity_threshold = 0.7) const;

    /**
     * Get total number of experiences
     * @return Total experience count
     */
    size_t getTotalExperiences() const;

    /**
     * Clear old experiences to manage memory
     * @param keep_count Number of experiences to keep
     */
    void compressExperiences(size_t keep_count);

    /**
     * Set maximum number of experiences to store
     * @param max_count Maximum experience count
     */
    void setMaxExperiences(size_t max_count);

private:
    void initializeExperienceBase();
    Handle createExperienceAtom(ExperienceType type, Handle data, const std::vector<Handle>& context);
    double calculateExperienceSimilarity(Handle exp1, Handle exp2) const;
};

} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_EXPERIENCE_MANAGER_H