/*
 * opencog/agentzero/SkillAcquisition.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * SkillAcquisition - Learning new capabilities through experience
 * Core learning component of Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 */

#ifndef _OPENCOG_AGENTZERO_SKILL_ACQUISITION_H
#define _OPENCOG_AGENTZERO_SKILL_ACQUISITION_H

#include <memory>
#include <string>
#include <vector>
#include <map>
#include <functional>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/util/Logger.h>

namespace opencog {
namespace agentzero {

// Forward declarations
class ExperienceManager;
class PolicyOptimizer;
class MetaLearning;

/**
 * SkillAcquisition - Core framework for learning new capabilities
 *
 * This class implements the core skill acquisition mechanism for Agent-Zero,
 * enabling agents to learn new capabilities through experience and practice.
 * It integrates with OpenCog's AtomSpace for knowledge representation and
 * uses various learning strategies to acquire and refine skills.
 *
 * Key Features:
 * - Experience-based skill learning
 * - Incremental skill refinement
 * - Skill transfer and generalization
 * - Integration with MOSES for optimization
 * - AtomSpace representation of skills
 * - Performance monitoring and adaptation
 */
class SkillAcquisition
{
public:
    /**
     * Skill types supported by the acquisition framework
     */
    enum class SkillType {
        PROCEDURAL,     // Step-by-step procedures
        COGNITIVE,      // Mental reasoning patterns
        PERCEPTUAL,     // Perception and recognition skills
        MOTOR,          // Physical action sequences
        SOCIAL,         // Interaction and communication skills
        CREATIVE,       // Creative problem-solving abilities
        ADAPTIVE        // Context-adaptive behaviors
    };

    /**
     * Learning strategies for skill acquisition
     */
    enum class LearningStrategy {
        IMITATION,      // Learn by copying observed behaviors
        REINFORCEMENT,  // Learn through reward-based feedback
        EXPLORATORY,    // Learn through systematic exploration
        TRANSFER,       // Learn by transferring from similar skills
        COMPOSITIONAL,  // Learn by combining existing skills
        REFLECTIVE,     // Learn through self-reflection and analysis
        COLLABORATIVE   // Learn through interaction with others
    };

    /**
     * Skill proficiency levels
     */
    enum class ProficiencyLevel {
        NOVICE = 0,
        BEGINNER = 25,
        INTERMEDIATE = 50,
        ADVANCED = 75,
        EXPERT = 100
    };

private:
    AtomSpacePtr _atomspace;
    std::unique_ptr<ExperienceManager> _experience_manager;
    std::unique_ptr<PolicyOptimizer> _policy_optimizer;
    std::unique_ptr<MetaLearning> _meta_learning;

    // Core skill representation
    Handle _skill_base;
    Handle _skill_hierarchy;
    std::map<std::string, Handle> _skill_registry;
    std::map<Handle, SkillType> _skill_types;
    std::map<Handle, ProficiencyLevel> _skill_proficiency;

    // Learning configuration
    bool _enable_meta_learning;
    bool _enable_skill_transfer;
    bool _enable_incremental_learning;
    double _learning_rate;
    size_t _max_skill_complexity;

    // Performance tracking
    std::map<Handle, std::vector<double>> _skill_performance_history;
    std::map<Handle, size_t> _skill_practice_counts;
    std::map<Handle, double> _skill_confidence_scores;

public:
    /**
     * Constructor
     * @param atomspace AtomSpace for knowledge representation
     */
    explicit SkillAcquisition(AtomSpacePtr atomspace);

    /**
     * Destructor
     */
    ~SkillAcquisition();

    // Core skill acquisition methods
    /**
     * Learn a new skill from experience
     * @param skill_name Name of the skill to learn
     * @param skill_type Type of skill being learned
     * @param strategy Learning strategy to use
     * @param experience_data Experiential data for learning
     * @return Handle to the learned skill atom
     */
    Handle learnSkill(const std::string& skill_name,
                     SkillType skill_type,
                     LearningStrategy strategy,
                     const std::vector<Handle>& experience_data);

    /**
     * Practice and refine an existing skill
     * @param skill_handle Handle to the skill atom
     * @param practice_data New practice experiences
     * @return Updated proficiency level
     */
    ProficiencyLevel practiceSkill(Handle skill_handle,
                                  const std::vector<Handle>& practice_data);

    /**
     * Apply a learned skill to a task
     * @param skill_handle Handle to the skill atom
     * @param task_context Context atoms for task execution
     * @param parameters Skill execution parameters
     * @return Success indication and result atoms
     */
    std::pair<bool, std::vector<Handle>> applySkill(Handle skill_handle,
                                                   const std::vector<Handle>& task_context,
                                                   const std::map<std::string, ValuePtr>& parameters = {});

    /**
     * Transfer learning from one skill to another
     * @param source_skill Handle to source skill
     * @param target_skill_name Name of target skill
     * @param adaptation_rules Rules for skill adaptation
     * @return Handle to the new transferred skill
     */
    Handle transferSkill(Handle source_skill,
                        const std::string& target_skill_name,
                        const std::vector<Handle>& adaptation_rules);

    // Skill management and query methods
    /**
     * Get all learned skills
     * @return Vector of skill handles
     */
    std::vector<Handle> getLearnedSkills() const;

    /**
     * Get skills of a specific type
     * @param skill_type Type of skills to retrieve
     * @return Vector of matching skill handles
     */
    std::vector<Handle> getSkillsByType(SkillType skill_type) const;

    /**
     * Get skill proficiency level
     * @param skill_handle Handle to the skill atom
     * @return Current proficiency level
     */
    ProficiencyLevel getSkillProficiency(Handle skill_handle) const;

    /**
     * Get skill performance history
     * @param skill_handle Handle to the skill atom
     * @return Vector of historical performance scores
     */
    std::vector<double> getSkillPerformanceHistory(Handle skill_handle) const;

    /**
     * Check if a skill exists
     * @param skill_name Name of the skill
     * @return True if skill exists
     */
    bool hasSkill(const std::string& skill_name) const;

    /**
     * Get skill by name
     * @param skill_name Name of the skill
     * @return Handle to the skill atom, or UNDEFINED if not found
     */
    Handle getSkill(const std::string& skill_name) const;

    // Configuration and optimization methods
    /**
     * Set learning rate
     * @param rate Learning rate (0.0 to 1.0)
     */
    void setLearningRate(double rate);

    /**
     * Enable or disable meta-learning
     * @param enable True to enable meta-learning
     */
    void setMetaLearningEnabled(bool enable);

    /**
     * Enable or disable skill transfer
     * @param enable True to enable skill transfer
     */
    void setSkillTransferEnabled(bool enable);

    /**
     * Set maximum skill complexity
     * @param complexity Maximum number of components in a skill
     */
    void setMaxSkillComplexity(size_t complexity);

    /**
     * Optimize learning parameters using experience
     * @return True if optimization was successful
     */
    bool optimizeLearningParameters();

    // Status and diagnostic methods
    /**
     * Get learning statistics
     * @return Map of statistic names to values
     */
    std::map<std::string, double> getLearningStatistics() const;

    /**
     * Get status information
     * @return Status string
     */
    std::string getStatusInfo() const;

    /**
     * Reset skill acquisition system
     */
    void reset();

private:
    // Internal helper methods
    void initializeSkillBase();
    void initializeComponents();
    Handle createSkillAtom(const std::string& name, SkillType type);
    void updateSkillProficiency(Handle skill_handle, double performance_score);
    void recordSkillPerformance(Handle skill_handle, double score);
    bool validateSkillParameters(const std::map<std::string, ValuePtr>& parameters);
    std::vector<Handle> extractSkillComponents(const std::vector<Handle>& experience_data);
    double calculateSkillComplexity(Handle skill_handle);
    double calculateTransferSimilarity(Handle source_skill, Handle target_context);
};

} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_SKILL_ACQUISITION_H