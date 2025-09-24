/*
 * opencog/agentzero/SkillAcquisition.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * SkillAcquisition - Learns new capabilities through experience
 * Part of AZ-LEARN-004: Implement MetaLearning capabilities
 */

#ifndef _OPENCOG_AGENTZERO_SKILL_ACQUISITION_H
#define _OPENCOG_AGENTZERO_SKILL_ACQUISITION_H

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <chrono>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/util/Logger.h>

namespace opencog {
namespace agentzero {

// Forward declarations
class ExperienceManager;

/**
 * Skill representation
 */
struct Skill {
    Handle id;                          // Unique skill identifier
    std::string name;                   // Human-readable skill name
    std::string description;            // Skill description
    Handle preconditions;               // Required preconditions
    Handle actions;                     // Actions that comprise the skill
    Handle postconditions;              // Expected outcomes
    double proficiency;                 // Current proficiency level (0.0-1.0)
    double confidence;                  // Confidence in skill execution (0.0-1.0)
    int practice_count;                 // Number of times skill has been practiced
    std::chrono::system_clock::time_point last_used; // Last time skill was used
    std::vector<Handle> contexts;       // Contexts where skill is applicable
    
    Skill() : proficiency(0.0), confidence(0.0), practice_count(0) {}
};

/**
 * Learning opportunity representation
 */
struct LearningOpportunity {
    Handle context;                     // Context where learning can occur
    Handle task;                        // Task that could be learned
    Handle demonstration;               // Example or demonstration to learn from
    double potential;                   // Learning potential score (0.0-1.0)
    std::string learning_method;        // Suggested learning method
    
    LearningOpportunity() : potential(0.0) {}
};

/**
 * Skill acquisition configuration
 */
struct SkillAcquisitionConfig {
    double min_proficiency_threshold;   // Minimum proficiency to consider skill learned
    double confidence_threshold;        // Minimum confidence for skill execution
    int max_practice_attempts;          // Maximum practice attempts per session
    double learning_rate;               // Rate of skill improvement
    bool enable_imitation_learning;     // Enable learning by imitation
    bool enable_exploratory_learning;   // Enable learning through exploration
    std::chrono::hours skill_decay_period; // Time after which unused skills decay
    
    SkillAcquisitionConfig()
        : min_proficiency_threshold(0.7), confidence_threshold(0.6),
          max_practice_attempts(10), learning_rate(0.1),
          enable_imitation_learning(true), enable_exploratory_learning(true),
          skill_decay_period(std::chrono::hours(24 * 7)) {} // 1 week
};

/**
 * SkillAcquisition - Learns new capabilities through experience
 *
 * This class manages the acquisition and refinement of skills through
 * various learning mechanisms including imitation, exploration, and
 * practice. It integrates with the experience manager to learn from
 * past experiences and identify opportunities for skill development.
 *
 * Key features:
 * - Skill discovery and acquisition
 * - Imitation-based learning
 * - Exploratory skill development
 * - Skill proficiency tracking and improvement
 * - Context-aware skill application
 */
class SkillAcquisition
{
private:
    // Core references
    AtomSpacePtr _atomspace;
    std::shared_ptr<ExperienceManager> _experience_manager;
    
    // Skill storage
    std::vector<Skill> _skills;
    std::map<Handle, size_t> _skill_index;          // Handle to index mapping
    std::map<std::string, std::vector<size_t>> _name_index; // Name-based index
    std::map<Handle, std::vector<size_t>> _context_index;   // Context-based index
    
    // Learning state
    SkillAcquisitionConfig _config;
    std::vector<LearningOpportunity> _opportunities;
    Handle _current_learning_context;
    
    // AtomSpace handles
    Handle _skill_context;
    Handle _learning_link;
    
    // Internal methods - Skill Management
    Handle createSkillAtom(const Skill& skill);
    void updateSkillProficiency(size_t skill_index, bool success, double performance);
    void indexSkill(const Skill& skill, size_t index);
    bool shouldDecaySkill(const Skill& skill) const;
    void decayUnusedSkills();
    
    // Internal methods - Learning Discovery
    std::vector<LearningOpportunity> identifyLearningOpportunities();
    LearningOpportunity analyzeTaskForLearning(const Handle& task, const Handle& context);
    double assessLearningPotential(const Handle& task, const Handle& context);
    
    // Internal methods - Skill Learning
    bool learnSkillThroughImitation(const Handle& demonstration, const Handle& context);
    bool learnSkillThroughExploration(const Handle& task, const Handle& context);
    Handle decomposeTaskIntoActions(const Handle& task);
    Handle identifySkillPreconditions(const Handle& task, const Handle& context);
    Handle identifySkillPostconditions(const Handle& task, const Handle& context);
    
    // Internal methods - Skill Refinement
    void refineSkillThroughPractice(size_t skill_index);
    void optimizeSkillExecution(size_t skill_index);
    bool combineSkillsIntoComposite(const std::vector<size_t>& skill_indices);

public:
    /**
     * Constructor
     * @param atomspace Shared pointer to AtomSpace
     * @param config Skill acquisition configuration
     */
    SkillAcquisition(AtomSpacePtr atomspace, 
                    const SkillAcquisitionConfig& config = SkillAcquisitionConfig());
    
    /**
     * Destructor
     */
    ~SkillAcquisition();
    
    /**
     * Initialize skill acquisition system
     */
    void initialize();
    
    // Core skill operations
    /**
     * Learn a new skill from demonstration
     * @param demonstration Handle to demonstration or example
     * @param context Context in which skill should be learned
     * @param skill_name Optional name for the skill
     * @return Handle to learned skill, or undefined if learning failed
     */
    Handle learnSkillFromDemonstration(const Handle& demonstration, const Handle& context,
                                      const std::string& skill_name = "");
    
    /**
     * Learn a skill through exploration and practice
     * @param task Task to learn
     * @param context Learning context
     * @param skill_name Optional name for the skill
     * @return Handle to learned skill, or undefined if learning failed
     */
    Handle learnSkillThroughPractice(const Handle& task, const Handle& context,
                                    const std::string& skill_name = "");
    
    /**
     * Execute a skill in a given context
     * @param skill_handle Handle to skill
     * @param context Execution context
     * @param parameters Optional parameters for skill execution
     * @return Execution result handle
     */
    Handle executeSkill(const Handle& skill_handle, const Handle& context,
                       const Handle& parameters = Handle::UNDEFINED);
    
    /**
     * Practice an existing skill
     * @param skill_handle Handle to skill
     * @param context Practice context
     * @return True if practice was successful
     */
    bool practiceSkill(const Handle& skill_handle, const Handle& context);
    
    // Skill discovery and management
    /**
     * Discover potential skills from experience
     * Analyzes experience history to identify patterns that could become skills
     * @return Number of potential skills discovered
     */
    int discoverSkillsFromExperience();
    
    /**
     * Get skills applicable to a context
     * @param context Context to match
     * @param min_proficiency Minimum proficiency level required
     * @return Vector of applicable skills
     */
    std::vector<Skill> getApplicableSkills(const Handle& context, double min_proficiency = 0.5) const;
    
    /**
     * Get skill by handle
     * @param skill_handle Handle to skill
     * @return Skill if found, empty skill otherwise
     */
    Skill getSkill(const Handle& skill_handle) const;
    
    /**
     * Get skill by name
     * @param skill_name Name of skill
     * @return Vector of skills with matching name
     */
    std::vector<Skill> getSkillsByName(const std::string& skill_name) const;
    
    /**
     * Update skill proficiency based on execution results
     * @param skill_handle Handle to skill
     * @param success Whether skill execution was successful
     * @param performance Performance score (0.0-1.0)
     * @return True if successfully updated
     */
    bool updateSkillProficiency(const Handle& skill_handle, bool success, double performance = 0.5);
    
    // Learning opportunity management
    /**
     * Identify current learning opportunities
     * @param context Current context
     * @return Vector of learning opportunities
     */
    std::vector<LearningOpportunity> identifyLearningOpportunities(const Handle& context);
    
    /**
     * Pursue a learning opportunity
     * @param opportunity Learning opportunity to pursue
     * @return Handle to newly learned skill, or undefined if unsuccessful
     */
    Handle pursueLearningOpportunity(const LearningOpportunity& opportunity);
    
    // Skill composition and decomposition
    /**
     * Compose multiple skills into a compound skill
     * @param skill_handles Vector of skill handles to compose
     * @param composition_name Name for the compound skill
     * @return Handle to compound skill
     */
    Handle composeSkills(const std::vector<Handle>& skill_handles, 
                        const std::string& composition_name);
    
    /**
     * Decompose a complex skill into simpler components
     * @param skill_handle Handle to skill to decompose
     * @return Vector of component skill handles
     */
    std::vector<Handle> decomposeSkill(const Handle& skill_handle);
    
    // Analysis and metrics
    /**
     * Analyze skill acquisition progress
     * @param time_window Time window for analysis
     * @return Analysis results atom
     */
    Handle analyzeSkillAcquisitionProgress(std::chrono::hours time_window = std::chrono::hours(24));
    
    /**
     * Get skill acquisition statistics
     * @return Map of statistics (skill counts, proficiency levels, etc.)
     */
    std::map<std::string, double> getSkillStatistics() const;
    
    /**
     * Get learning progress for a specific skill
     * @param skill_handle Handle to skill
     * @return Progress information atom
     */
    Handle getSkillProgress(const Handle& skill_handle) const;
    
    // Configuration and control
    /**
     * Configure skill acquisition parameters
     * @param config New configuration
     */
    void configure(const SkillAcquisitionConfig& config);
    
    /**
     * Set experience manager reference
     * @param experience_manager Shared pointer to experience manager
     */
    void setExperienceManager(std::shared_ptr<ExperienceManager> experience_manager);
    
    /**
     * Reset all skills and learning state
     */
    void reset();
    
    /**
     * Check if skill acquisition system is initialized
     * @return True if initialized
     */
    bool isInitialized() const;
    
    // Status and debugging
    /**
     * Get total number of skills
     * @return Skill count
     */
    size_t getSkillCount() const { return _skills.size(); }
    
    /**
     * Get skills with proficiency above threshold
     * @param threshold Proficiency threshold
     * @return Vector of proficient skills
     */
    std::vector<Skill> getProficientSkills(double threshold = 0.7) const;
    
    /**
     * Validate skill storage integrity
     * @return True if storage is consistent
     */
    bool validateSkillIntegrity() const;
    
    /**
     * Trigger maintenance tasks (skill decay, optimization, etc.)
     */
    void performMaintenance();
};

} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_SKILL_ACQUISITION_H