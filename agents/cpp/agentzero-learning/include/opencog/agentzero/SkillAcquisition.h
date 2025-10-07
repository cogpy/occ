/*
 * opencog/agentzero/SkillAcquisition.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * SkillAcquisition - Learning new capabilities through experience
 * Core learning component of Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 * SkillAcquisition - Learns new capabilities through experience
 * Part of AZ-LEARN-004: Implement MetaLearning capabilities
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
     * @param atomspace AtomSpace for knowledge representation
     */
    explicit SkillAcquisition(AtomSpacePtr atomspace);

     * @param atomspace Shared pointer to AtomSpace
     * @param config Skill acquisition configuration
     */
    SkillAcquisition(AtomSpacePtr atomspace, 
                    const SkillAcquisitionConfig& config = SkillAcquisitionConfig());
    
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