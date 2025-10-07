/*
 * opencog/agentzero/learning/ExperienceManager.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * ExperienceManager Implementation
 * Manages agent's experiential memory and learning from experiences
 * Part of the AGENT-ZERO-GENESIS project - Phase 5: Learning & Adaptation
 */

#ifndef _OPENCOG_AGENTZERO_LEARNING_EXPERIENCE_MANAGER_H
#define _OPENCOG_AGENTZERO_LEARNING_EXPERIENCE_MANAGER_H

#include <memory>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <chrono>
#include <functional>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/util/Logger.h>

namespace opencog {
namespace agentzero {

class AgentZeroCore; // Forward declaration

namespace learning {

/**
 * ExperienceManager - Manages agent's experiential memory
 *
 * This class provides comprehensive experience management capabilities,
 * integrating experiential learning with OpenCog's AtomSpace. It handles
 * experience acquisition, storage, retrieval, analysis, and learning
 * from past experiences.
 *
 * Key features:
 * - Experience representation in AtomSpace
 * - Temporal sequence modeling
 * - Experience-based pattern discovery
 * - Learning from success/failure patterns
 * - Memory consolidation and organization
 * - Context-aware experience retrieval
 * - Integration with MOSES for policy optimization
 * - Experience-driven skill acquisition
 */
class ExperienceManager
{
public:
    // Experience types for categorization
    enum class ExperienceType {
        ACTION_OUTCOME,     // Results of actions taken
        INTERACTION,        // Social/environmental interactions
        PROBLEM_SOLVING,    // Problem-solving episodes
        SKILL_APPLICATION,  // Application of learned skills
        GOAL_PURSUIT,       // Goal-directed behavior sequences
        UNEXPECTED,         // Unexpected events or outcomes
        LEARNING_EPISODE,   // Meta-learning experiences
        EMOTIONAL           // Emotional experiences and reactions
    };
    
    // Experience outcome classification
    enum class ExperienceOutcome {
        SUCCESS,            // Experience led to desired outcome
        FAILURE,            // Experience did not achieve goal
        PARTIAL_SUCCESS,    // Partially achieved desired outcome
        UNEXPECTED_OUTCOME, // Outcome was different than expected
        INCONCLUSIVE,       // Outcome unclear or mixed
        LEARNING_OPPORTUNITY // Experience provided valuable learning
    };
    
    // Experience importance levels
    enum class ExperienceImportance {
        CRITICAL = 100,     // Major successes/failures
        HIGH = 75,          // Important learning experiences
        MEDIUM = 50,        // Regular experiences
        LOW = 25,           // Minor experiences
        ROUTINE = 10        // Routine, expected experiences
    };

    // Experience context structure
    struct ExperienceContext {
        std::chrono::system_clock::time_point timestamp;
        std::vector<Handle> environmental_state;
        std::vector<Handle> agent_state;
        std::vector<Handle> active_goals;
        std::vector<Handle> applied_skills;
        std::map<std::string, double> emotional_state;
        double confidence_level;
        
        ExperienceContext() : confidence_level(0.5) {}
    };
    
    // Experience record structure
    struct Experience {
        Handle experience_atom;
        std::string description;
        ExperienceType type;
        ExperienceOutcome outcome;
        ExperienceImportance importance;
        ExperienceContext context;
        std::vector<Handle> preconditions;
        std::vector<Handle> actions;
        std::vector<Handle> consequences;
        std::vector<Handle> learned_patterns;
        double learning_value;
        TruthValuePtr reliability;
        
        Experience() : learning_value(0.0) {}
    };

private:
    // Core references
    AgentZeroCore* _agent_core;
    AtomSpacePtr _atomspace;
    
    // Experience storage and organization
    std::map<ExperienceType, std::vector<Experience>> _experiences_by_type;
    std::map<Handle, Experience> _experience_registry;
    std::vector<Experience> _recent_experiences;
    std::set<Handle> _significant_experiences;
    
    // AtomSpace handles for experience contexts
    Handle _experience_base;
    Handle _episodic_memory;
    Handle _experience_patterns;
    Handle _learning_outcomes;
    Handle _skill_experiences;
    Handle _goal_experiences;
    
    // Temporal organization
    std::map<std::chrono::system_clock::time_point, std::vector<Handle>> _temporal_index;
    std::vector<std::vector<Handle>> _experience_sequences;
    Handle _temporal_context;
    
    // Learning integration
    std::map<std::string, std::vector<Handle>> _pattern_library;
    std::map<Handle, double> _pattern_success_rates;
    std::vector<Handle> _active_learning_objectives;
    std::map<Handle, std::vector<Handle>> _skill_experience_map;
    
    // Configuration parameters
    bool _enable_pattern_discovery;
    bool _enable_moses_integration;
    bool _enable_temporal_modeling;
    bool _enable_emotional_learning;
    double _experience_retention_threshold;
    size_t _max_recent_experiences;
    double _pattern_significance_threshold;
    
    // MOSES integration for policy optimization
    bool _moses_available;
    Handle _moses_policy_space;
    std::map<Handle, Handle> _experience_to_policy_map;
    
    // Internal experience management methods
    void initializeExperienceStructures();
    Handle createExperienceAtom(const Experience& exp);
    void indexExperience(const Experience& exp);
    void updateExperiencePatterns(const Experience& exp);
    void consolidateExperiences();
    std::vector<Handle> extractPatternsFromExperience(const Experience& exp);
    double calculateExperienceSimilarity(const Experience& exp1, const Experience& exp2);
    void updateSkillExperienceMapping(const Experience& exp);
    
    // Pattern discovery and learning
    std::vector<Handle> discoverSequentialPatterns(const std::vector<Experience>& experiences);
    std::vector<Handle> discoverCausalPatterns(const std::vector<Experience>& experiences);
    std::map<Handle, double> analyzePatternOutcomes(const std::vector<Handle>& patterns);
    void learnFromPatternAnalysis(const std::map<Handle, double>& pattern_outcomes);
    
    // MOSES integration methods
    void integrateMOSESOptimization(const std::vector<Experience>& experiences);
    Handle createPolicyFromExperience(const Experience& exp);
    void optimizePolicyWithMOSES(const Handle& policy, const std::vector<Experience>& related_exp);
    std::vector<Handle> generatePolicyVariants(const Handle& base_policy);
    
    // Experience evaluation and filtering
    bool shouldRetainExperience(const Experience& exp);
    double calculateLearningValue(const Experience& exp);
    void pruneRedundantExperiences();
    void updateExperienceImportance();
    
    // Context and retrieval helpers
    double calculateContextSimilarity(const ExperienceContext& ctx1, const ExperienceContext& ctx2);
    std::vector<Handle> getContextualFeatures(const ExperienceContext& context);
    bool isExperienceRelevant(const Experience& exp, const std::vector<Handle>& current_context);

public:
    /**
     * Constructor
     * @param agent_core Pointer to the parent AgentZeroCore instance
     * @param atomspace Shared pointer to the AtomSpace
     */
    ExperienceManager(AgentZeroCore* agent_core, AtomSpacePtr atomspace);
    
    /**
     * Destructor - cleans up experience management resources
     */
    ~ExperienceManager();
    
    /**
     * Record a new experience
     * @param description Human-readable description of the experience
     * @param type Type/category of the experience
     * @param outcome Outcome classification of the experience
     * @param context Context in which the experience occurred
     * @param actions Actions that were taken during the experience
     * @param consequences Observed consequences of the actions
     * @return Handle to the created experience atom
     */
    Handle recordExperience(const std::string& description,
                           ExperienceType type,
                           ExperienceOutcome outcome,
                           const ExperienceContext& context,
                           const std::vector<Handle>& actions,
                           const std::vector<Handle>& consequences);
    
    /**
     * Record an experience with automatic context capture
     * @param description Description of the experience
     * @param type Type of experience
     * @param outcome Outcome of the experience
     * @return Handle to the created experience atom
     */
    Handle recordExperience(const std::string& description,
                           ExperienceType type,
                           ExperienceOutcome outcome);
    
    /**
     * Retrieve experiences similar to current context
     * @param current_context Current agent and environmental context
     * @param experience_type Optional filter by experience type
     * @param max_results Maximum number of results to return
     * @return Vector of similar experience handles
     */
    std::vector<Handle> getSimilarExperiences(const std::vector<Handle>& current_context,
                                            ExperienceType experience_type = ExperienceType::ACTION_OUTCOME,
                                            size_t max_results = 10);
    
    /**
     * Retrieve experiences by outcome type
     * @param outcome Target outcome type to filter by
     * @param limit Maximum number of experiences to return
     * @return Vector of experience handles with specified outcome
     */
    std::vector<Handle> getExperiencesByOutcome(ExperienceOutcome outcome, size_t limit = 20);
    
    /**
     * Learn patterns from accumulated experiences
     * @param experience_type Optional filter for specific experience types
     * @return Number of new patterns discovered
     */
    size_t discoverExperiencePatterns(ExperienceType experience_type = ExperienceType::ACTION_OUTCOME);
    
    /**
     * Get successful patterns for a given context
     * @param context_atoms Context atoms to match against
     * @param min_success_rate Minimum success rate for patterns
     * @return Vector of successful pattern atoms
     */
    std::vector<Handle> getSuccessfulPatterns(const std::vector<Handle>& context_atoms,
                                            double min_success_rate = 0.6);
    
    /**
     * Analyze experience for learning opportunities
     * @param experience_atom Handle to the experience to analyze
     * @return Vector of learning insight atoms
     */
    std::vector<Handle> analyzeExperienceForLearning(const Handle& experience_atom);
    
    /**
     * Get experiences related to a specific skill
     * @param skill_atom Handle to the skill atom
     * @param include_failures Whether to include failed attempts
     * @return Vector of skill-related experience handles
     */
    std::vector<Handle> getSkillExperiences(const Handle& skill_atom, bool include_failures = true);
    
    /**
     * Get temporal sequence of experiences
     * @param start_time Start time for the sequence
     * @param end_time End time for the sequence
     * @return Chronologically ordered vector of experience handles
     */
    std::vector<Handle> getExperienceSequence(const std::chrono::system_clock::time_point& start_time,
                                            const std::chrono::system_clock::time_point& end_time);
    
    /**
     * Optimize policies using MOSES based on experience data
     * @param policy_atom Handle to the policy to optimize
     * @param related_experiences Experiences related to the policy
     * @return Handle to the optimized policy atom
     */
    Handle optimizePolicyFromExperience(const Handle& policy_atom,
                                       const std::vector<Handle>& related_experiences);
    
    /**
     * Get experience statistics
     * @return Map of experience type to count
     */
    std::map<ExperienceType, size_t> getExperienceStatistics() const;
    
    /**
     * Get learning insights from recent experiences
     * @param days Number of days to look back
     * @return Vector of learning insight atoms
     */
    std::vector<Handle> getRecentLearningInsights(int days = 7);
    
    /**
     * Clear old experiences based on retention policy
     * @return Number of experiences removed
     */
    size_t pruneOldExperiences();
    
    /**
     * Export experiences for external analysis
     * @param experience_type Optional filter by type
     * @return JSON string representation of experiences
     */
    std::string exportExperiences(ExperienceType experience_type = ExperienceType::ACTION_OUTCOME) const;
    
    /**
     * Get configuration status
     * @return String containing configuration information
     */
    std::string getConfigurationStatus() const;
    
    /**
     * Process experience management (should be called periodically)
     * @return True if processing was successful
     */
    bool processExperienceManagement();

    // Configuration methods
    void enablePatternDiscovery(bool enable) { _enable_pattern_discovery = enable; }
    void enableMOSESIntegration(bool enable) { _enable_moses_integration = enable; }
    void enableTemporalModeling(bool enable) { _enable_temporal_modeling = enable; }
    void setExperienceRetentionThreshold(double threshold) { _experience_retention_threshold = threshold; }
    void setMaxRecentExperiences(size_t max_size) { _max_recent_experiences = max_size; }
    void setPatternSignificanceThreshold(double threshold) { _pattern_significance_threshold = threshold; }
};

} // namespace learning
} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_LEARNING_EXPERIENCE_MANAGER_H