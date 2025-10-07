/**
 * ExperienceManager.h - Experience Memory Management for Agent-Zero Learning
 * 
 * Part of AZ-LEARN-003: MOSES Policy Optimization Integration
 * Manages agent's experiential memory for learning and policy optimization
 * 
 * Copyright (C) 2024 OpenCog Foundation
 */

#ifndef AGENTZERO_EXPERIENCE_MANAGER_H
#define AGENTZERO_EXPERIENCE_MANAGER_H

#include <memory>
#include <vector>
#include <deque>
#include <map>
#include <set>
#include <functional>
#include <mutex>
#include <atomic>

#include "LearningTypes.h"

namespace opencog {
namespace agentzero {
namespace learning {

/**
 * ExperienceManager - Manages agent's experiential memory
 * 
 * This class provides comprehensive experience management for Agent-Zero:
 * - Storage and retrieval of experience tuples (state, action, reward, next_state)
 * - Experience replay buffer for policy optimization
 * - AtomSpace integration for persistent experience storage
 * - Experience filtering and sampling strategies
 * - Statistical analysis of experience patterns
 */
class ExperienceManager {
public:
    /**
     * Constructor
     * @param atomspace Shared pointer to the AtomSpace
     * @param config Learning configuration parameters
     */
    explicit ExperienceManager(AtomSpacePtr atomspace, 
                              const LearningConfig& config = LearningConfig{});
    
    /**
     * Destructor
     */
    ~ExperienceManager();
    
    // Disable copy construction and assignment
    ExperienceManager(const ExperienceManager&) = delete;
    ExperienceManager& operator=(const ExperienceManager&) = delete;
    
    /**
     * Add a new experience to the memory
     * @param experience The experience to add
     * @return true if successfully added
     */
    bool addExperience(const Experience& experience);
    
    /**
     * Add a new experience with individual components
     * @param state_atom Current state
     * @param action_atom Action taken
     * @param next_state_atom Resulting state
     * @param reward Reward received
     * @param terminal Whether this is a terminal state
     * @return Experience ID if successful, empty string if failed
     */
    ExperienceId addExperience(Handle state_atom, Handle action_atom,
                              Handle next_state_atom, double reward, bool terminal);
    
    /**
     * Retrieve a specific experience by ID
     * @param experience_id The experience identifier
     * @return Shared pointer to the experience, or nullptr if not found
     */
    std::shared_ptr<Experience> getExperience(const ExperienceId& experience_id);
    
    /**
     * Get recent experiences up to a specified count
     * @param count Maximum number of experiences to return
     * @return Vector of recent experiences
     */
    std::vector<std::shared_ptr<Experience>> getRecentExperiences(size_t count);
    
    /**
     * Sample random experiences for replay learning
     * @param sample_size Number of experiences to sample
     * @param prioritized Whether to use prioritized sampling
     * @return Vector of sampled experiences
     */
    std::vector<std::shared_ptr<Experience>> sampleExperiences(size_t sample_size,
                                                              bool prioritized = false);
    
    /**
     * Get experiences matching specific criteria
     * @param filter_function Function to filter experiences
     * @param max_results Maximum number of results to return
     * @return Vector of matching experiences
     */
    std::vector<std::shared_ptr<Experience>> getExperiencesByFilter(
        std::function<bool(const Experience&)> filter_function,
        size_t max_results = 0);
    
    /**
     * Get experiences involving a specific state atom
     * @param state_atom The state atom to search for
     * @param include_next_states Whether to include experiences where this is the next state
     * @return Vector of matching experiences
     */
    std::vector<std::shared_ptr<Experience>> getExperiencesByState(
        Handle state_atom, bool include_next_states = true);
    
    /**
     * Get experiences involving a specific action atom
     * @param action_atom The action atom to search for
     * @return Vector of matching experiences
     */
    std::vector<std::shared_ptr<Experience>> getExperiencesByAction(Handle action_atom);
    
    /**
     * Get experiences within a reward range
     * @param min_reward Minimum reward value
     * @param max_reward Maximum reward value
     * @return Vector of matching experiences
     */
    std::vector<std::shared_ptr<Experience>> getExperiencesByRewardRange(
        double min_reward, double max_reward);
    
    /**
     * Store experiences to AtomSpace for persistence
     * @param experiences Vector of experiences to store (empty for all)
     * @return Number of experiences successfully stored
     */
    size_t storeExperiencesToAtomSpace(const std::vector<std::shared_ptr<Experience>>& experiences = {});
    
    /**
     * Load experiences from AtomSpace
     * @param max_experiences Maximum number to load (0 for all)
     * @return Number of experiences successfully loaded
     */
    size_t loadExperiencesFromAtomSpace(size_t max_experiences = 0);
    
    /**
     * Clear all experiences from memory (not AtomSpace)
     */
    void clearMemory();
    
    /**
     * Get the total number of experiences in memory
     * @return Number of experiences
     */
    size_t getExperienceCount() const;
    
    /**
     * Get the current memory buffer size limit
     * @return Buffer size limit
     */
    size_t getBufferSizeLimit() const;
    
    /**
     * Set the memory buffer size limit
     * @param size New buffer size limit
     */
    void setBufferSizeLimit(size_t size);
    
    /**
     * Get experience statistics
     * @return Map of statistic names to values
     */
    std::map<std::string, double> getExperienceStats() const;
    
    /**
     * Get reward distribution statistics
     * @return Map with min, max, mean, std_dev of rewards
     */
    std::map<std::string, double> getRewardStats() const;
    
    /**
     * Get the most frequent state-action pairs
     * @param top_k Number of top pairs to return
     * @return Vector of (state, action, count) tuples
     */
    std::vector<std::tuple<Handle, Handle, size_t>> getMostFrequentStateActionPairs(size_t top_k = 10);
    
    /**
     * Calculate state transition probabilities
     * @param state_atom The state to analyze
     * @return Map of next states to transition probabilities
     */
    std::map<Handle, double> getStateTransitionProbabilities(Handle state_atom);
    
    /**
     * Set experience priority for prioritized replay
     * @param experience_id Experience to update
     * @param priority New priority value
     * @return true if successful
     */
    bool setExperiencePriority(const ExperienceId& experience_id, double priority);
    
    /**
     * Update configuration parameters
     * @param config New configuration
     */
    void updateConfig(const LearningConfig& config);
    
    /**
     * Get current configuration
     * @return Current configuration
     */
    const LearningConfig& getConfig() const;
    
    /**
     * Record an experience with advanced classification (from main branch approach)
     * @param description Human-readable description of the experience
     * @param type Type of experience
     * @param outcome Outcome classification
     * @param actions Actions taken during the experience
     * @param consequences Observed consequences
     * @param confidence Confidence level in the classification
     * @return Experience ID if successful, empty string if failed
     */
    ExperienceId recordExperience(const std::string& description,
                                ExperienceType type,
                                ExperienceOutcome outcome,
                                const std::vector<Handle>& actions = {},
                                const std::vector<Handle>& consequences = {},
                                double confidence = 0.8);
    
    /**
     * Discover patterns from accumulated experiences
     * @return Number of patterns discovered
     */
    size_t discoverExperiencePatterns();
    
    /**
     * Get experiences by type
     * @param type Experience type to filter by
     * @return Vector of matching experiences
     */
    std::vector<std::shared_ptr<Experience>> getExperiencesByType(ExperienceType type);
    
    /**
     * Get experiences by outcome
     * @param outcome Experience outcome to filter by
     * @return Vector of matching experiences
     */
    std::vector<std::shared_ptr<Experience>> getExperiencesByOutcome(ExperienceOutcome outcome);
    
    /**
     * Get successful patterns for decision making
     * @param context Current context atoms
     * @param min_success_rate Minimum success rate required
     * @return Vector of successful experience patterns
     */
    std::vector<std::shared_ptr<Experience>> getSuccessfulPatterns(
        const std::vector<Handle>& context, double min_success_rate = 0.7);

private:
    // Core members
    AtomSpacePtr atomspace_;
    LearningConfig config_;
    
    // Experience storage
    std::deque<std::shared_ptr<Experience>> experience_buffer_;
    std::map<ExperienceId, std::shared_ptr<Experience>> experience_index_;
    std::map<ExperienceId, double> experience_priorities_;
    
    // Thread safety
    mutable std::mutex buffer_mutex_;
    mutable std::mutex index_mutex_;
    mutable std::mutex priority_mutex_;
    
    // Statistics
    std::atomic<size_t> total_experiences_added_;
    std::atomic<double> total_reward_accumulated_;
    mutable std::mutex stats_mutex_;
    
    // Indexing for fast retrieval
    std::map<Handle, std::set<ExperienceId>> state_to_experiences_;
    std::map<Handle, std::set<ExperienceId>> action_to_experiences_;
    std::map<Handle, std::set<ExperienceId>> next_state_to_experiences_;
    mutable std::mutex index_maps_mutex_;
    
    // Advanced structure handles (from main branch integration)
    Handle _experience_base;
    Handle _episodic_memory;
    Handle _experience_patterns;
    Handle _learning_outcomes;
    Handle _skill_experiences;
    Handle _goal_experiences;
    Handle _temporal_context;
    Handle _moses_policy_space;
    
    // Advanced configuration flags
    bool _enable_pattern_discovery;
    bool _enable_moses_integration;
    bool _enable_temporal_modeling;
    bool _enable_emotional_learning;
    double _experience_retention_threshold;
    size_t _max_recent_experiences;
    double _pattern_significance_threshold;
    bool _moses_available;
    
    // Private methods
    
    /**
     * Initialize advanced AtomSpace structures for sophisticated experience management
     */
    void initializeAdvancedStructures();
    
    /**
     * Generate unique experience ID
     */
    ExperienceId generateExperienceId() const;
    
    /**
     * Add experience to internal indices
     */
    void addToIndices(const std::shared_ptr<Experience>& experience);
    
    /**
     * Remove experience from internal indices
     */
    void removeFromIndices(const std::shared_ptr<Experience>& experience);
    
    /**
     * Enforce buffer size limit by removing oldest experiences
     */
    void enforceBufferLimit();
    
    /**
     * Convert experience to AtomSpace representation
     */
    Handle experienceToAtomSpaceRepresentation(const Experience& experience);
    
    /**
     * Convert AtomSpace representation back to experience
     */
    std::shared_ptr<Experience> atomSpaceRepresentationToExperience(Handle experience_atom);
    
    /**
     * Sample with prioritized experience replay
     */
    std::vector<std::shared_ptr<Experience>> prioritizedSample(size_t sample_size);
    
    /**
     * Sample uniformly at random
     */
    std::vector<std::shared_ptr<Experience>> uniformSample(size_t sample_size);
    
    /**
     * Update experience priority based on temporal difference error
     */
    void updatePriorityBasedOnTDError(const ExperienceId& experience_id, double td_error);
    
    /**
     * Initialize AtomSpace structures for experience storage
     */
    void initializeAtomSpaceStructures();
    
    /**
     * Validate experience for consistency
     */
    bool validateExperience(const Experience& experience) const;
    
    /**
     * Clean up old experiences based on age and importance
     */
    void cleanupOldExperiences();
    
    /**
     * Calculate experience importance score
     */
    double calculateExperienceImportance(const Experience& experience) const;
};

/**
 * Factory function to create an ExperienceManager with common configurations
 */
std::unique_ptr<ExperienceManager> createExperienceManager(
    AtomSpacePtr atomspace,
    const std::string& config_preset = "default");

} // namespace learning
} // namespace agentzero
} // namespace opencog

#endif // AGENTZERO_EXPERIENCE_MANAGER_H