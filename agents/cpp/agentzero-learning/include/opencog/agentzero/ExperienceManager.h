/*
 * opencog/agentzero/ExperienceManager.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * ExperienceManager - Manages agent's experiential memory
 * Part of AZ-LEARN-004: Implement MetaLearning capabilities
 */

#ifndef _OPENCOG_AGENTZERO_EXPERIENCE_MANAGER_H
#define _OPENCOG_AGENTZERO_EXPERIENCE_MANAGER_H

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

/**
 * Experience types for categorization
 */
enum class ExperienceType {
    LEARNING,       // Learning-related experiences
    PLANNING,       // Planning and goal-related experiences  
    EXECUTION,      // Action execution experiences
    SOCIAL,         // Social interaction experiences
    EXPLORATION,    // Exploration and discovery experiences
    REFLECTION      // Meta-cognitive reflection experiences
};

/**
 * Experience entry structure
 */
struct Experience {
    Handle id;                                    // Unique identifier
    ExperienceType type;                         // Type of experience
    Handle context;                              // Context where experience occurred
    Handle task;                                 // Associated task or goal
    Handle outcome;                              // Result or outcome
    std::chrono::system_clock::time_point timestamp; // When experience occurred
    double importance;                           // Importance/relevance score
    std::map<std::string, Handle> metadata;      // Additional metadata
    
    Experience() : type(ExperienceType::LEARNING), importance(0.5) {}
};

/**
 * Experience query criteria
 */
struct ExperienceQuery {
    ExperienceType type_filter;
    Handle context_filter;
    Handle task_filter;
    std::chrono::system_clock::time_point start_time;
    std::chrono::system_clock::time_point end_time;
    double min_importance;
    int max_results;
    
    ExperienceQuery() 
        : type_filter(ExperienceType::LEARNING), min_importance(0.0), max_results(100) {}
};

/**
 * ExperienceManager - Manages agent's experiential memory
 *
 * This class provides comprehensive management of the agent's experiences,
 * including storage, retrieval, analysis, and integration with the AtomSpace.
 * It serves as the experiential memory component for meta-learning.
 *
 * Key features:
 * - Experience storage and retrieval
 * - Temporal and contextual indexing
 * - Importance-based memory consolidation
 * - Pattern recognition in experiences
 * - Integration with AtomSpace for persistent storage
 */
class ExperienceManager
{
private:
    // Core references
    AtomSpacePtr _atomspace;
    
    // Experience storage
    std::vector<Experience> _experiences;
    std::map<Handle, size_t> _experience_index;        // Handle to index mapping
    std::map<ExperienceType, std::vector<size_t>> _type_index;  // Type-based index
    std::map<Handle, std::vector<size_t>> _context_index;       // Context-based index
    
    // Memory management
    size_t _max_experiences;
    double _importance_threshold;
    std::chrono::hours _retention_period;
    
    // AtomSpace handles
    Handle _experience_context;
    Handle _memory_link;
    
    // Internal methods
    void consolidateMemory();
    void updateImportanceScores();
    bool shouldRetainExperience(const Experience& exp) const;
    Handle createExperienceAtom(const Experience& exp);
    void indexExperience(const Experience& exp, size_t index);
    void removeExperienceFromIndices(size_t index);

public:
    /**
     * Constructor
     * @param atomspace Shared pointer to AtomSpace
     * @param max_experiences Maximum number of experiences to retain
     * @param importance_threshold Minimum importance for retention
     * @param retention_period How long to retain experiences
     */
    ExperienceManager(AtomSpacePtr atomspace, size_t max_experiences = 10000,
                     double importance_threshold = 0.1,
                     std::chrono::hours retention_period = std::chrono::hours(24 * 30));
    
    /**
     * Destructor
     */
    ~ExperienceManager();
    
    /**
     * Initialize experience manager
     */
    void initialize();
    
    // Experience management
    /**
     * Record a new experience
     * @param type Type of experience
     * @param context Context where experience occurred
     * @param task Associated task or goal
     * @param outcome Result or outcome
     * @param importance Importance score (0.0-1.0)
     * @return Handle to the created experience
     */
    Handle recordExperience(ExperienceType type, const Handle& context, 
                           const Handle& task, const Handle& outcome,
                           double importance = 0.5);
    
    /**
     * Retrieve experiences matching query criteria
     * @param query Query criteria
     * @return Vector of matching experiences
     */
    std::vector<Experience> queryExperiences(const ExperienceQuery& query) const;
    
    /**
     * Get experience by handle
     * @param experience_handle Handle to experience
     * @return Experience if found, empty Experience otherwise
     */
    Experience getExperience(const Handle& experience_handle) const;
    
    /**
     * Update experience importance score
     * @param experience_handle Handle to experience
     * @param new_importance New importance score
     * @return True if successfully updated
     */
    bool updateExperienceImportance(const Handle& experience_handle, double new_importance);
    
    /**
     * Get recent experiences
     * @param time_window Time window to consider
     * @param max_count Maximum number of experiences to return
     * @return Vector of recent experiences
     */
    std::vector<Experience> getRecentExperiences(std::chrono::hours time_window, 
                                                int max_count = 100) const;
    
    /**
     * Get experiences by type
     * @param type Experience type
     * @param max_count Maximum number of experiences to return
     * @return Vector of experiences of specified type
     */
    std::vector<Experience> getExperiencesByType(ExperienceType type, int max_count = 100) const;
    
    /**
     * Get experiences by context
     * @param context Context handle
     * @param max_count Maximum number of experiences to return
     * @return Vector of experiences in specified context
     */
    std::vector<Experience> getExperiencesByContext(const Handle& context, int max_count = 100) const;
    
    // Analysis and patterns
    /**
     * Analyze experience patterns
     * @param time_window Time window for analysis
     * @return Handle to pattern analysis results
     */
    Handle analyzeExperiencePatterns(std::chrono::hours time_window = std::chrono::hours(24));
    
    /**
     * Find similar experiences
     * @param target_experience Experience to match against
     * @param max_results Maximum number of similar experiences to return
     * @return Vector of similar experiences
     */
    std::vector<Experience> findSimilarExperiences(const Experience& target_experience, 
                                                  int max_results = 10) const;
    
    /**
     * Get experience statistics
     * @return Map of statistics (counts by type, average importance, etc.)
     */
    std::map<std::string, double> getExperienceStatistics() const;
    
    // Memory management
    /**
     * Trigger memory consolidation
     * Removes old or low-importance experiences to maintain memory limits
     */
    void consolidateMemoryManual();
    
    /**
     * Clear all experiences
     */
    void clearAllExperiences();
    
    /**
     * Get current memory usage statistics
     * @return Map of memory usage statistics
     */
    std::map<std::string, size_t> getMemoryUsage() const;
    
    // Configuration
    /**
     * Set maximum number of experiences
     * @param max_experiences New maximum
     */
    void setMaxExperiences(size_t max_experiences);
    
    /**
     * Set importance threshold for retention
     * @param threshold New threshold (0.0-1.0)
     */
    void setImportanceThreshold(double threshold);
    
    /**
     * Set retention period for experiences
     * @param period New retention period
     */
    void setRetentionPeriod(std::chrono::hours period);
    
    // Utility methods
    /**
     * Convert experience type to string
     * @param type Experience type
     * @return String representation
     */
    static std::string experienceTypeToString(ExperienceType type);
    
    /**
     * Convert string to experience type
     * @param type_str String representation
     * @return Experience type
     */
    static ExperienceType stringToExperienceType(const std::string& type_str);
    
    /**
     * Calculate similarity between two experiences
     * @param exp1 First experience
     * @param exp2 Second experience
     * @return Similarity score (0.0-1.0)
     */
    static double calculateExperienceSimilarity(const Experience& exp1, const Experience& exp2);
    
    // Status and debugging
    /**
     * Check if experience manager is initialized
     * @return True if initialized
     */
    bool isInitialized() const;
    
    /**
     * Get total number of experiences
     * @return Total experience count
     */
    size_t getExperienceCount() const { return _experiences.size(); }
    
    /**
     * Validate experience storage integrity
     * @return True if storage is consistent
     */
    bool validateStorageIntegrity() const;
};

} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_EXPERIENCE_MANAGER_H