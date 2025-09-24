/*
 * opencog/agentzero/MetaLearning.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * MetaLearning - Learning how to learn more effectively
 * Part of the Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 */

#ifndef _OPENCOG_AGENTZERO_META_LEARNING_H
#define _OPENCOG_AGENTZERO_META_LEARNING_H

#include <memory>
#include <string>
#include <vector>
#include <map>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/util/Logger.h>

namespace opencog {
namespace agentzero {

// Forward declaration
class SkillAcquisition;

/**
 * MetaLearning - Optimizes the learning process itself
 *
 * This class implements meta-learning capabilities that allow the agent
 * to learn how to learn more effectively. It analyzes learning patterns,
 * adapts learning strategies, and optimizes learning parameters based
 * on experience and performance feedback.
 */
class MetaLearning
{
public:
    /**
     * Learning strategy adaptations that can be made
     */
    enum class StrategyAdaptation {
        INCREASE_EXPLORATION,   // Explore more variations
        DECREASE_EXPLORATION,   // Focus on exploitation
        CHANGE_REPRESENTATION, // Modify knowledge representation
        ADJUST_LEARNING_RATE,  // Change learning speed
        MODIFY_OBJECTIVES,     // Adjust learning objectives
        TRANSFER_KNOWLEDGE,    // Use knowledge from other domains
        SIMPLIFY_APPROACH,     // Reduce complexity
        INCREASE_COMPLEXITY    // Add more sophisticated methods
    };

    /**
     * Meta-learning objectives
     */
    enum class MetaObjective {
        MINIMIZE_LEARNING_TIME,    // Learn faster
        MAXIMIZE_GENERALIZATION,   // Learn more general patterns
        IMPROVE_RETENTION,         // Remember better
        INCREASE_TRANSFER,         // Transfer knowledge better
        REDUCE_INTERFERENCE,       // Avoid negative transfer
        OPTIMIZE_RESOURCES,        // Use less computational resources
        BALANCE_ACCURACY_SPEED     // Balance learning quality and speed
    };

private:
    AtomSpacePtr _atomspace;
    Handle _meta_learning_base;
    
    // Learning performance tracking
    std::map<std::string, std::vector<double>> _learning_performance_history;
    std::map<std::string, std::vector<double>> _adaptation_effectiveness;
    std::map<std::string, size_t> _strategy_usage_counts;
    
    // Meta-learning parameters
    double _adaptation_threshold;
    size_t _min_samples_for_adaptation;
    bool _enable_strategy_transfer;
    
    // Performance metrics
    std::map<std::string, double> _current_learning_rates;
    std::map<std::string, double> _learning_efficiency_scores;

public:
    /**
     * Constructor
     * @param atomspace AtomSpace for meta-learning knowledge storage
     */
    explicit MetaLearning(AtomSpacePtr atomspace);

    /**
     * Destructor
     */
    ~MetaLearning();

    /**
     * Adapt learning strategy based on performance
     * @param skill_handle Handle to the skill being learned
     * @param current_strategy Current learning strategy being used
     * @param experience_data Recent learning experiences
     * @return Recommended strategy adaptation
     */
    StrategyAdaptation adaptLearningStrategy(Handle skill_handle,
                                           int current_strategy, // Using int to avoid circular dependency
                                           const std::vector<Handle>& experience_data);

    /**
     * Optimize learning parameters for a specific context
     * @param context_description Description of learning context
     * @param current_parameters Current learning parameters
     * @param performance_feedback Recent performance feedback
     * @return Optimized parameters
     */
    std::map<std::string, double> optimizeLearningParameters(
        const std::string& context_description,
        const std::map<std::string, double>& current_parameters,
        const std::vector<double>& performance_feedback);

    /**
     * Analyze learning patterns to identify improvement opportunities
     * @param learning_history Historical learning data
     * @return Vector of identified patterns and recommendations
     */
    std::vector<std::pair<std::string, StrategyAdaptation>> 
    analyzeLearningPatterns(const std::map<std::string, std::vector<double>>& learning_history);

    /**
     * Transfer meta-learning knowledge between domains
     * @param source_domain Source domain identifier
     * @param target_domain Target domain identifier
     * @param similarity_threshold Minimum similarity for transfer
     * @return True if transfer was successful
     */
    bool transferMetaKnowledge(const std::string& source_domain,
                              const std::string& target_domain,
                              double similarity_threshold = 0.6);

    /**
     * Evaluate the effectiveness of meta-learning adaptations
     * @param skill_handle Handle to skill that was adapted
     * @param adaptation_made The adaptation that was applied
     * @param performance_before Performance before adaptation
     * @param performance_after Performance after adaptation
     */
    void evaluateAdaptationEffectiveness(Handle skill_handle,
                                        StrategyAdaptation adaptation_made,
                                        double performance_before,
                                        double performance_after);

    /**
     * Get recommended learning strategy for a new skill
     * @param skill_type Type of skill being learned
     * @param context_similarity Similarity to previous learning contexts
     * @return Recommended strategy code
     */
    int getRecommendedStrategy(const std::string& skill_type,
                              double context_similarity = 0.5);

    /**
     * Update meta-learning knowledge based on learning outcomes
     * @param learning_session_data Data from completed learning session
     * @param outcomes Outcomes and performance metrics
     */
    void updateMetaKnowledge(const std::vector<Handle>& learning_session_data,
                            const std::map<std::string, double>& outcomes);

    /**
     * Get meta-learning statistics
     * @return Map of statistic names to values
     */
    std::map<std::string, double> getMetaLearningStatistics() const;

    /**
     * Set meta-learning parameters
     * @param adaptation_threshold Threshold for triggering adaptations
     * @param min_samples Minimum samples needed before adaptation
     * @param enable_transfer Whether to enable strategy transfer
     */
    void setMetaParameters(double adaptation_threshold,
                          size_t min_samples,
                          bool enable_transfer);

    /**
     * Reset meta-learning knowledge
     */
    void reset();

private:
    void initializeMetaLearningBase();
    double calculateLearningEfficiency(const std::vector<double>& performance_history);
    double calculateAdaptationImpact(StrategyAdaptation adaptation,
                                    double before_performance,
                                    double after_performance);
    std::string getStrategyName(int strategy_code);
    StrategyAdaptation selectBestAdaptation(const std::vector<double>& performance_history,
                                          const std::vector<Handle>& context_data);
    void recordStrategyUsage(const std::string& strategy_name);
    double calculateDomainSimilarity(const std::string& domain1, const std::string& domain2);
};

} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_META_LEARNING_H