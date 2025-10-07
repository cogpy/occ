/*
 * opencog/agentzero/MetaLearning.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * MetaLearning - Learning how to learn more effectively
 * Part of the Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 * Part of AZ-LEARN-004: Implement MetaLearning capabilities
 */

#ifndef _OPENCOG_AGENTZERO_META_LEARNING_H
#define _OPENCOG_AGENTZERO_META_LEARNING_H

#include <memory>
#include <string>
#include <vector>
#include <map>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <vector>
#include <map>
#include <string>
#include <chrono>
#include <functional>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
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
// Forward declarations
class ExperienceManager;
class SkillAcquisition;
class PolicyOptimizer;

/**
 * Learning strategy enumeration
 * Defines different approaches to learning and adaptation
 */
enum class LearningStrategy {
    SUPERVISED,      // Learn from labeled examples
    UNSUPERVISED,    // Discover patterns without labels
    REINFORCEMENT,   // Learn through reward/punishment
    IMITATION,       // Learn by copying successful behaviors
    EXPLORATION,     // Learn through experimentation
    HYBRID,          // Adaptive combination of strategies
    META_ADAPTIVE    // Learn which strategy to use when
};

/**
 * Learning performance metrics
 * Tracks effectiveness of learning processes
 */
struct LearningMetrics {
    double learning_rate;          // Rate of improvement
    double accuracy;               // Current performance accuracy
    double efficiency;             // Resource efficiency
    double retention;              // Knowledge retention rate
    double transfer;               // Transfer learning effectiveness
    double adaptation_speed;       // Speed of adaptation to new domains
    std::chrono::milliseconds processing_time;
    size_t memory_usage;
    
    LearningMetrics() 
        : learning_rate(0.0), accuracy(0.0), efficiency(0.0), 
          retention(0.0), transfer(0.0), adaptation_speed(0.0),
          processing_time(0), memory_usage(0) {}
};

/**
 * Meta-learning configuration
 */
struct MetaLearningConfig {
    double meta_learning_rate;     // Rate of meta-level adaptation
    double exploration_factor;     // Balance between exploration/exploitation
    double strategy_switch_threshold; // Threshold for switching strategies
    size_t max_experience_history; // Maximum experiences to retain
    bool enable_transfer_learning; // Enable cross-domain transfer
    bool enable_curriculum_learning; // Enable structured learning progression
    std::chrono::milliseconds reflection_interval; // How often to reflect
    
    MetaLearningConfig()
        : meta_learning_rate(0.1), exploration_factor(0.2),
          strategy_switch_threshold(0.15), max_experience_history(10000),
          enable_transfer_learning(true), enable_curriculum_learning(true),
          reflection_interval(std::chrono::minutes(5)) {}
};

/**
 * MetaLearning - Learning how to learn more effectively
 *
 * This class implements meta-cognitive capabilities for learning optimization.
 * It analyzes learning performance across different strategies and contexts,
 * adapts learning approaches based on experience, and transfers knowledge
 * across domains.
 *
 * Key features:
 * - Strategy selection and optimization
 * - Transfer learning across domains
 * - Curriculum learning progression
 * - Performance monitoring and adaptation
 * - Integration with MOSES, ASMoses, and learn components
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
    // Learning experience representation
    struct LearningExperience {
        Handle context;                    // Context where learning occurred
        LearningStrategy strategy_used;    // Strategy that was applied
        Handle task;                       // Task that was learned
        LearningMetrics metrics;           // Performance metrics
        std::chrono::system_clock::time_point timestamp;
        Handle outcome;                    // Learning outcome atoms
        
        LearningExperience() : strategy_used(LearningStrategy::SUPERVISED) {}
    };

private:
    // Core references
    AtomSpacePtr _atomspace;
    std::shared_ptr<ExperienceManager> _experience_manager;
    std::shared_ptr<SkillAcquisition> _skill_acquisition;
    std::shared_ptr<PolicyOptimizer> _policy_optimizer;
    
    // Meta-learning state
    LearningStrategy _current_strategy;
    MetaLearningConfig _config;
    LearningMetrics _current_metrics;
    std::vector<LearningExperience> _learning_history;
    std::map<LearningStrategy, LearningMetrics> _strategy_performance;
    
    // Learning adaptation structures
    std::map<Handle, std::vector<LearningExperience>> _context_experiences;
    std::map<std::string, double> _domain_transfer_weights;
    std::vector<Handle> _curriculum_progression;
    
    // AtomSpace handles for meta-learning
    Handle _metalearning_context;
    Handle _strategy_evaluation_link;
    Handle _transfer_learning_link;

    // Internal methods - Strategy Management
    LearningStrategy selectOptimalStrategy(const Handle& context, const Handle& task);
    double evaluateStrategyEffectiveness(LearningStrategy strategy, const Handle& context);
    void updateStrategyPerformance(LearningStrategy strategy, const LearningMetrics& metrics);
    bool shouldSwitchStrategy(const Handle& context);
    
    // Internal methods - Transfer Learning
    std::vector<Handle> identifyTransferableDomains(const Handle& new_domain);
    double calculateDomainSimilarity(const Handle& domain1, const Handle& domain2);
    void transferKnowledge(const Handle& source_domain, const Handle& target_domain);
    Handle createTransferLearningAtom(const Handle& source, const Handle& target, double weight);
    
    // Internal methods - Curriculum Learning
    void updateCurriculumProgression();
    Handle selectNextLearningTask();
    bool isReadyForAdvancedTask(const Handle& task);
    void adaptCurriculumBasedOnPerformance();
    
    // Internal methods - Experience Analysis
    void analyzeRecentExperiences();
    Handle identifyLearningPatterns();
    void extractMetaKnowledge();
    void optimizeLearningParameters();
    
    // Internal methods - AtomSpace Integration
    Handle createLearningExperienceAtom(const LearningExperience& experience);
    Handle createStrategyEvaluationAtom(LearningStrategy strategy, const LearningMetrics& metrics);
    Handle createMetaLearningInsightAtom(const std::string& insight_type, double confidence);
    void recordLearningDecision(LearningStrategy strategy, const Handle& context, double outcome);

public:
    /**
     * Constructor
     * @param atomspace AtomSpace for meta-learning knowledge storage
     */
    explicit MetaLearning(AtomSpacePtr atomspace);

     * @param atomspace Shared pointer to AtomSpace
     * @param config Meta-learning configuration
     */
    MetaLearning(AtomSpacePtr atomspace, const MetaLearningConfig& config = MetaLearningConfig());
    
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
    
    /**
     * Initialize meta-learning system
     * Sets up initial state and connects to dependency components
     */
    void initialize();
    
    // Core learning operations
    /**
     * Learn from a specific task with context
     * @param task Task to learn
     * @param context Context in which learning occurs
     * @param feedback Optional feedback for supervised learning
     * @return Learning outcome atom
     */
    Handle learnTask(const Handle& task, const Handle& context, const Handle& feedback = Handle::UNDEFINED);
    
    /**
     * Adapt learning strategy based on current performance
     * @param context Current learning context
     * @return New learning strategy
     */
    LearningStrategy adaptLearningStrategy(const Handle& context);
    
    /**
     * Transfer knowledge from one domain to another
     * @param source_domain Source domain
     * @param target_domain Target domain
     * @return Transfer effectiveness score
     */
    double transferKnowledgeBetweenDomains(const Handle& source_domain, const Handle& target_domain);
    
    /**
     * Update curriculum based on learning progress
     * @return Next recommended task
     */
    Handle updateCurriculum();
    
    // Performance monitoring
    /**
     * Get current learning performance metrics
     * @return Current metrics
     */
    LearningMetrics getCurrentMetrics() const { return _current_metrics; }
    
    /**
     * Get performance metrics for a specific strategy
     * @param strategy Learning strategy to query
     * @return Strategy performance metrics
     */
    LearningMetrics getStrategyMetrics(LearningStrategy strategy) const;
    
    /**
     * Analyze learning effectiveness over time
     * @param time_window Time window to analyze
     * @return Analysis results atom
     */
    Handle analyzeLearningEffectiveness(std::chrono::hours time_window);
    
    /**
     * Get learning progress trend
     * @param time_window Time window for trend analysis
     * @return Trend analysis atom
     */
    Handle getLearningTrend(std::chrono::hours time_window);
    
    // Meta-learning reflection
    /**
     * Trigger meta-learning reflection process
     * Analyzes recent learning experiences and optimizes strategies
     * @return Reflection insights atom
     */
    Handle triggerReflection();
    
    /**
     * Learn meta-patterns from learning history
     * @param max_experiences Maximum experiences to analyze
     * @return Number of meta-patterns discovered
     */
    int learnMetaPatterns(int max_experiences = 1000);
    
    /**
     * Apply meta-learning insights to improve learning
     * @param context Current context
     * @return Number of optimizations applied
     */
    int applyMetaInsights(const Handle& context);
    
    // Strategy management
    /**
     * Set learning strategy manually
     * @param strategy Strategy to use
     */
    void setLearningStrategy(LearningStrategy strategy);
    
    /**
     * Get current learning strategy
     * @return Current strategy
     */
    LearningStrategy getCurrentStrategy() const { return _current_strategy; }
    
    /**
     * Record a learning experience
     * @param context Learning context
     * @param task Task that was learned
     * @param strategy Strategy used
     * @param success Whether learning was successful
     * @param processing_time Time taken for learning
     */
    void recordLearningExperience(const Handle& context, const Handle& task, 
                                 LearningStrategy strategy, bool success,
                                 std::chrono::milliseconds processing_time);
    
    // Configuration and control
    /**
     * Configure meta-learning parameters
     * @param config New configuration
     */
    void configure(const MetaLearningConfig& config);
    
    /**
     * Reset learning metrics and history
     */
    void reset();
    
    /**
     * Check if meta-learning system is properly initialized
     * @return True if initialized
     */
    bool isInitialized() const;
    
    // Component integration
    /**
     * Set experience manager component
     * @param experience_manager Shared pointer to experience manager
     */
    void setExperienceManager(std::shared_ptr<ExperienceManager> experience_manager);
    
    /**
     * Set skill acquisition component
     * @param skill_acquisition Shared pointer to skill acquisition
     */
    void setSkillAcquisition(std::shared_ptr<SkillAcquisition> skill_acquisition);
    
    /**
     * Set policy optimizer component
     * @param policy_optimizer Shared pointer to policy optimizer
     */
    void setPolicyOptimizer(std::shared_ptr<PolicyOptimizer> policy_optimizer);
    
    // Utility methods
    /**
     * Convert learning strategy to string
     * @param strategy Strategy to convert
     * @return String representation
     */
    static std::string strategyToString(LearningStrategy strategy);
    
    /**
     * Convert string to learning strategy
     * @param strategy_str String representation
     * @return Learning strategy
     */
    static LearningStrategy stringToStrategy(const std::string& strategy_str);
};

} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_META_LEARNING_H