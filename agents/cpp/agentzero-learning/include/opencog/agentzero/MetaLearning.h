/*
 * opencog/agentzero/MetaLearning.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * MetaLearning - Learning how to learn more effectively
 * Part of AZ-LEARN-004: Implement MetaLearning capabilities
 */

#ifndef _OPENCOG_AGENTZERO_META_LEARNING_H
#define _OPENCOG_AGENTZERO_META_LEARNING_H

#include <memory>
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
     * @param atomspace Shared pointer to AtomSpace
     * @param config Meta-learning configuration
     */
    MetaLearning(AtomSpacePtr atomspace, const MetaLearningConfig& config = MetaLearningConfig());
    
    /**
     * Destructor
     */
    ~MetaLearning();
    
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