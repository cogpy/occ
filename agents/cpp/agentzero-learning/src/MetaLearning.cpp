/*
 * opencog/agentzero/MetaLearning.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * MetaLearning - Learning how to learn more effectively
 * Part of AZ-LEARN-004: Implement MetaLearning capabilities
 */

#include "opencog/agentzero/MetaLearning.h"
#include "opencog/agentzero/ExperienceManager.h"
#include "opencog/agentzero/SkillAcquisition.h"
#include "opencog/agentzero/PolicyOptimizer.h"

#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/util/random.h>

#include <algorithm>
#include <numeric>
#include <sstream>

using namespace opencog;
using namespace opencog::agentzero;

// Constructor
MetaLearning::MetaLearning(AtomSpacePtr atomspace, const MetaLearningConfig& config)
    : _atomspace(atomspace)
    , _current_strategy(LearningStrategy::META_ADAPTIVE)
    , _config(config)
    , _metalearning_context(Handle::UNDEFINED)
    , _strategy_evaluation_link(Handle::UNDEFINED)
    , _transfer_learning_link(Handle::UNDEFINED)
{
    logger().info() << "[MetaLearning] Initializing meta-learning system with " 
                    << strategyToString(_current_strategy) << " strategy";
}

// Destructor
MetaLearning::~MetaLearning()
{
    logger().info() << "[MetaLearning] Shutting down meta-learning system";
}

// Initialize meta-learning system
void MetaLearning::initialize()
{
    logger().info() << "[MetaLearning] Initializing meta-learning components";
    
    // Create meta-learning context in AtomSpace
    _metalearning_context = _atomspace->add_node(CONCEPT_NODE, "MetaLearningContext");
    
    // Initialize strategy evaluation link
    _strategy_evaluation_link = _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "StrategyEvaluation"),
        _metalearning_context);
        
    // Initialize transfer learning link
    _transfer_learning_link = _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "TransferLearning"),
        _metalearning_context);
    
    // Initialize strategy performance tracking
    for (int i = 0; i < static_cast<int>(LearningStrategy::META_ADAPTIVE) + 1; ++i) {
        LearningStrategy strategy = static_cast<LearningStrategy>(i);
        _strategy_performance[strategy] = LearningMetrics();
    }
    
    logger().info() << "[MetaLearning] Meta-learning system initialized successfully";
}

// Core learning operations
Handle MetaLearning::learnTask(const Handle& task, const Handle& context, const Handle& feedback)
{
    if (task == Handle::UNDEFINED) {
        logger().warn() << "[MetaLearning] Cannot learn undefined task";
        return Handle::UNDEFINED;
    }
    
    auto start_time = std::chrono::high_resolution_clock::now();
    
    // Select optimal learning strategy for this context and task
    LearningStrategy optimal_strategy = selectOptimalStrategy(context, task);
    
    // Create learning outcome atom
    Handle outcome_atom = _atomspace->add_node(CONCEPT_NODE, 
        "LearningOutcome_" + std::to_string(rand()));
    
    // Simulate learning process based on strategy
    bool learning_success = false;
    double learning_effectiveness = 0.0;
    
    switch (optimal_strategy) {
        case LearningStrategy::SUPERVISED:
            learning_success = (feedback != Handle::UNDEFINED);
            learning_effectiveness = learning_success ? 0.8 : 0.3;
            break;
            
        case LearningStrategy::REINFORCEMENT:
            learning_success = (randGen().randdouble() > 0.4);
            learning_effectiveness = learning_success ? 0.7 : 0.2;
            break;
            
        case LearningStrategy::IMITATION:
            learning_success = (randGen().randdouble() > 0.3);
            learning_effectiveness = learning_success ? 0.6 : 0.4;
            break;
            
        case LearningStrategy::EXPLORATION:
            learning_success = (randGen().randdouble() > 0.5);
            learning_effectiveness = learning_success ? 0.9 : 0.1;
            break;
            
        case LearningStrategy::HYBRID:
        case LearningStrategy::META_ADAPTIVE:
            learning_success = (randGen().randdouble() > 0.25);
            learning_effectiveness = learning_success ? 0.85 : 0.3;
            break;
            
        default:
            learning_success = (randGen().randdouble() > 0.6);
            learning_effectiveness = learning_success ? 0.5 : 0.2;
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto processing_time = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    
    // Record learning experience
    recordLearningExperience(context, task, optimal_strategy, learning_success, processing_time);
    
    // Update metrics
    _current_metrics.accuracy = learning_effectiveness;
    _current_metrics.processing_time = processing_time;
    
    // Create learning result link
    Handle result_link = _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "LearningResult"),
        _atomspace->add_link(LIST_LINK, task, outcome_atom));
    
    logger().info() << "[MetaLearning] Learned task using " << strategyToString(optimal_strategy) 
                    << " strategy, success: " << learning_success 
                    << ", effectiveness: " << learning_effectiveness;
    
    return outcome_atom;
}

// Adapt learning strategy based on current performance
LearningStrategy MetaLearning::adaptLearningStrategy(const Handle& context)
{
    // Analyze recent performance
    analyzeRecentExperiences();
    
    // Check if current strategy is performing well
    if (_current_metrics.accuracy > 0.7 && !shouldSwitchStrategy(context)) {
        return _current_strategy;
    }
    
    // Select new optimal strategy
    LearningStrategy new_strategy = selectOptimalStrategy(context, Handle::UNDEFINED);
    
    if (new_strategy != _current_strategy) {
        logger().info() << "[MetaLearning] Switching strategy from " 
                        << strategyToString(_current_strategy) << " to " 
                        << strategyToString(new_strategy);
        _current_strategy = new_strategy;
    }
    
    return _current_strategy;
}

// Transfer knowledge between domains
double MetaLearning::transferKnowledgeBetweenDomains(const Handle& source_domain, const Handle& target_domain)
{
    if (source_domain == Handle::UNDEFINED || target_domain == Handle::UNDEFINED) {
        logger().warn() << "[MetaLearning] Cannot transfer knowledge between undefined domains";
        return 0.0;
    }
    
    // Calculate domain similarity
    double similarity = calculateDomainSimilarity(source_domain, target_domain);
    
    if (similarity < 0.3) {
        logger().info() << "[MetaLearning] Domains too dissimilar for effective transfer: " << similarity;
        return similarity;
    }
    
    // Perform knowledge transfer
    transferKnowledge(source_domain, target_domain);
    
    // Update transfer weights
    std::string transfer_key = source_domain->to_string() + "->" + target_domain->to_string();
    _domain_transfer_weights[transfer_key] = similarity;
    
    // Create transfer learning atom
    Handle transfer_atom = createTransferLearningAtom(source_domain, target_domain, similarity);
    
    logger().info() << "[MetaLearning] Successfully transferred knowledge between domains, "
                    << "similarity: " << similarity;
    
    return similarity;
}

// Update curriculum based on learning progress
Handle MetaLearning::updateCurriculum()
{
    adaptCurriculumBasedOnPerformance();
    
    Handle next_task = selectNextLearningTask();
    
    if (next_task != Handle::UNDEFINED) {
        _curriculum_progression.push_back(next_task);
        logger().info() << "[MetaLearning] Updated curriculum, next task selected";
    }
    
    return next_task;
}

// Get performance metrics for a specific strategy
LearningMetrics MetaLearning::getStrategyMetrics(LearningStrategy strategy) const
{
    auto it = _strategy_performance.find(strategy);
    if (it != _strategy_performance.end()) {
        return it->second;
    }
    return LearningMetrics();
}

// Analyze learning effectiveness over time
Handle MetaLearning::analyzeLearningEffectiveness(std::chrono::hours time_window)
{
    auto cutoff_time = std::chrono::system_clock::now() - time_window;
    
    // Filter recent experiences
    std::vector<LearningExperience> recent_experiences;
    for (const auto& exp : _learning_history) {
        if (exp.timestamp >= cutoff_time) {
            recent_experiences.push_back(exp);
        }
    }
    
    if (recent_experiences.empty()) {
        return Handle::UNDEFINED;
    }
    
    // Calculate effectiveness metrics
    double avg_accuracy = 0.0;
    double avg_efficiency = 0.0;
    std::map<LearningStrategy, int> strategy_counts;
    
    for (const auto& exp : recent_experiences) {
        avg_accuracy += exp.metrics.accuracy;
        avg_efficiency += exp.metrics.efficiency;
        strategy_counts[exp.strategy_used]++;
    }
    
    avg_accuracy /= recent_experiences.size();
    avg_efficiency /= recent_experiences.size();
    
    // Create analysis result atom
    Handle analysis_atom = _atomspace->add_node(CONCEPT_NODE, 
        "LearningEffectivenessAnalysis_" + std::to_string(rand()));
    
    // Add metrics as properties
    _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "AverageAccuracy"),
        _atomspace->add_link(LIST_LINK, analysis_atom, 
            _atomspace->add_node(NUMBER_NODE, std::to_string(avg_accuracy))));
    
    logger().info() << "[MetaLearning] Analyzed " << recent_experiences.size() 
                    << " experiences, avg accuracy: " << avg_accuracy;
    
    return analysis_atom;
}

// Get learning progress trend
Handle MetaLearning::getLearningTrend(std::chrono::hours time_window)
{
    // Similar to analyzeLearningEffectiveness but focuses on trends
    auto analysis_atom = analyzeLearningEffectiveness(time_window);
    
    if (analysis_atom == Handle::UNDEFINED) {
        return Handle::UNDEFINED;
    }
    
    // Create trend atom
    Handle trend_atom = _atomspace->add_node(CONCEPT_NODE, 
        "LearningTrend_" + std::to_string(rand()));
    
    // Link to analysis
    _atomspace->add_link(INHERITANCE_LINK, trend_atom, analysis_atom);
    
    return trend_atom;
}

// Trigger meta-learning reflection process
Handle MetaLearning::triggerReflection()
{
    logger().info() << "[MetaLearning] Triggering meta-learning reflection";
    
    // Analyze recent experiences
    analyzeRecentExperiences();
    
    // Identify learning patterns
    Handle patterns = identifyLearningPatterns();
    
    // Extract meta-knowledge
    extractMetaKnowledge();
    
    // Optimize learning parameters
    optimizeLearningParameters();
    
    // Create reflection insights atom
    Handle insights_atom = _atomspace->add_node(CONCEPT_NODE, 
        "MetaLearningInsights_" + std::to_string(rand()));
    
    if (patterns != Handle::UNDEFINED) {
        _atomspace->add_link(EVALUATION_LINK,
            _atomspace->add_node(PREDICATE_NODE, "ReflectionPatterns"),
            _atomspace->add_link(LIST_LINK, insights_atom, patterns));
    }
    
    return insights_atom;
}

// Learn meta-patterns from learning history
int MetaLearning::learnMetaPatterns(int max_experiences)
{
    int patterns_learned = 0;
    size_t experiences_to_analyze = std::min(static_cast<size_t>(max_experiences), _learning_history.size());
    
    if (experiences_to_analyze < 10) {
        logger().info() << "[MetaLearning] Insufficient experiences for pattern learning: " 
                        << experiences_to_analyze;
        return 0;
    }
    
    // Analyze patterns in strategy effectiveness
    std::map<std::pair<LearningStrategy, std::string>, std::vector<double>> context_strategy_performance;
    
    for (size_t i = _learning_history.size() - experiences_to_analyze; i < _learning_history.size(); ++i) {
        const auto& exp = _learning_history[i];
        std::string context_str = exp.context != Handle::UNDEFINED ? exp.context->to_string() : "unknown";
        auto key = std::make_pair(exp.strategy_used, context_str);
        context_strategy_performance[key].push_back(exp.metrics.accuracy);
    }
    
    // Identify effective strategy-context combinations
    for (const auto& [key, performances] : context_strategy_performance) {
        if (performances.size() >= 3) {
            double avg_performance = std::accumulate(performances.begin(), performances.end(), 0.0) / performances.size();
            if (avg_performance > 0.7) {
                patterns_learned++;
                logger().debug() << "[MetaLearning] Learned pattern: " 
                                << strategyToString(key.first) << " effective in context " << key.second;
            }
        }
    }
    
    logger().info() << "[MetaLearning] Learned " << patterns_learned << " meta-patterns from " 
                    << experiences_to_analyze << " experiences";
    
    return patterns_learned;
}

// Apply meta-learning insights to improve learning
int MetaLearning::applyMetaInsights(const Handle& context)
{
    int optimizations_applied = 0;
    
    // Apply insights from pattern learning
    if (!_learning_history.empty()) {
        // Adjust learning rate based on recent performance
        double recent_avg_accuracy = 0.0;
        int recent_count = 0;
        
        for (auto it = _learning_history.rbegin(); it != _learning_history.rend() && recent_count < 10; ++it, ++recent_count) {
            recent_avg_accuracy += it->metrics.accuracy;
        }
        
        if (recent_count > 0) {
            recent_avg_accuracy /= recent_count;
            
            if (recent_avg_accuracy < 0.5) {
                _config.meta_learning_rate = std::min(1.0, _config.meta_learning_rate * 1.2);
                optimizations_applied++;
            } else if (recent_avg_accuracy > 0.8) {
                _config.meta_learning_rate = std::max(0.01, _config.meta_learning_rate * 0.9);
                optimizations_applied++;
            }
        }
    }
    
    logger().info() << "[MetaLearning] Applied " << optimizations_applied << " meta-learning optimizations";
    
    return optimizations_applied;
}

// Strategy management
void MetaLearning::setLearningStrategy(LearningStrategy strategy)
{
    _current_strategy = strategy;
    logger().info() << "[MetaLearning] Learning strategy set to " << strategyToString(strategy);
}

// Record a learning experience
void MetaLearning::recordLearningExperience(const Handle& context, const Handle& task, 
                                           LearningStrategy strategy, bool success,
                                           std::chrono::milliseconds processing_time)
{
    LearningExperience experience;
    experience.context = context;
    experience.task = task;
    experience.strategy_used = strategy;
    experience.metrics.accuracy = success ? 0.8 : 0.2;
    experience.metrics.processing_time = processing_time;
    experience.timestamp = std::chrono::system_clock::now();
    experience.outcome = success ? _atomspace->add_node(CONCEPT_NODE, "Success") :
                                  _atomspace->add_node(CONCEPT_NODE, "Failure");
    
    _learning_history.push_back(experience);
    
    // Maintain history size limit
    if (_learning_history.size() > _config.max_experience_history) {
        _learning_history.erase(_learning_history.begin());
    }
    
    // Update strategy performance
    updateStrategyPerformance(strategy, experience.metrics);
    
    // Create experience atom in AtomSpace
    Handle experience_atom = createLearningExperienceAtom(experience);
    
    logger().debug() << "[MetaLearning] Recorded learning experience, success: " << success 
                     << ", strategy: " << strategyToString(strategy);
}

// Configuration and control
void MetaLearning::configure(const MetaLearningConfig& config)
{
    _config = config;
    logger().info() << "[MetaLearning] Configuration updated";
}

void MetaLearning::reset()
{
    _learning_history.clear();
    _strategy_performance.clear();
    _context_experiences.clear();
    _domain_transfer_weights.clear();
    _curriculum_progression.clear();
    _current_metrics = LearningMetrics();
    
    logger().info() << "[MetaLearning] Meta-learning system reset";
}

bool MetaLearning::isInitialized() const
{
    return _metalearning_context != Handle::UNDEFINED && _atomspace != nullptr;
}

// Component integration
void MetaLearning::setExperienceManager(std::shared_ptr<ExperienceManager> experience_manager)
{
    _experience_manager = experience_manager;
}

void MetaLearning::setSkillAcquisition(std::shared_ptr<SkillAcquisition> skill_acquisition)
{
    _skill_acquisition = skill_acquisition;
}

void MetaLearning::setPolicyOptimizer(std::shared_ptr<PolicyOptimizer> policy_optimizer)
{
    _policy_optimizer = policy_optimizer;
}

// Utility methods
std::string MetaLearning::strategyToString(LearningStrategy strategy)
{
    switch (strategy) {
        case LearningStrategy::SUPERVISED: return "SUPERVISED";
        case LearningStrategy::UNSUPERVISED: return "UNSUPERVISED";
        case LearningStrategy::REINFORCEMENT: return "REINFORCEMENT";
        case LearningStrategy::IMITATION: return "IMITATION";
        case LearningStrategy::EXPLORATION: return "EXPLORATION";
        case LearningStrategy::HYBRID: return "HYBRID";
        case LearningStrategy::META_ADAPTIVE: return "META_ADAPTIVE";
        default: return "UNKNOWN";
    }
}

LearningStrategy MetaLearning::stringToStrategy(const std::string& strategy_str)
{
    if (strategy_str == "SUPERVISED") return LearningStrategy::SUPERVISED;
    if (strategy_str == "UNSUPERVISED") return LearningStrategy::UNSUPERVISED;
    if (strategy_str == "REINFORCEMENT") return LearningStrategy::REINFORCEMENT;
    if (strategy_str == "IMITATION") return LearningStrategy::IMITATION;
    if (strategy_str == "EXPLORATION") return LearningStrategy::EXPLORATION;
    if (strategy_str == "HYBRID") return LearningStrategy::HYBRID;
    if (strategy_str == "META_ADAPTIVE") return LearningStrategy::META_ADAPTIVE;
    return LearningStrategy::SUPERVISED; // Default
}

// Private implementation methods
LearningStrategy MetaLearning::selectOptimalStrategy(const Handle& context, const Handle& task)
{
    // If no context provided, use current strategy
    if (context == Handle::UNDEFINED) {
        return _current_strategy;
    }
    
    // Analyze context and select best strategy
    std::string context_str = context->to_string();
    
    // Find best performing strategy for this context
    LearningStrategy best_strategy = _current_strategy;
    double best_performance = 0.0;
    
    for (const auto& [strategy, metrics] : _strategy_performance) {
        if (metrics.accuracy > best_performance) {
            best_performance = metrics.accuracy;
            best_strategy = strategy;
        }
    }
    
    return best_strategy;
}

double MetaLearning::evaluateStrategyEffectiveness(LearningStrategy strategy, const Handle& context)
{
    auto it = _strategy_performance.find(strategy);
    if (it != _strategy_performance.end()) {
        return it->second.accuracy;
    }
    return 0.5; // Default effectiveness
}

void MetaLearning::updateStrategyPerformance(LearningStrategy strategy, const LearningMetrics& metrics)
{
    if (_strategy_performance.find(strategy) == _strategy_performance.end()) {
        _strategy_performance[strategy] = metrics;
    } else {
        // Update with exponential moving average
        auto& current = _strategy_performance[strategy];
        double alpha = 0.1; // Learning rate for moving average
        current.accuracy = alpha * metrics.accuracy + (1 - alpha) * current.accuracy;
        current.efficiency = alpha * metrics.efficiency + (1 - alpha) * current.efficiency;
        current.learning_rate = alpha * metrics.learning_rate + (1 - alpha) * current.learning_rate;
    }
}

bool MetaLearning::shouldSwitchStrategy(const Handle& context)
{
    if (_learning_history.size() < 5) {
        return false; // Need more data
    }
    
    // Check recent performance
    double recent_performance = 0.0;
    int count = 0;
    
    for (auto it = _learning_history.rbegin(); it != _learning_history.rend() && count < 5; ++it, ++count) {
        recent_performance += it->metrics.accuracy;
    }
    
    recent_performance /= count;
    
    return recent_performance < _config.strategy_switch_threshold;
}

double MetaLearning::calculateDomainSimilarity(const Handle& domain1, const Handle& domain2)
{
    if (domain1 == Handle::UNDEFINED || domain2 == Handle::UNDEFINED) {
        return 0.0;
    }
    
    // Simple similarity based on atom types and names
    if (domain1->get_type() == domain2->get_type()) {
        std::string name1 = domain1->to_string();
        std::string name2 = domain2->to_string();
        
        // Simple string similarity (could be enhanced with more sophisticated methods)
        size_t common_chars = 0;
        size_t min_length = std::min(name1.length(), name2.length());
        
        for (size_t i = 0; i < min_length; ++i) {
            if (name1[i] == name2[i]) {
                common_chars++;
            }
        }
        
        return static_cast<double>(common_chars) / std::max(name1.length(), name2.length());
    }
    
    return 0.3; // Different atom types have some base similarity
}

void MetaLearning::transferKnowledge(const Handle& source_domain, const Handle& target_domain)
{
    // Create transfer learning representation
    Handle transfer_link = _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "KnowledgeTransfer"),
        _atomspace->add_link(LIST_LINK, source_domain, target_domain));
        
    logger().debug() << "[MetaLearning] Created knowledge transfer link between domains";
}

Handle MetaLearning::createTransferLearningAtom(const Handle& source, const Handle& target, double weight)
{
    Handle weight_atom = _atomspace->add_node(NUMBER_NODE, std::to_string(weight));
    
    return _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "TransferWeight"),
        _atomspace->add_link(LIST_LINK, source, target, weight_atom));
}

void MetaLearning::analyzeRecentExperiences()
{
    if (_learning_history.size() < 10) {
        return;
    }
    
    // Analyze last 10 experiences
    double total_accuracy = 0.0;
    auto recent_start = _learning_history.end() - 10;
    
    for (auto it = recent_start; it != _learning_history.end(); ++it) {
        total_accuracy += it->metrics.accuracy;
    }
    
    _current_metrics.accuracy = total_accuracy / 10.0;
}

Handle MetaLearning::identifyLearningPatterns()
{
    // Create pattern analysis atom
    Handle pattern_atom = _atomspace->add_node(CONCEPT_NODE, 
        "LearningPattern_" + std::to_string(rand()));
    
    return pattern_atom;
}

void MetaLearning::extractMetaKnowledge()
{
    // Extract insights from learning history
    logger().debug() << "[MetaLearning] Extracting meta-knowledge from " 
                     << _learning_history.size() << " experiences";
}

void MetaLearning::optimizeLearningParameters()
{
    // Optimize parameters based on performance
    if (_current_metrics.accuracy < 0.5) {
        _config.exploration_factor = std::min(1.0, _config.exploration_factor * 1.1);
    } else if (_current_metrics.accuracy > 0.8) {
        _config.exploration_factor = std::max(0.1, _config.exploration_factor * 0.9);
    }
}

Handle MetaLearning::createLearningExperienceAtom(const LearningExperience& experience)
{
    Handle experience_atom = _atomspace->add_node(CONCEPT_NODE, 
        "LearningExperience_" + std::to_string(rand()));
    
    // Add properties
    if (experience.context != Handle::UNDEFINED) {
        _atomspace->add_link(EVALUATION_LINK,
            _atomspace->add_node(PREDICATE_NODE, "ExperienceContext"),
            _atomspace->add_link(LIST_LINK, experience_atom, experience.context));
    }
    
    if (experience.task != Handle::UNDEFINED) {
        _atomspace->add_link(EVALUATION_LINK,
            _atomspace->add_node(PREDICATE_NODE, "ExperienceTask"),
            _atomspace->add_link(LIST_LINK, experience_atom, experience.task));
    }
    
    return experience_atom;
}

void MetaLearning::updateCurriculumProgression()
{
    // Update curriculum based on current performance
    logger().debug() << "[MetaLearning] Updating curriculum progression";
}

Handle MetaLearning::selectNextLearningTask()
{
    // Select next task in curriculum
    return _atomspace->add_node(CONCEPT_NODE, "NextTask_" + std::to_string(rand()));
}

bool MetaLearning::isReadyForAdvancedTask(const Handle& task)
{
    return _current_metrics.accuracy > 0.7;
}

void MetaLearning::adaptCurriculumBasedOnPerformance()
{
    // Adapt curriculum progression
    if (_current_metrics.accuracy > 0.8) {
        logger().debug() << "[MetaLearning] Performance good, advancing curriculum";
    } else if (_current_metrics.accuracy < 0.5) {
        logger().debug() << "[MetaLearning] Performance poor, simplifying curriculum";
    }
}