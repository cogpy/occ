/*
 * src/MetaLearning.cpp
 * opencog/agentzero/MetaLearning.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * MetaLearning Implementation
 * Part of the Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 */

#include <algorithm>
#include <cmath>
#include <sstream>
#include <stdexcept>

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>

#include "opencog/agentzero/MetaLearning.h"
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
/**
 * MetaLearning.cpp
 *
 * Learning how to learn more effectively
 * Part of Agent-Zero Learning & Adaptation Phase 5
 *
 * Copyright (C) 2024 OpenCog Foundation
 */

#include "agentzero-learning/MetaLearning.h"
#include <opencog/util/Logger.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/atom_types/atom_types.h>

using namespace opencog;
using namespace opencog::agentzero;

MetaLearning::MetaLearning(AtomSpacePtr atomspace)
    : _atomspace(atomspace)
    , _meta_learning_base(Handle::UNDEFINED)
    , _adaptation_threshold(0.1)
    , _min_samples_for_adaptation(5)
    , _enable_strategy_transfer(true)
{
    if (!_atomspace) {
        throw std::runtime_error("MetaLearning requires valid AtomSpace");
    }

    logger().info() << "[MetaLearning] Initializing meta-learning system";
    initializeMetaLearningBase();
    logger().info() << "[MetaLearning] Meta-learning system initialized successfully";
}

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

MetaLearning::StrategyAdaptation MetaLearning::adaptLearningStrategy(
    Handle skill_handle,
    int current_strategy,
    const std::vector<Handle>& experience_data)
{
    if (skill_handle == Handle::UNDEFINED) {
        logger().error() << "[MetaLearning] Cannot adapt strategy for undefined skill";
        return StrategyAdaptation::ADJUST_LEARNING_RATE; // Default fallback
    }

    logger().debug() << "[MetaLearning] Adapting learning strategy for skill";

    std::string skill_name = skill_handle->get_name();
    
    // Get performance history for this skill
    std::vector<double> performance_history;
    auto it = _learning_performance_history.find(skill_name);
    if (it != _learning_performance_history.end()) {
        performance_history = it->second;
    }

    // Check if we have enough data for adaptation
    if (performance_history.size() < _min_samples_for_adaptation) {
        logger().debug() << "[MetaLearning] Insufficient data for adaptation, using default";
        return StrategyAdaptation::ADJUST_LEARNING_RATE;
    }

    // Analyze recent performance trends
    double recent_improvement = 0.0;
    if (performance_history.size() >= 2) {
        size_t recent_window = std::min(static_cast<size_t>(5), performance_history.size());
        double recent_avg = 0.0, earlier_avg = 0.0;
        
        // Calculate recent average
        for (size_t i = performance_history.size() - recent_window; i < performance_history.size(); ++i) {
            recent_avg += performance_history[i];
        }
        recent_avg /= recent_window;
        
        // Calculate earlier average
        size_t earlier_start = std::max(static_cast<size_t>(0), 
                                       performance_history.size() - 2 * recent_window);
        size_t earlier_end = performance_history.size() - recent_window;
        if (earlier_end > earlier_start) {
            for (size_t i = earlier_start; i < earlier_end; ++i) {
                earlier_avg += performance_history[i];
            }
            earlier_avg /= (earlier_end - earlier_start);
            recent_improvement = recent_avg - earlier_avg;
        }
    }

    // Select adaptation based on performance trends and current strategy
    StrategyAdaptation recommended_adaptation = selectBestAdaptation(performance_history, experience_data);

    // Record strategy usage
    std::string strategy_name = getStrategyName(current_strategy);
    recordStrategyUsage(strategy_name);

    // Update learning efficiency for this skill
    double efficiency = calculateLearningEfficiency(performance_history);
    _learning_efficiency_scores[skill_name] = efficiency;

    logger().debug() << "[MetaLearning] Strategy adaptation recommended: " 
                    << static_cast<int>(recommended_adaptation)
                    << " (efficiency: " << efficiency << ", improvement: " << recent_improvement << ")";

    return recommended_adaptation;
}

std::map<std::string, double> MetaLearning::optimizeLearningParameters(
    const std::string& context_description,
    const std::map<std::string, double>& current_parameters,
    const std::vector<double>& performance_feedback)
{
    logger().debug() << "[MetaLearning] Optimizing learning parameters for context: " << context_description;

    std::map<std::string, double> optimized_parameters = current_parameters;

    if (performance_feedback.empty()) {
        logger().warn() << "[MetaLearning] No performance feedback available for optimization";
        return optimized_parameters;
    }

    // Calculate recent performance trend
    double performance_trend = 0.0;
    if (performance_feedback.size() >= 2) {
        double recent_perf = 0.0, earlier_perf = 0.0;
        size_t window = std::min(static_cast<size_t>(3), performance_feedback.size() / 2);
        
        // Recent performance
        for (size_t i = performance_feedback.size() - window; i < performance_feedback.size(); ++i) {
            recent_perf += performance_feedback[i];
        }
        recent_perf /= window;
        
        // Earlier performance
        for (size_t i = 0; i < window; ++i) {
            earlier_perf += performance_feedback[i];
        }
        earlier_perf /= window;
        
        performance_trend = recent_perf - earlier_perf;
    }

    // Optimize learning rate
    auto lr_it = optimized_parameters.find("learning_rate");
    if (lr_it != optimized_parameters.end()) {
        double current_lr = lr_it->second;
        
        if (performance_trend < -_adaptation_threshold) {
            // Performance declining - increase learning rate
            optimized_parameters["learning_rate"] = std::min(1.0, current_lr * 1.2);
        } else if (performance_trend > _adaptation_threshold) {
            // Performance improving - slightly decrease for fine-tuning
            optimized_parameters["learning_rate"] = std::max(0.001, current_lr * 0.9);
        }
    }

    // Optimize exploration rate
    auto exp_it = optimized_parameters.find("exploration_rate");
    if (exp_it != optimized_parameters.end()) {
        double current_exp = exp_it->second;
        double avg_performance = 0.0;
        for (double perf : performance_feedback) {
            avg_performance += perf;
        }
        avg_performance /= performance_feedback.size();
        
        if (avg_performance < 0.5) {
            // Low performance - increase exploration
            optimized_parameters["exploration_rate"] = std::min(1.0, current_exp * 1.1);
        } else if (avg_performance > 0.8) {
            // High performance - decrease exploration
            optimized_parameters["exploration_rate"] = std::max(0.01, current_exp * 0.8);
        }
    }

    // Update parameter history
    _current_learning_rates[context_description] = optimized_parameters["learning_rate"];

    logger().debug() << "[MetaLearning] Parameter optimization complete for " << context_description;
    return optimized_parameters;
}

std::vector<std::pair<std::string, MetaLearning::StrategyAdaptation>> 
MetaLearning::analyzeLearningPatterns(const std::map<std::string, std::vector<double>>& learning_history)
{
    std::vector<std::pair<std::string, StrategyAdaptation>> recommendations;

    logger().debug() << "[MetaLearning] Analyzing learning patterns across " 
                    << learning_history.size() << " contexts";

    for (const auto& context_pair : learning_history) {
        const std::string& context = context_pair.first;
        const std::vector<double>& history = context_pair.second;

        if (history.size() < 3) {
            continue; // Need minimum history for pattern analysis
        }

        // Analyze learning curve shape
        double initial_performance = history.front();
        double final_performance = history.back();
        double max_performance = *std::max_element(history.begin(), history.end());
        
        // Calculate learning rate (slope of improvement)
        double learning_rate = (final_performance - initial_performance) / history.size();
        
        // Identify plateaus (periods of little improvement)
        int plateau_count = 0;
        for (size_t i = 1; i < history.size(); ++i) {
            if (std::abs(history[i] - history[i-1]) < 0.05) {
                plateau_count++;
            }
        }
        double plateau_ratio = static_cast<double>(plateau_count) / history.size();

        // Generate recommendations based on patterns
        if (learning_rate < 0.01 && plateau_ratio > 0.6) {
            recommendations.push_back({context, StrategyAdaptation::INCREASE_EXPLORATION});
        } else if (final_performance < 0.5 && learning_rate < 0.02) {
            recommendations.push_back({context, StrategyAdaptation::CHANGE_REPRESENTATION});
        } else if (max_performance - final_performance > 0.2) {
            recommendations.push_back({context, StrategyAdaptation::IMPROVE_RETENTION});
        } else if (learning_rate > 0.1 && final_performance > 0.8) {
            recommendations.push_back({context, StrategyAdaptation::TRANSFER_KNOWLEDGE});
        }
    }

    logger().debug() << "[MetaLearning] Pattern analysis complete, generated " 
                    << recommendations.size() << " recommendations";

    return recommendations;
}

bool MetaLearning::transferMetaKnowledge(const std::string& source_domain,
                                        const std::string& target_domain,
                                        double similarity_threshold)
{
    if (!_enable_strategy_transfer) {
        logger().debug() << "[MetaLearning] Strategy transfer is disabled";
        return false;
    }

    logger().info() << "[MetaLearning] Attempting knowledge transfer from " 
                   << source_domain << " to " << target_domain;

    // Calculate domain similarity
    double similarity = calculateDomainSimilarity(source_domain, target_domain);
    
    if (similarity < similarity_threshold) {
        logger().debug() << "[MetaLearning] Domains not similar enough for transfer "
                        << "(similarity: " << similarity << ", threshold: " << similarity_threshold << ")";
        return false;
    }

    // Transfer learning parameters
    auto source_lr = _current_learning_rates.find(source_domain);
    if (source_lr != _current_learning_rates.end()) {
        _current_learning_rates[target_domain] = source_lr->second * 0.8; // Slightly reduced for safety
    }

    // Transfer efficiency knowledge
    auto source_efficiency = _learning_efficiency_scores.find(source_domain);
    if (source_efficiency != _learning_efficiency_scores.end()) {
        _learning_efficiency_scores[target_domain] = source_efficiency->second * 0.9;
    }

    // Transfer performance history (scaled)
    auto source_history = _learning_performance_history.find(source_domain);
    if (source_history != _learning_performance_history.end()) {
        std::vector<double> transferred_history;
        for (double value : source_history->second) {
            transferred_history.push_back(value * 0.7); // Conservative transfer
        }
        _learning_performance_history[target_domain] = transferred_history;
    }

    logger().info() << "[MetaLearning] Knowledge transfer successful (similarity: " << similarity << ")";
    return true;
}

void MetaLearning::evaluateAdaptationEffectiveness(Handle skill_handle,
                                                  StrategyAdaptation adaptation_made,
                                                  double performance_before,
                                                  double performance_after)
{
    if (skill_handle == Handle::UNDEFINED) {
        return;
    }

    std::string skill_name = skill_handle->get_name();
    double impact = calculateAdaptationImpact(adaptation_made, performance_before, performance_after);

    // Record adaptation effectiveness
    std::string adaptation_key = skill_name + "_" + std::to_string(static_cast<int>(adaptation_made));
    _adaptation_effectiveness[adaptation_key].push_back(impact);

    // Update learning performance history
    _learning_performance_history[skill_name].push_back(performance_after);

    logger().debug() << "[MetaLearning] Adaptation effectiveness recorded for " << skill_name
                    << " (impact: " << impact << ")";

    // Keep history manageable
    if (_learning_performance_history[skill_name].size() > 100) {
        _learning_performance_history[skill_name].erase(
            _learning_performance_history[skill_name].begin());
    }
}

int MetaLearning::getRecommendedStrategy(const std::string& skill_type,
                                        double context_similarity)
{
    logger().debug() << "[MetaLearning] Getting recommended strategy for skill type: " << skill_type;

    // Strategy mapping (simplified)
    // In practice, this would be based on extensive meta-learning analysis
    if (skill_type.find("motor") != std::string::npos || 
        skill_type.find("physical") != std::string::npos) {
        return 0; // IMITATION strategy for motor skills
    } else if (skill_type.find("cognitive") != std::string::npos ||
               skill_type.find("reasoning") != std::string::npos) {
        return 1; // REINFORCEMENT strategy for cognitive skills
    } else if (skill_type.find("creative") != std::string::npos) {
        return 2; // EXPLORATORY strategy for creative skills
    } else if (context_similarity > 0.7) {
        return 3; // TRANSFER strategy when context is similar
    } else {
        return 1; // Default to REINFORCEMENT
    }
}

void MetaLearning::updateMetaKnowledge(const std::vector<Handle>& learning_session_data,
                                      const std::map<std::string, double>& outcomes)
{
    logger().debug() << "[MetaLearning] Updating meta-knowledge with " 
                    << learning_session_data.size() << " data points";

    // Extract learning context information
    std::string context_key = "session_" + std::to_string(learning_session_data.size());
    
    // Update performance tracking
    auto performance_it = outcomes.find("performance");
    if (performance_it != outcomes.end()) {
        _learning_performance_history[context_key].push_back(performance_it->second);
    }

    // Update learning rate tracking
    auto lr_it = outcomes.find("learning_rate");
    if (lr_it != outcomes.end()) {
        _current_learning_rates[context_key] = lr_it->second;
    }

    // Calculate and store efficiency
    auto time_it = outcomes.find("learning_time");
    auto accuracy_it = outcomes.find("final_accuracy");
    if (time_it != outcomes.end() && accuracy_it != outcomes.end()) {
        double efficiency = accuracy_it->second / (1.0 + time_it->second);
        _learning_efficiency_scores[context_key] = efficiency;
    }

    logger().debug() << "[MetaLearning] Meta-knowledge update complete";
}

std::map<std::string, double> MetaLearning::getMetaLearningStatistics() const
{
    std::map<std::string, double> stats;
    
    stats["total_learning_contexts"] = static_cast<double>(_learning_performance_history.size());
    stats["adaptation_threshold"] = _adaptation_threshold;
    stats["min_samples_for_adaptation"] = static_cast<double>(_min_samples_for_adaptation);
    stats["strategy_transfer_enabled"] = _enable_strategy_transfer ? 1.0 : 0.0;
    
    // Calculate average learning efficiency
    double total_efficiency = 0.0;
    for (const auto& pair : _learning_efficiency_scores) {
        total_efficiency += pair.second;
    }
    stats["average_learning_efficiency"] = _learning_efficiency_scores.empty() ? 0.0 :
                                          total_efficiency / _learning_efficiency_scores.size();
    
    // Count total adaptations made
    double total_adaptations = 0.0;
    for (const auto& pair : _adaptation_effectiveness) {
        total_adaptations += pair.second.size();
    }
    stats["total_adaptations_made"] = total_adaptations;
    
    return stats;
}

void MetaLearning::setMetaParameters(double adaptation_threshold,
                                    size_t min_samples,
                                    bool enable_transfer)
{
    _adaptation_threshold = std::max(0.0, std::min(1.0, adaptation_threshold));
    _min_samples_for_adaptation = min_samples;
    _enable_strategy_transfer = enable_transfer;
    
    logger().debug() << "[MetaLearning] Meta-parameters updated: threshold=" << _adaptation_threshold
                    << ", min_samples=" << min_samples << ", transfer=" << enable_transfer;
}

void MetaLearning::reset()
{
    logger().info() << "[MetaLearning] Resetting meta-learning system";
    
    _learning_performance_history.clear();
    _adaptation_effectiveness.clear();
    _strategy_usage_counts.clear();
    _current_learning_rates.clear();
    _learning_efficiency_scores.clear();
    
    initializeMetaLearningBase();
    
    logger().info() << "[MetaLearning] Meta-learning system reset complete";
}

// Private methods

void MetaLearning::initializeMetaLearningBase()
{
    _meta_learning_base = _atomspace->add_node(CONCEPT_NODE, "MetaLearningBase");
    
    // Create key meta-learning concepts
    Handle strategy_concepts = _atomspace->add_node(CONCEPT_NODE, "LearningStrategies");
    Handle adaptation_concepts = _atomspace->add_node(CONCEPT_NODE, "StrategyAdaptations");
    Handle performance_concepts = _atomspace->add_node(CONCEPT_NODE, "PerformanceMetrics");
    
    _atomspace->add_link(INHERITANCE_LINK, {strategy_concepts, _meta_learning_base});
    _atomspace->add_link(INHERITANCE_LINK, {adaptation_concepts, _meta_learning_base});
    _atomspace->add_link(INHERITANCE_LINK, {performance_concepts, _meta_learning_base});
    
    logger().debug() << "[MetaLearning] Meta-learning base initialized in AtomSpace";
}

double MetaLearning::calculateLearningEfficiency(const std::vector<double>& performance_history)
{
    if (performance_history.empty()) {
        return 0.0;
    }

    if (performance_history.size() == 1) {
        return performance_history[0];
    }

    // Calculate efficiency as improvement rate relative to time
    double initial_performance = performance_history.front();
    double final_performance = performance_history.back();
    double improvement = final_performance - initial_performance;
    double time_factor = 1.0 / performance_history.size(); // Inverse of learning time
    
    // Efficiency combines improvement and speed
    double efficiency = (improvement + 1.0) * time_factor;
    return std::max(0.0, std::min(1.0, efficiency));
}

double MetaLearning::calculateAdaptationImpact(StrategyAdaptation adaptation,
                                              double before_performance,
                                              double after_performance)
{
    double raw_impact = after_performance - before_performance;
    
    // Normalize impact based on adaptation type
    switch (adaptation) {
        case StrategyAdaptation::INCREASE_EXPLORATION:
        case StrategyAdaptation::DECREASE_EXPLORATION:
            // Exploration changes may have delayed effects
            return raw_impact * 0.8;
        case StrategyAdaptation::ADJUST_LEARNING_RATE:
            // Learning rate changes should have immediate effects
            return raw_impact * 1.2;
        case StrategyAdaptation::TRANSFER_KNOWLEDGE:
            // Knowledge transfer can have large positive impacts
            return std::max(0.0, raw_impact * 1.5);
        default:
            return raw_impact;
    }
}

std::string MetaLearning::getStrategyName(int strategy_code)
{
    switch (strategy_code) {
        case 0: return "IMITATION";
        case 1: return "REINFORCEMENT";
        case 2: return "EXPLORATORY";
        case 3: return "TRANSFER";
        case 4: return "COMPOSITIONAL";
        case 5: return "REFLECTIVE";
        case 6: return "COLLABORATIVE";
        default: return "UNKNOWN";
    }
}

MetaLearning::StrategyAdaptation MetaLearning::selectBestAdaptation(
    const std::vector<double>& performance_history,
    const std::vector<Handle>& context_data)
{
    if (performance_history.empty()) {
        return StrategyAdaptation::ADJUST_LEARNING_RATE;
    }

    // Analyze performance trend
    double recent_performance = performance_history.back();
    double improvement_rate = 0.0;
    
    if (performance_history.size() >= 2) {
        improvement_rate = performance_history.back() - performance_history.front();
        improvement_rate /= performance_history.size();
    }

    // Select adaptation based on performance analysis
    if (recent_performance < 0.3) {
        // Very poor performance - need major changes
        return StrategyAdaptation::CHANGE_REPRESENTATION;
    } else if (improvement_rate < -0.01) {
        // Performance declining
        return StrategyAdaptation::INCREASE_EXPLORATION;
    } else if (improvement_rate < 0.001) {
        // Performance plateaued
        return StrategyAdaptation::MODIFY_OBJECTIVES;
    } else if (recent_performance > 0.8 && improvement_rate > 0.01) {
        // High performance, still improving
        return StrategyAdaptation::TRANSFER_KNOWLEDGE;
    } else {
        // Default adjustment
        return StrategyAdaptation::ADJUST_LEARNING_RATE;
    }
}

void MetaLearning::recordStrategyUsage(const std::string& strategy_name)
{
    _strategy_usage_counts[strategy_name]++;
}

double MetaLearning::calculateDomainSimilarity(const std::string& domain1, const std::string& domain2)
{
    if (domain1 == domain2) {
        return 1.0;
    }

    // Simplified similarity calculation based on string similarity
    // In practice, this would involve sophisticated domain analysis
    
    // Check for common substrings
    size_t common_length = 0;
    size_t max_length = std::max(domain1.length(), domain2.length());
    
    for (size_t i = 0; i < std::min(domain1.length(), domain2.length()); ++i) {
        if (domain1[i] == domain2[i]) {
            common_length++;
        } else {
            break;
        }
    }
    
    double similarity = static_cast<double>(common_length) / max_length;
    return std::max(0.0, std::min(1.0, similarity));
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
MetaLearning::MetaLearning(AtomSpacePtr atomspace)
    : _atomspace(atomspace), _initialized(false)
{
    logger().info() << "[MetaLearning] Creating meta-learning module";
}

MetaLearning::~MetaLearning()
{
    logger().info() << "[MetaLearning] Destroyed meta-learning module";
}

bool MetaLearning::initialize()
{
    if (_initialized) {
        return true;
    }

    if (!_atomspace) {
        logger().error() << "[MetaLearning] AtomSpace is null";
        return false;
    }

    _initialized = true;
    logger().info() << "[MetaLearning] Meta-learning initialized";
    return true;
}

void MetaLearning::optimizeLearningParams(const std::map<std::string, std::string>& current_params)
{
    if (!_initialized) {
        logger().error() << "[MetaLearning] Not initialized";
        return;
    }

    try {
        logger().info() << "[MetaLearning] Optimizing learning parameters";
        
        // Simple meta-learning: log current parameters
        for (const auto& param : current_params) {
            logger().debug() << "[MetaLearning] Parameter " << param.first << " = " << param.second;
        }
        
        // In a full implementation, this would analyze learning performance
        // and adjust parameters accordingly
        
        logger().info() << "[MetaLearning] Learning parameter optimization completed";
    }
    catch (const std::exception& e) {
        logger().error() << "[MetaLearning] Error optimizing learning parameters: " << e.what();
    }
}
 * MetaLearning.cpp - Implementation of Meta-Learning Capabilities
 * 
 * Part of AZ-LEARN-003: MOSES Policy Optimization Integration
 * Copyright (C) 2024 OpenCog Foundation
 */

#include <agentzero/learning/MetaLearning.h>
#include <agentzero/learning/PolicyOptimizer.h>
#include <agentzero/learning/ExperienceManager.h>
#include <agentzero/learning/SkillAcquisition.h>
#include <agentzero/learning/LearningUtils.h>

#include <opencog/util/Logger.h>

namespace opencog {
namespace agentzero {
namespace learning {

MetaLearning::MetaLearning(AtomSpacePtr atomspace,
                          std::shared_ptr<PolicyOptimizer> policy_optimizer,
                          std::shared_ptr<ExperienceManager> experience_manager,
                          std::shared_ptr<SkillAcquisition> skill_acquisition,
                          const LearningConfig& config)
    : atomspace_(atomspace), policy_optimizer_(policy_optimizer),
      experience_manager_(experience_manager), skill_acquisition_(skill_acquisition), config_(config) {
    
    if (!atomspace_) {
        throw LearningException("AtomSpace cannot be null");
    }
    
    if (!policy_optimizer_) {
        throw LearningException("PolicyOptimizer cannot be null");
    }
    
    if (!experience_manager_) {
        throw LearningException("ExperienceManager cannot be null");
    }
    
    if (!skill_acquisition_) {
        throw LearningException("SkillAcquisition cannot be null");
    }
    
    logger().info("MetaLearning: Initialized");
}

MetaLearning::~MetaLearning() {
    logger().info("MetaLearning: Destroyed");
}

void MetaLearning::adaptLearningParameters() {
    logger().info("MetaLearning: Adapting learning parameters");
    
    // Get current optimization statistics
    auto opt_stats = policy_optimizer_->getOptimizationStats();
    auto exp_stats = experience_manager_->getExperienceStats();
    
    // Simple adaptation logic - can be enhanced
    double current_performance = opt_stats["average_fitness"];
    
    if (current_performance < 0.5) {
        // Performance is low, increase exploration
        LearningConfig updated_config = config_;
        updated_config.exploration_rate = std::min(1.0, updated_config.exploration_rate * 1.1);
        updated_config.diversity_pressure = std::min(1.0, updated_config.diversity_pressure * 1.05);
        
        policy_optimizer_->updateConfig(updated_config);
        config_ = updated_config;
        
        logger().info("MetaLearning: Increased exploration rate to %.3f", updated_config.exploration_rate);
    } else if (current_performance > 0.8) {
        // Performance is high, reduce exploration
        LearningConfig updated_config = config_;
        updated_config.exploration_rate = std::max(0.01, updated_config.exploration_rate * 0.95);
        updated_config.diversity_pressure = std::max(0.01, updated_config.diversity_pressure * 0.98);
        
        policy_optimizer_->updateConfig(updated_config);
        config_ = updated_config;
        
        logger().info("MetaLearning: Reduced exploration rate to %.3f", updated_config.exploration_rate);
    }
}

void MetaLearning::optimizeHyperparameters() {
    logger().info("MetaLearning: Optimizing hyperparameters");
    
    // Get performance metrics
    auto opt_stats = policy_optimizer_->getOptimizationStats();
    
    double total_evaluations = opt_stats["total_evaluations"];
    double best_fitness = opt_stats["best_fitness_ever"];
    
    // Simple hyperparameter optimization
    if (total_evaluations > 1000 && best_fitness < 0.3) {
        // Increase population size and generations for better exploration
        LearningConfig updated_config = config_;
        updated_config.population_size = std::min(size_t(2000), 
                                                 static_cast<size_t>(updated_config.population_size * 1.2));
        updated_config.max_gens = std::min(size_t(2000), 
                                          static_cast<size_t>(updated_config.max_gens * 1.1));
        
        policy_optimizer_->updateConfig(updated_config);
        config_ = updated_config;
        
        logger().info("MetaLearning: Increased population size to %zu, max_gens to %zu", 
                      updated_config.population_size, updated_config.max_gens);
    }
}

void MetaLearning::updateLearningStrategy() {
    logger().info("MetaLearning: Updating learning strategy");
    
    // Analyze experience patterns
    auto exp_stats = experience_manager_->getExperienceStats();
    auto reward_stats = experience_manager_->getRewardStats();
    
    double avg_reward = reward_stats["mean"];
    double reward_variance = reward_stats["std_dev"] * reward_stats["std_dev"];
    
    // Adjust learning strategy based on reward patterns
    if (reward_variance > 1.0) {
        // High variance in rewards, focus on experience replay
        LearningConfig updated_config = config_;
        updated_config.experience_buffer_size = std::min(size_t(10000),
                                                        static_cast<size_t>(updated_config.experience_buffer_size * 1.5));
        
        experience_manager_->updateConfig(updated_config);
        config_ = updated_config;
        
        logger().info("MetaLearning: Increased experience buffer size to %zu due to high reward variance", 
                      updated_config.experience_buffer_size);
    }
    
    // Trigger skill discovery if we have enough positive experiences
    if (avg_reward > 0.6) {
        auto discovered_skills = skill_acquisition_->discoverSkillsFromExperience();
        logger().info("MetaLearning: Discovered %zu new skills", discovered_skills.size());
    }
}

std::map<std::string, double> MetaLearning::getMetaLearningStats() const {
    std::lock_guard<std::mutex> lock(stats_mutex_);
    
    std::map<std::string, double> stats;
    
    // Combine stats from all components
    auto opt_stats = policy_optimizer_->getOptimizationStats();
    auto exp_stats = experience_manager_->getExperienceStats();
    
    stats["current_exploration_rate"] = config_.exploration_rate;
    stats["current_diversity_pressure"] = config_.diversity_pressure;
    stats["current_population_size"] = static_cast<double>(config_.population_size);
    stats["current_buffer_size"] = static_cast<double>(config_.experience_buffer_size);
    
    // Derived meta-learning metrics
    if (opt_stats["total_evaluations"] > 0) {
        stats["learning_efficiency"] = opt_stats["best_fitness_ever"] / opt_stats["total_evaluations"];
    } else {
        stats["learning_efficiency"] = 0.0;
    }
    
    if (exp_stats["total_experiences"] > 0) {
        stats["experience_utilization"] = exp_stats["current_buffer_size"] / exp_stats["total_experiences"];
    } else {
        stats["experience_utilization"] = 0.0;
    }
    
    return stats;
}

} // namespace learning
} // namespace agentzero
} // namespace opencog
