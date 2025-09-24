/*
 * src/MetaLearning.cpp
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