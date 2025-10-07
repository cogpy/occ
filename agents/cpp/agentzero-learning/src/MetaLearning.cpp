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
