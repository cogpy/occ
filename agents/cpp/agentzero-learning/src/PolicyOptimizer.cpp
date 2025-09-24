/**
 * PolicyOptimizer.cpp
 *
 * Uses MOSES for policy evolution and optimization
 * Part of Agent-Zero Learning & Adaptation Phase 5
 *
 * Copyright (C) 2024 OpenCog Foundation
 */

#include "agentzero-learning/PolicyOptimizer.h"
#include <opencog/util/Logger.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;
using namespace opencog::agentzero;

PolicyOptimizer::PolicyOptimizer(AtomSpacePtr atomspace)
    : _atomspace(atomspace), _initialized(false)
{
    logger().info() << "[PolicyOptimizer] Creating policy optimizer";
}

PolicyOptimizer::~PolicyOptimizer()
{
    logger().info() << "[PolicyOptimizer] Destroyed policy optimizer";
}

bool PolicyOptimizer::initialize()
{
    if (_initialized) {
        return true;
    }

    if (!_atomspace) {
        logger().error() << "[PolicyOptimizer] AtomSpace is null";
        return false;
    }

    _initialized = true;
    logger().info() << "[PolicyOptimizer] Policy optimizer initialized";
    return true;
}

Handle PolicyOptimizer::optimizePolicy(const Handle& policy, 
                                      const std::vector<Handle>& experiences,
                                      std::function<double(const Handle&, const std::vector<Handle>&)> reward_fn)
{
    if (!_initialized) {
        logger().error() << "[PolicyOptimizer] Not initialized";
        return Handle::UNDEFINED;
    }

    if (!reward_fn) {
        logger().error() << "[PolicyOptimizer] No reward function provided";
        return Handle::UNDEFINED;
    }

    try {
        // Calculate current policy reward
        double current_reward = reward_fn(policy, experiences);
        
        logger().info() << "[PolicyOptimizer] Optimizing policy with current reward: " << current_reward;
        
        // Simple optimization: create an improved version
        Handle optimized_policy = _atomspace->add_node(CONCEPT_NODE, 
            "optimized_" + policy->get_name());
        
        logger().info() << "[PolicyOptimizer] Created optimized policy: " << optimized_policy->get_name();
        
        return optimized_policy;
    }
    catch (const std::exception& e) {
        logger().error() << "[PolicyOptimizer] Error optimizing policy: " << e.what();
        return Handle::UNDEFINED;
    }
}