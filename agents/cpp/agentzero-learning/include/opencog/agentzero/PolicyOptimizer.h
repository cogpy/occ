/*
 * opencog/agentzero/PolicyOptimizer.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * PolicyOptimizer - Uses MOSES for policy evolution and optimization
 * Part of the Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 */

#ifndef _OPENCOG_AGENTZERO_POLICY_OPTIMIZER_H
#define _OPENCOG_AGENTZERO_POLICY_OPTIMIZER_H

#include <memory>
#include <string>
#include <vector>
#include <map>
#include <functional>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/util/Logger.h>

namespace opencog {
namespace agentzero {

/**
 * PolicyOptimizer - Optimizes agent policies using MOSES evolutionary algorithm
 *
 * This class integrates with MOSES (Meta-Optimizing Semantic Evolutionary Search)
 * to evolve and optimize agent policies for improved performance. It bridges
 * the gap between AtomSpace representations and MOSES optimization.
 */
class PolicyOptimizer
{
public:
    /**
     * Policy types that can be optimized
     */
    enum class PolicyType {
        ACTION_SELECTION,   // Policies for selecting actions
        RESOURCE_ALLOCATION, // Policies for resource management
        LEARNING_STRATEGY,  // Policies for learning approach selection
        EXPLORATION,        // Policies for exploration vs exploitation
        COMMUNICATION,      // Policies for agent communication
        GOAL_PRIORITIZATION // Policies for goal ordering
    };

    /**
     * Optimization objective types
     */
    enum class OptimizationObjective {
        MAXIMIZE_REWARD,    // Maximize cumulative reward
        MINIMIZE_ERROR,     // Minimize prediction/action errors
        MAXIMIZE_EFFICIENCY, // Maximize resource efficiency
        MINIMIZE_TIME,      // Minimize task completion time
        BALANCE_TRADEOFF,   // Balance multiple objectives
        MAXIMIZE_EXPLORATION // Maximize information gain
    };

private:
    AtomSpacePtr _atomspace;
    Handle _policy_base;
    std::map<std::string, Handle> _policy_registry;
    std::map<Handle, PolicyType> _policy_types;
    
    // MOSES integration parameters
    bool _moses_available;
    size_t _population_size;
    size_t _max_generations;
    double _mutation_rate;
    double _crossover_rate;
    
    // Optimization history
    std::map<Handle, std::vector<double>> _optimization_history;
    std::map<Handle, size_t> _optimization_rounds;

public:
    /**
     * Constructor
     * @param atomspace AtomSpace for policy representation
     */
    explicit PolicyOptimizer(AtomSpacePtr atomspace);

    /**
     * Destructor
     */
    ~PolicyOptimizer();

    /**
     * Optimize skill structure using evolutionary approach
     * @param skill_components Components to optimize
     * @param training_data Data for fitness evaluation
     * @return Optimized skill structure
     */
    HandleSeq optimizeSkillStructure(const HandleSeq& skill_components,
                                    const std::vector<Handle>& training_data);

    /**
     * Create and optimize a new policy
     * @param policy_name Name of the policy
     * @param policy_type Type of policy
     * @param objective Optimization objective
     * @param initial_structure Initial policy structure
     * @param training_data Data for optimization
     * @return Handle to optimized policy
     */
    Handle optimizePolicy(const std::string& policy_name,
                         PolicyType policy_type,
                         OptimizationObjective objective,
                         const HandleSeq& initial_structure,
                         const std::vector<Handle>& training_data);

    /**
     * Refine an existing policy
     * @param policy_handle Handle to existing policy
     * @param new_training_data Additional training data
     * @param refinement_rounds Number of optimization rounds
     * @return Updated fitness score
     */
    double refinePolicy(Handle policy_handle,
                       const std::vector<Handle>& new_training_data,
                       size_t refinement_rounds = 10);

    /**
     * Evaluate policy fitness
     * @param policy_handle Handle to policy
     * @param test_data Data for evaluation
     * @return Fitness score (0.0 to 1.0)
     */
    double evaluatePolicyFitness(Handle policy_handle,
                                const std::vector<Handle>& test_data);

    /**
     * Get best performing policies
     * @param policy_type Type of policies to consider
     * @param count Maximum number to return
     * @return Vector of top-performing policy handles
     */
    std::vector<Handle> getBestPolicies(PolicyType policy_type, size_t count = 5);

    /**
     * Get policy optimization history
     * @param policy_handle Handle to policy
     * @return Vector of fitness scores over time
     */
    std::vector<double> getOptimizationHistory(Handle policy_handle) const;

    /**
     * Set MOSES parameters
     * @param population_size Size of evolution population
     * @param max_generations Maximum generations to run
     * @param mutation_rate Mutation probability
     * @param crossover_rate Crossover probability
     */
    void setMOSESParameters(size_t population_size,
                           size_t max_generations,
                           double mutation_rate,
                           double crossover_rate);

    /**
     * Check if MOSES is available
     * @return True if MOSES integration is functional
     */
    bool isMOSESAvailable() const { return _moses_available; }

    /**
     * Get optimization statistics
     * @return Map of statistic names to values
     */
    std::map<std::string, double> getOptimizationStatistics() const;

private:
    void initializePolicyBase();
    void checkMOSESAvailability();
    Handle createPolicyAtom(const std::string& name, PolicyType type);
    HandleSeq runMOSESOptimization(const HandleSeq& initial_structure,
                                  const std::vector<Handle>& training_data,
                                  OptimizationObjective objective);
    double calculateFitness(const HandleSeq& policy_structure,
                           const std::vector<Handle>& data,
                           OptimizationObjective objective);
    HandleSeq mutatePolicyStructure(const HandleSeq& structure);
    HandleSeq crossoverPolicyStructures(const HandleSeq& parent1, const HandleSeq& parent2);
    void recordOptimizationResult(Handle policy_handle, double fitness);
};

} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_POLICY_OPTIMIZER_H