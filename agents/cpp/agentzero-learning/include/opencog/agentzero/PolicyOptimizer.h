/*
 * opencog/agentzero/PolicyOptimizer.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * PolicyOptimizer - Uses MOSES for policy evolution and optimization
 * Part of the Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 * PolicyOptimizer - Uses MOSES for policy evolution
 * Part of AZ-LEARN-004: Implement MetaLearning capabilities
 */

#ifndef _OPENCOG_AGENTZERO_POLICY_OPTIMIZER_H
#define _OPENCOG_AGENTZERO_POLICY_OPTIMIZER_H

#include <memory>
#include <string>
#include <vector>
#include <map>
#include <vector>
#include <map>
#include <string>
#include <chrono>
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
 * Policy representation
 */
struct Policy {
    Handle id;                          // Unique policy identifier
    std::string name;                   // Human-readable policy name
    Handle conditions;                  // Conditions under which policy applies
    Handle actions;                     // Actions defined by policy
    double fitness;                     // Current fitness score
    double performance;                 // Performance metric (0.0-1.0)
    int evaluation_count;               // Number of times policy has been evaluated
    std::chrono::system_clock::time_point created; // Creation timestamp
    std::chrono::system_clock::time_point last_evaluated; // Last evaluation
    std::map<std::string, double> metrics; // Additional performance metrics
    
    Policy() : fitness(0.0), performance(0.0), evaluation_count(0) {}
};

/**
 * Policy optimization configuration
 */
struct PolicyOptimizerConfig {
    int population_size;                // Size of policy population
    int max_generations;                // Maximum generations for evolution
    double mutation_rate;               // Rate of policy mutation
    double crossover_rate;              // Rate of policy crossover
    double elite_fraction;              // Fraction of elite policies to preserve
    int max_evaluation_time_ms;         // Maximum time for policy evaluation
    bool enable_moses_integration;      // Enable MOSES-based optimization
    bool enable_asmoses_integration;    // Enable ASMoses-based optimization
    std::string fitness_function;       // Fitness function to use
    
    PolicyOptimizerConfig()
        : population_size(50), max_generations(100), mutation_rate(0.1),
          crossover_rate(0.7), elite_fraction(0.2), max_evaluation_time_ms(1000),
          enable_moses_integration(true), enable_asmoses_integration(true),
          fitness_function("performance") {}
};

/**
 * PolicyOptimizer - Uses MOSES for policy evolution
 *
 * This class implements policy optimization using evolutionary algorithms,
 * with integration to MOSES and ASMoses for advanced optimization techniques.
 * It maintains populations of policies and evolves them based on performance
 * feedback from the environment.
 *
 * Key features:
 * - Policy population management
 * - MOSES/ASMoses integration for optimization
 * - Multi-objective policy optimization  
 * - Performance-based policy selection
 * - Adaptive policy evolution
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
    // Fitness evaluation function type
    using FitnessFunction = std::function<double(const Policy&, const Handle&)>;

private:
    // Core references
    AtomSpacePtr _atomspace;
    
    // Policy storage
    std::vector<Policy> _policies;
    std::map<Handle, size_t> _policy_index;         // Handle to index mapping
    std::map<std::string, std::vector<size_t>> _name_index; // Name-based index
    
    // Optimization state
    PolicyOptimizerConfig _config;
    std::vector<Policy> _current_population;
    int _current_generation;
    Handle _optimization_context;
    FitnessFunction _fitness_function;
    
    // Performance tracking
    std::vector<double> _generation_best_fitness;
    std::vector<double> _generation_avg_fitness;
    Policy _best_policy_overall;
    
    // AtomSpace handles
    Handle _policy_context;
    Handle _optimization_link;
    
    // Internal methods - Population Management
    void initializePopulation();
    std::vector<Policy> selectParents();
    Policy crossoverPolicies(const Policy& parent1, const Policy& parent2);
    Policy mutatePPolicy(const Policy& policy);
    void evaluatePopulation();
    void updatePopulation();
    
    // Internal methods - MOSES Integration
    #ifdef HAVE_MOSES
    bool optimizeWithMOSES(std::vector<Policy>& policies);
    Handle convertPolicyToMOSESProgram(const Policy& policy);
    Policy convertMOSESProgramToPolicy(const Handle& program);
    #endif
    
    // Internal methods - ASMoses Integration
    #ifdef HAVE_ASMOSES
    bool optimizeWithASMoses(std::vector<Policy>& policies);
    Handle evolveAtomSpacePolicies(const std::vector<Handle>& policy_atoms);
    #endif
    
    // Internal methods - Policy Creation and Evaluation
    Handle createPolicyAtom(const Policy& policy);
    void updatePolicyMetrics(Policy& policy, double fitness, double performance);
    double evaluatePolicyFitness(const Policy& policy, const Handle& context);
    bool isPolicyValid(const Policy& policy) const;

public:
    /**
     * Constructor
     * @param atomspace AtomSpace for policy representation
     */
    explicit PolicyOptimizer(AtomSpacePtr atomspace);

     * @param atomspace Shared pointer to AtomSpace
     * @param config Policy optimizer configuration
     */
    PolicyOptimizer(AtomSpacePtr atomspace, 
                   const PolicyOptimizerConfig& config = PolicyOptimizerConfig());
    
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
    
    /**
     * Initialize policy optimizer
     */
    void initialize();
    
    // Core optimization operations
    /**
     * Create a new policy
     * @param name Policy name
     * @param conditions Conditions under which policy applies
     * @param actions Actions defined by policy
     * @return Handle to created policy
     */
    Handle createPolicy(const std::string& name, const Handle& conditions, const Handle& actions);
    
    /**
     * Optimize policies for a specific context and objective
     * @param context Optimization context
     * @param objective Optimization objective
     * @param max_iterations Maximum optimization iterations
     * @return Handle to best policy found
     */
    Handle optimizePolicies(const Handle& context, const Handle& objective, int max_iterations = 100);
    
    /**
     * Evolve policy population using evolutionary algorithms
     * @param generations Number of generations to evolve
     * @return Best policy from evolution
     */
    Policy evolvePolicies(int generations);
    
    /**
     * Evaluate a policy's performance
     * @param policy_handle Handle to policy
     * @param context Evaluation context
     * @return Performance score (0.0-1.0)
     */
    double evaluatePolicy(const Handle& policy_handle, const Handle& context);
    
    // Policy management
    /**
     * Get policy by handle
     * @param policy_handle Handle to policy
     * @return Policy if found, empty policy otherwise
     */
    Policy getPolicy(const Handle& policy_handle) const;
    
    /**
     * Get policies by name
     * @param policy_name Name to search for
     * @return Vector of matching policies
     */
    std::vector<Policy> getPoliciesByName(const std::string& policy_name) const;
    
    /**
     * Get best performing policies
     * @param count Number of top policies to return
     * @param min_evaluations Minimum number of evaluations required
     * @return Vector of best policies
     */
    std::vector<Policy> getBestPolicies(int count = 10, int min_evaluations = 5) const;
    
    /**
     * Update policy performance based on execution results
     * @param policy_handle Handle to policy
     * @param performance Performance score (0.0-1.0)
     * @param context Execution context
     * @return True if successfully updated
     */
    bool updatePolicyPerformance(const Handle& policy_handle, double performance, 
                                const Handle& context);
    
    /**
     * Remove underperforming policies
     * @param min_performance Minimum performance threshold
     * @param min_evaluations Minimum evaluations required
     * @return Number of policies removed
     */
    int pruneUnderperformingPolicies(double min_performance = 0.3, int min_evaluations = 10);
    
    // Optimization techniques
    /**
     * Optimize policies using MOSES
     * @param context Optimization context
     * @param target_policies Policies to optimize (empty for all)
     * @return Number of policies improved
     */
    int optimizeWithMOSESIntegration(const Handle& context, 
                                    const std::vector<Handle>& target_policies = {});
    
    /**
     * Optimize policies using ASMoses
     * @param context Optimization context
     * @param target_policies Policies to optimize (empty for all)
     * @return Number of policies improved
     */
    int optimizeWithASMosesIntegration(const Handle& context,
                                      const std::vector<Handle>& target_policies = {});
    
    /**
     * Apply multi-objective optimization
     * @param objectives Vector of optimization objectives
     * @param weights Weights for each objective
     * @return Pareto-optimal policies
     */
    std::vector<Policy> multiObjectiveOptimization(const std::vector<Handle>& objectives,
                                                   const std::vector<double>& weights);
    
    // Analysis and metrics
    /**
     * Analyze optimization progress
     * @param time_window Time window for analysis
     * @return Analysis results atom
     */
    Handle analyzeOptimizationProgress(std::chrono::hours time_window = std::chrono::hours(24));
    
    /**
     * Get optimization statistics
     * @return Map of statistics (policy counts, fitness distributions, etc.)
     */
    std::map<std::string, double> getOptimizationStatistics() const;
    
    /**
     * Get convergence information
     * @return Handle to convergence analysis atom
     */
    Handle getConvergenceAnalysis() const;
    
    /**
     * Get policy diversity metrics
     * @return Diversity score (0.0-1.0)
     */
    double getPolicyDiversity() const;
    
    // Configuration and control
    /**
     * Configure optimization parameters
     * @param config New configuration
     */
    void configure(const PolicyOptimizerConfig& config);
    
    /**
     * Set custom fitness function
     * @param fitness_function Custom fitness evaluation function
     */
    void setFitnessFunction(FitnessFunction fitness_function);
    
    /**
     * Reset optimizer state and policies
     */
    void reset();
    
    /**
     * Check if policy optimizer is initialized
     * @return True if initialized
     */
    bool isInitialized() const;
    
    // Status and debugging
    /**
     * Get total number of policies
     * @return Policy count
     */
    size_t getPolicyCount() const { return _policies.size(); }
    
    /**
     * Get current generation number
     * @return Current generation
     */
    int getCurrentGeneration() const { return _current_generation; }
    
    /**
     * Get best policy overall
     * @return Best policy found so far
     */
    Policy getBestPolicyOverall() const { return _best_policy_overall; }
    
    /**
     * Get fitness evolution history
     * @return Vector of best fitness scores per generation
     */
    std::vector<double> getFitnessHistory() const { return _generation_best_fitness; }
    
    /**
     * Validate policy storage integrity
     * @return True if storage is consistent
     */
    bool validatePolicyIntegrity() const;
    
    /**
     * Get resource usage statistics
     * @return Map of resource usage metrics
     */
    std::map<std::string, size_t> getResourceUsage() const;
};

} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_POLICY_OPTIMIZER_H