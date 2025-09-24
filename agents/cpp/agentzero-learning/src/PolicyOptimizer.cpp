/*
 * opencog/agentzero/PolicyOptimizer.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * PolicyOptimizer - Uses MOSES for policy evolution
 * Part of AZ-LEARN-004: Implement MetaLearning capabilities
 */

#include "opencog/agentzero/PolicyOptimizer.h"

#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/util/random.h>

#include <algorithm>
#include <numeric>

using namespace opencog;
using namespace opencog::agentzero;

// Constructor
PolicyOptimizer::PolicyOptimizer(AtomSpacePtr atomspace, const PolicyOptimizerConfig& config)
    : _atomspace(atomspace)
    , _config(config)
    , _current_generation(0)
    , _optimization_context(Handle::UNDEFINED)
    , _policy_context(Handle::UNDEFINED)
    , _optimization_link(Handle::UNDEFINED)
{
    logger().info() << "[PolicyOptimizer] Initializing policy optimizer with population size " 
                    << _config.population_size;
    
    // Set default fitness function
    _fitness_function = [this](const Policy& policy, const Handle& context) {
        return policy.performance;
    };
}

// Destructor
PolicyOptimizer::~PolicyOptimizer()
{
    logger().info() << "[PolicyOptimizer] Shutting down with " << _policies.size() 
                    << " policies and " << _current_generation << " generations";
}

// Initialize policy optimizer
void PolicyOptimizer::initialize()
{
    logger().info() << "[PolicyOptimizer] Initializing policy optimization system";
    
    // Create policy context in AtomSpace
    _policy_context = _atomspace->add_node(CONCEPT_NODE, "PolicyContext");
    
    // Create optimization link
    _optimization_link = _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "PolicyOptimization"),
        _policy_context);
    
    // Initialize population
    initializePopulation();
    
    logger().info() << "[PolicyOptimizer] Policy optimization system initialized";
}

// Create a new policy
Handle PolicyOptimizer::createPolicy(const std::string& name, const Handle& conditions, const Handle& actions)
{
    Policy policy;
    policy.id = _atomspace->add_node(CONCEPT_NODE, "Policy_" + std::to_string(rand()));
    policy.name = name;
    policy.conditions = conditions;
    policy.actions = actions;
    policy.fitness = 0.0;
    policy.performance = 0.0;
    policy.evaluation_count = 0;
    policy.created = std::chrono::system_clock::now();
    policy.last_evaluated = policy.created;
    
    // Add to storage
    size_t index = _policies.size();
    _policies.push_back(policy);
    _policy_index[policy.id] = index;
    _name_index[policy.name].push_back(index);
    
    // Create AtomSpace representation
    Handle policy_atom = createPolicyAtom(policy);
    
    logger().info() << "[PolicyOptimizer] Created policy '" << name << "'";
    
    return policy.id;
}

// Optimize policies for a specific context and objective
Handle PolicyOptimizer::optimizePolicies(const Handle& context, const Handle& objective, int max_iterations)
{
    _optimization_context = context;
    
    logger().info() << "[PolicyOptimizer] Starting policy optimization for " << max_iterations << " iterations";
    
    Policy best_policy;
    double best_fitness = -1.0;
    
    for (int iteration = 0; iteration < max_iterations; ++iteration) {
        // Evaluate current population
        evaluatePopulation();
        
        // Find best policy in current population
        for (const auto& policy : _current_population) {
            if (policy.fitness > best_fitness) {
                best_fitness = policy.fitness;
                best_policy = policy;
            }
        }
        
        // Update generation statistics
        if (iteration < static_cast<int>(_generation_best_fitness.size())) {
            _generation_best_fitness[iteration] = best_fitness;
        } else {
            _generation_best_fitness.push_back(best_fitness);
        }
        
        // Evolve population if not final iteration
        if (iteration < max_iterations - 1) {
            updatePopulation();
            _current_generation++;
        }
        
        logger().debug() << "[PolicyOptimizer] Iteration " << iteration 
                        << ", best fitness: " << best_fitness;
    }
    
    // Update best policy overall
    if (best_fitness > _best_policy_overall.fitness) {
        _best_policy_overall = best_policy;
    }
    
    logger().info() << "[PolicyOptimizer] Optimization complete, best fitness: " << best_fitness;
    
    return best_policy.id;
}

// Evolve policy population using evolutionary algorithms
Policy PolicyOptimizer::evolvePolicies(int generations)
{
    logger().info() << "[PolicyOptimizer] Evolving policies for " << generations << " generations";
    
    for (int gen = 0; gen < generations; ++gen) {
        // Evaluate current population
        evaluatePopulation();
        
        // Calculate generation statistics
        double total_fitness = 0.0;
        double best_fitness = 0.0;
        
        for (const auto& policy : _current_population) {
            total_fitness += policy.fitness;
            best_fitness = std::max(best_fitness, policy.fitness);
        }
        
        double avg_fitness = total_fitness / _current_population.size();
        _generation_best_fitness.push_back(best_fitness);
        _generation_avg_fitness.push_back(avg_fitness);
        
        // Update population for next generation
        if (gen < generations - 1) {
            updatePopulation();
        }
        
        _current_generation++;
        
        logger().debug() << "[PolicyOptimizer] Generation " << gen 
                        << ", avg fitness: " << avg_fitness << ", best: " << best_fitness;
    }
    
    // Return best policy
    Policy best_policy;
    double best_fitness = -1.0;
    
    for (const auto& policy : _current_population) {
        if (policy.fitness > best_fitness) {
            best_fitness = policy.fitness;
            best_policy = policy;
        }
    }
    
    if (best_fitness > _best_policy_overall.fitness) {
        _best_policy_overall = best_policy;
    }
    
    return best_policy;
}

// Evaluate a policy's performance
double PolicyOptimizer::evaluatePolicy(const Handle& policy_handle, const Handle& context)
{
    auto it = _policy_index.find(policy_handle);
    if (it == _policy_index.end() || it->second >= _policies.size()) {
        return 0.0;
    }
    
    Policy& policy = _policies[it->second];
    
    // Use fitness function to evaluate policy
    double fitness = _fitness_function(policy, context);
    
    // Update policy metrics
    updatePolicyMetrics(policy, fitness, fitness);
    
    return fitness;
}

// Get policy by handle
Policy PolicyOptimizer::getPolicy(const Handle& policy_handle) const
{
    auto it = _policy_index.find(policy_handle);
    if (it != _policy_index.end() && it->second < _policies.size()) {
        return _policies[it->second];
    }
    return Policy(); // Return empty policy if not found
}

// Get best performing policies
std::vector<Policy> PolicyOptimizer::getBestPolicies(int count, int min_evaluations) const
{
    std::vector<Policy> candidates;
    
    // Filter policies by minimum evaluations
    for (const auto& policy : _policies) {
        if (policy.evaluation_count >= min_evaluations) {
            candidates.push_back(policy);
        }
    }
    
    // Sort by fitness (descending)
    std::sort(candidates.begin(), candidates.end(),
              [](const Policy& a, const Policy& b) {
                  return a.fitness > b.fitness;
              });
    
    // Return top policies
    if (candidates.size() > static_cast<size_t>(count)) {
        candidates.resize(count);
    }
    
    return candidates;
}

// Update policy performance based on execution results
bool PolicyOptimizer::updatePolicyPerformance(const Handle& policy_handle, double performance, 
                                             const Handle& context)
{
    auto it = _policy_index.find(policy_handle);
    if (it != _policy_index.end() && it->second < _policies.size()) {
        updatePolicyMetrics(_policies[it->second], performance, performance);
        return true;
    }
    return false;
}

// Get optimization statistics
std::map<std::string, double> PolicyOptimizer::getOptimizationStatistics() const
{
    std::map<std::string, double> stats;
    
    stats["total_policies"] = static_cast<double>(_policies.size());
    stats["current_generation"] = static_cast<double>(_current_generation);
    stats["population_size"] = static_cast<double>(_current_population.size());
    
    if (_policies.empty()) {
        return stats;
    }
    
    double total_fitness = 0.0;
    double total_performance = 0.0;
    int evaluated_policies = 0;
    
    for (const auto& policy : _policies) {
        total_fitness += policy.fitness;
        total_performance += policy.performance;
        if (policy.evaluation_count > 0) {
            evaluated_policies++;
        }
    }
    
    stats["average_fitness"] = total_fitness / _policies.size();
    stats["average_performance"] = total_performance / _policies.size();
    stats["evaluated_policies"] = static_cast<double>(evaluated_policies);
    stats["best_fitness"] = _best_policy_overall.fitness;
    
    return stats;
}

// Configuration and control
void PolicyOptimizer::configure(const PolicyOptimizerConfig& config)
{
    _config = config;
    logger().info() << "[PolicyOptimizer] Configuration updated";
}

void PolicyOptimizer::setFitnessFunction(FitnessFunction fitness_function)
{
    _fitness_function = fitness_function;
    logger().info() << "[PolicyOptimizer] Custom fitness function set";
}

void PolicyOptimizer::reset()
{
    _policies.clear();
    _policy_index.clear();
    _name_index.clear();
    _current_population.clear();
    _generation_best_fitness.clear();
    _generation_avg_fitness.clear();
    _current_generation = 0;
    _best_policy_overall = Policy();
    
    logger().info() << "[PolicyOptimizer] Policy optimizer reset";
}

bool PolicyOptimizer::isInitialized() const
{
    return _policy_context != Handle::UNDEFINED && _atomspace != nullptr;
}

bool PolicyOptimizer::validatePolicyIntegrity() const
{
    return _policy_index.size() <= _policies.size();
}

// Private implementation methods
void PolicyOptimizer::initializePopulation()
{
    _current_population.clear();
    _current_population.reserve(_config.population_size);
    
    // Create initial random population
    for (int i = 0; i < _config.population_size; ++i) {
        Policy policy;
        policy.id = _atomspace->add_node(CONCEPT_NODE, "InitialPolicy_" + std::to_string(i));
        policy.name = "InitialPolicy_" + std::to_string(i);
        policy.conditions = _atomspace->add_node(CONCEPT_NODE, "InitialCondition_" + std::to_string(i));
        policy.actions = _atomspace->add_node(CONCEPT_NODE, "InitialAction_" + std::to_string(i));
        policy.fitness = 0.0;
        policy.performance = 0.0;
        policy.evaluation_count = 0;
        policy.created = std::chrono::system_clock::now();
        
        _current_population.push_back(policy);
    }
    
    logger().debug() << "[PolicyOptimizer] Initialized population with " 
                     << _current_population.size() << " policies";
}

void PolicyOptimizer::evaluatePopulation()
{
    for (auto& policy : _current_population) {
        // Simulate policy evaluation
        double fitness = _fitness_function(policy, _optimization_context);
        updatePolicyMetrics(policy, fitness, fitness);
    }
}

std::vector<Policy> PolicyOptimizer::selectParents()
{
    std::vector<Policy> parents;
    
    // Tournament selection
    for (int i = 0; i < _config.population_size; ++i) {
        int tournament_size = 3;
        Policy best_candidate;
        double best_fitness = -1.0;
        
        for (int j = 0; j < tournament_size; ++j) {
            int candidate_idx = randGen().randint(_current_population.size());
            const auto& candidate = _current_population[candidate_idx];
            
            if (candidate.fitness > best_fitness) {
                best_fitness = candidate.fitness;
                best_candidate = candidate;
            }
        }
        
        parents.push_back(best_candidate);
    }
    
    return parents;
}

Policy PolicyOptimizer::crossoverPolicies(const Policy& parent1, const Policy& parent2)
{
    Policy offspring;
    offspring.id = _atomspace->add_node(CONCEPT_NODE, "Offspring_" + std::to_string(rand()));
    offspring.name = "Offspring_" + std::to_string(rand());
    
    // Simple crossover - combine conditions and actions from parents
    offspring.conditions = (randGen().randdouble() < 0.5) ? parent1.conditions : parent2.conditions;
    offspring.actions = (randGen().randdouble() < 0.5) ? parent1.actions : parent2.actions;
    
    offspring.fitness = 0.0;
    offspring.performance = 0.0;
    offspring.evaluation_count = 0;
    offspring.created = std::chrono::system_clock::now();
    
    return offspring;
}

Policy PolicyOptimizer::mutatePPolicy(const Policy& policy)
{
    Policy mutated = policy;
    mutated.id = _atomspace->add_node(CONCEPT_NODE, "Mutated_" + std::to_string(rand()));
    mutated.name = "Mutated_" + std::to_string(rand());
    mutated.created = std::chrono::system_clock::now();
    mutated.evaluation_count = 0;
    
    // Simple mutation - create new conditions or actions with some probability
    if (randGen().randdouble() < _config.mutation_rate) {
        mutated.conditions = _atomspace->add_node(CONCEPT_NODE, "MutatedCondition_" + std::to_string(rand()));
    }
    
    if (randGen().randdouble() < _config.mutation_rate) {
        mutated.actions = _atomspace->add_node(CONCEPT_NODE, "MutatedAction_" + std::to_string(rand()));
    }
    
    return mutated;
}

void PolicyOptimizer::updatePopulation()
{
    std::vector<Policy> new_population;
    
    // Select elite policies
    int elite_count = static_cast<int>(_config.elite_fraction * _config.population_size);
    std::vector<Policy> sorted_population = _current_population;
    std::sort(sorted_population.begin(), sorted_population.end(),
              [](const Policy& a, const Policy& b) {
                  return a.fitness > b.fitness;
              });
    
    // Add elite policies
    for (int i = 0; i < elite_count && i < static_cast<int>(sorted_population.size()); ++i) {
        new_population.push_back(sorted_population[i]);
    }
    
    // Generate offspring to fill rest of population
    auto parents = selectParents();
    
    while (new_population.size() < static_cast<size_t>(_config.population_size)) {
        // Select two parents
        int parent1_idx = randGen().randint(parents.size());
        int parent2_idx = randGen().randint(parents.size());
        
        Policy offspring;
        if (randGen().randdouble() < _config.crossover_rate) {
            offspring = crossoverPolicies(parents[parent1_idx], parents[parent2_idx]);
        } else {
            offspring = parents[parent1_idx];
        }
        
        // Apply mutation
        offspring = mutatePPolicy(offspring);
        
        new_population.push_back(offspring);
    }
    
    _current_population = new_population;
}

Handle PolicyOptimizer::createPolicyAtom(const Policy& policy)
{
    Handle policy_atom = policy.id;
    
    // Add policy properties
    if (policy.conditions != Handle::UNDEFINED) {
        _atomspace->add_link(EVALUATION_LINK,
            _atomspace->add_node(PREDICATE_NODE, "PolicyConditions"),
            _atomspace->add_link(LIST_LINK, policy_atom, policy.conditions));
    }
    
    if (policy.actions != Handle::UNDEFINED) {
        _atomspace->add_link(EVALUATION_LINK,
            _atomspace->add_node(PREDICATE_NODE, "PolicyActions"),
            _atomspace->add_link(LIST_LINK, policy_atom, policy.actions));
    }
    
    // Add fitness
    Handle fitness_atom = _atomspace->add_node(NUMBER_NODE, std::to_string(policy.fitness));
    _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "PolicyFitness"),
        _atomspace->add_link(LIST_LINK, policy_atom, fitness_atom));
    
    return policy_atom;
}

void PolicyOptimizer::updatePolicyMetrics(Policy& policy, double fitness, double performance)
{
    policy.fitness = fitness;
    policy.performance = performance;
    policy.evaluation_count++;
    policy.last_evaluated = std::chrono::system_clock::now();
}

double PolicyOptimizer::evaluatePolicyFitness(const Policy& policy, const Handle& context)
{
    // Simple fitness evaluation based on performance
    return policy.performance + (randGen().randdouble() - 0.5) * 0.1; // Add some noise
}