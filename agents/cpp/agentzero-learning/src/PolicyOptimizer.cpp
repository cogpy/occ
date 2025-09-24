/*
 * src/PolicyOptimizer.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * PolicyOptimizer Implementation
 * Part of the Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 */

#include <algorithm>
#include <random>
#include <sstream>
#include <stdexcept>

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>

#include "opencog/agentzero/PolicyOptimizer.h"

using namespace opencog;
using namespace opencog::agentzero;

PolicyOptimizer::PolicyOptimizer(AtomSpacePtr atomspace)
    : _atomspace(atomspace)
    , _policy_base(Handle::UNDEFINED)
    , _moses_available(false)
    , _population_size(100)
    , _max_generations(50)
    , _mutation_rate(0.1)
    , _crossover_rate(0.7)
{
    if (!_atomspace) {
        throw std::runtime_error("PolicyOptimizer requires valid AtomSpace");
    }

    logger().info() << "[PolicyOptimizer] Initializing policy optimization system";
    
    initializePolicyBase();
    checkMOSESAvailability();
    
    logger().info() << "[PolicyOptimizer] Policy optimization system initialized"
                   << " (MOSES available: " << (_moses_available ? "yes" : "no") << ")";
}

PolicyOptimizer::~PolicyOptimizer()
{
    logger().info() << "[PolicyOptimizer] Shutting down policy optimization system";
}

HandleSeq PolicyOptimizer::optimizeSkillStructure(const HandleSeq& skill_components,
                                                 const std::vector<Handle>& training_data)
{
    logger().debug() << "[PolicyOptimizer] Optimizing skill structure with " 
                    << skill_components.size() << " components";

    if (skill_components.empty()) {
        logger().warn() << "[PolicyOptimizer] No components to optimize";
        return skill_components;
    }

    // If MOSES is available, use evolutionary optimization
    if (_moses_available) {
        return runMOSESOptimization(skill_components, training_data, 
                                   OptimizationObjective::MAXIMIZE_EFFICIENCY);
    }

    // Fallback: simple heuristic optimization
    HandleSeq optimized_structure = skill_components;
    
    // Sort components by their truth value strength (simple heuristic)
    std::sort(optimized_structure.begin(), optimized_structure.end(),
              [](Handle a, Handle b) {
                  double strength_a = a->getTruthValue()->get_mean();
                  double strength_b = b->getTruthValue()->get_mean();
                  return strength_a > strength_b;
              });

    logger().debug() << "[PolicyOptimizer] Skill structure optimization complete (heuristic)";
    return optimized_structure;
}

Handle PolicyOptimizer::optimizePolicy(const std::string& policy_name,
                                      PolicyType policy_type,
                                      OptimizationObjective objective,
                                      const HandleSeq& initial_structure,
                                      const std::vector<Handle>& training_data)
{
    logger().info() << "[PolicyOptimizer] Optimizing policy: " << policy_name;

    // Create policy atom
    Handle policy_atom = createPolicyAtom(policy_name, policy_type);
    
    // Register policy
    _policy_registry[policy_name] = policy_atom;
    _policy_types[policy_atom] = policy_type;

    // Optimize structure
    HandleSeq optimized_structure;
    if (_moses_available) {
        optimized_structure = runMOSESOptimization(initial_structure, training_data, objective);
    } else {
        // Fallback optimization
        optimized_structure = initial_structure;
        
        // Apply simple improvements
        for (size_t i = 0; i < 10; ++i) {
            double current_fitness = calculateFitness(optimized_structure, training_data, objective);
            HandleSeq mutated = mutatePolicyStructure(optimized_structure);
            double mutated_fitness = calculateFitness(mutated, training_data, objective);
            
            if (mutated_fitness > current_fitness) {
                optimized_structure = mutated;
                recordOptimizationResult(policy_atom, mutated_fitness);
            }
        }
    }

    // Create policy definition
    Handle policy_definition = _atomspace->add_link(LIST_LINK, optimized_structure);
    _atomspace->add_link(INHERITANCE_LINK, {policy_atom, policy_definition});

    // Set truth value based on optimization results
    double final_fitness = calculateFitness(optimized_structure, training_data, objective);
    policy_atom->setTruthValue(SimpleTruthValue::createTV(final_fitness, 0.8));

    // Record optimization results
    recordOptimizationResult(policy_atom, final_fitness);
    _optimization_rounds[policy_atom] = 1;

    logger().info() << "[PolicyOptimizer] Policy optimization complete: " << policy_name
                   << " (fitness: " << final_fitness << ")";

    return policy_atom;
}

double PolicyOptimizer::refinePolicy(Handle policy_handle,
                                    const std::vector<Handle>& new_training_data,
                                    size_t refinement_rounds)
{
    if (policy_handle == Handle::UNDEFINED) {
        logger().error() << "[PolicyOptimizer] Cannot refine undefined policy";
        return 0.0;
    }

    logger().debug() << "[PolicyOptimizer] Refining policy with " << new_training_data.size() 
                    << " new examples over " << refinement_rounds << " rounds";

    // Get current policy structure
    HandleSeq current_structure;
    IncomingSet policy_links = policy_handle->getIncomingSetByType(INHERITANCE_LINK);
    for (Handle link : policy_links) {
        HandleSeq outgoing = link->getOutgoingSet();
        if (outgoing.size() == 2 && outgoing[0] == policy_handle) {
            Handle definition_link = outgoing[1];
            if (definition_link->get_type() == LIST_LINK) {
                current_structure = definition_link->getOutgoingSet();
                break;
            }
        }
    }

    if (current_structure.empty()) {
        logger().error() << "[PolicyOptimizer] No policy structure found for refinement";
        return 0.0;
    }

    // Determine optimization objective (simplified)
    OptimizationObjective objective = OptimizationObjective::MAXIMIZE_EFFICIENCY;

    // Perform refinement iterations
    double best_fitness = calculateFitness(current_structure, new_training_data, objective);
    HandleSeq best_structure = current_structure;

    for (size_t round = 0; round < refinement_rounds; ++round) {
        HandleSeq candidate_structure;
        
        if (_moses_available && round % 5 == 0) {
            // Use MOSES every 5th round for major optimization
            candidate_structure = runMOSESOptimization(best_structure, new_training_data, objective);
        } else {
            // Use mutation for fine-tuning
            candidate_structure = mutatePolicyStructure(best_structure);
        }

        double candidate_fitness = calculateFitness(candidate_structure, new_training_data, objective);
        
        if (candidate_fitness > best_fitness) {
            best_fitness = candidate_fitness;
            best_structure = candidate_structure;
            recordOptimizationResult(policy_handle, best_fitness);
        }
    }

    // Update policy definition if improved
    if (best_structure != current_structure) {
        // Remove old definition link
        for (Handle link : policy_links) {
            HandleSeq outgoing = link->getOutgoingSet();
            if (outgoing.size() == 2 && outgoing[0] == policy_handle) {
                _atomspace->remove_atom(link);
                break;
            }
        }

        // Add new definition
        Handle new_definition = _atomspace->add_link(LIST_LINK, best_structure);
        _atomspace->add_link(INHERITANCE_LINK, {policy_handle, new_definition});

        // Update truth value
        policy_handle->setTruthValue(SimpleTruthValue::createTV(best_fitness, 0.85));
    }

    // Update optimization round count
    _optimization_rounds[policy_handle]++;

    logger().debug() << "[PolicyOptimizer] Policy refinement complete, final fitness: " << best_fitness;
    return best_fitness;
}

double PolicyOptimizer::evaluatePolicyFitness(Handle policy_handle,
                                             const std::vector<Handle>& test_data)
{
    if (policy_handle == Handle::UNDEFINED) {
        return 0.0;
    }

    // Get policy structure
    HandleSeq policy_structure;
    IncomingSet policy_links = policy_handle->getIncomingSetByType(INHERITANCE_LINK);
    for (Handle link : policy_links) {
        HandleSeq outgoing = link->getOutgoingSet();
        if (outgoing.size() == 2 && outgoing[0] == policy_handle) {
            Handle definition_link = outgoing[1];
            if (definition_link->get_type() == LIST_LINK) {
                policy_structure = definition_link->getOutgoingSet();
                break;
            }
        }
    }

    if (policy_structure.empty()) {
        return 0.0;
    }

    // Use general efficiency objective for evaluation
    return calculateFitness(policy_structure, test_data, OptimizationObjective::MAXIMIZE_EFFICIENCY);
}

std::vector<Handle> PolicyOptimizer::getBestPolicies(PolicyType policy_type, size_t count)
{
    std::vector<std::pair<Handle, double>> policy_fitness_pairs;

    // Collect policies of the specified type with their fitness scores
    for (const auto& pair : _policy_types) {
        if (pair.second == policy_type) {
            Handle policy = pair.first;
            double fitness = policy->getTruthValue()->get_mean();
            policy_fitness_pairs.push_back({policy, fitness});
        }
    }

    // Sort by fitness (descending)
    std::sort(policy_fitness_pairs.begin(), policy_fitness_pairs.end(),
              [](const auto& a, const auto& b) {
                  return a.second > b.second;
              });

    // Extract top policies
    std::vector<Handle> best_policies;
    size_t limit = std::min(count, policy_fitness_pairs.size());
    for (size_t i = 0; i < limit; ++i) {
        best_policies.push_back(policy_fitness_pairs[i].first);
    }

    return best_policies;
}

std::vector<double> PolicyOptimizer::getOptimizationHistory(Handle policy_handle) const
{
    auto it = _optimization_history.find(policy_handle);
    if (it != _optimization_history.end()) {
        return it->second;
    }
    return {};
}

void PolicyOptimizer::setMOSESParameters(size_t population_size,
                                        size_t max_generations,
                                        double mutation_rate,
                                        double crossover_rate)
{
    _population_size = population_size;
    _max_generations = max_generations;
    _mutation_rate = std::max(0.0, std::min(1.0, mutation_rate));
    _crossover_rate = std::max(0.0, std::min(1.0, crossover_rate));

    logger().debug() << "[PolicyOptimizer] MOSES parameters updated: pop=" << population_size
                    << ", gen=" << max_generations << ", mut=" << mutation_rate 
                    << ", cross=" << crossover_rate;
}

std::map<std::string, double> PolicyOptimizer::getOptimizationStatistics() const
{
    std::map<std::string, double> stats;
    
    stats["total_policies"] = static_cast<double>(_policy_registry.size());
    stats["moses_available"] = _moses_available ? 1.0 : 0.0;
    stats["population_size"] = static_cast<double>(_population_size);
    stats["max_generations"] = static_cast<double>(_max_generations);
    stats["mutation_rate"] = _mutation_rate;
    stats["crossover_rate"] = _crossover_rate;
    
    // Calculate average optimization rounds
    double total_rounds = 0.0;
    for (const auto& pair : _optimization_rounds) {
        total_rounds += pair.second;
    }
    stats["average_optimization_rounds"] = _optimization_rounds.empty() ? 0.0 : 
                                          total_rounds / _optimization_rounds.size();
    
    return stats;
}

// Private methods

void PolicyOptimizer::initializePolicyBase()
{
    _policy_base = _atomspace->add_node(CONCEPT_NODE, "PolicyBase");
    logger().debug() << "[PolicyOptimizer] Policy base initialized in AtomSpace";
}

void PolicyOptimizer::checkMOSESAvailability()
{
    // In a real implementation, this would check for MOSES library availability
    // For now, we'll simulate that MOSES might not be available
    _moses_available = true; // Assume available for this implementation
    
    if (!_moses_available) {
        logger().warn() << "[PolicyOptimizer] MOSES not available, using fallback optimization";
    }
}

Handle PolicyOptimizer::createPolicyAtom(const std::string& name, PolicyType type)
{
    std::string policy_name = "Policy_" + name;
    Handle policy_atom = _atomspace->add_node(CONCEPT_NODE, policy_name);
    
    // Link to policy base
    _atomspace->add_link(INHERITANCE_LINK, {policy_atom, _policy_base});
    
    // Add type information
    Handle type_node = _atomspace->add_node(CONCEPT_NODE, 
                                           "PolicyType_" + std::to_string(static_cast<int>(type)));
    _atomspace->add_link(EVALUATION_LINK, {
        _atomspace->add_node(PREDICATE_NODE, "hasPolicyType"),
        _atomspace->add_link(LIST_LINK, {policy_atom, type_node})
    });
    
    return policy_atom;
}

HandleSeq PolicyOptimizer::runMOSESOptimization(const HandleSeq& initial_structure,
                                               const std::vector<Handle>& training_data,
                                               OptimizationObjective objective)
{
    logger().debug() << "[PolicyOptimizer] Running MOSES optimization";

    // Simplified MOSES-like optimization
    // In a real implementation, this would interface with the actual MOSES library
    
    HandleSeq best_structure = initial_structure;
    double best_fitness = calculateFitness(initial_structure, training_data, objective);
    
    std::random_device rd;
    std::mt19937 gen(rd());

    // Simulate evolutionary process
    for (size_t generation = 0; generation < _max_generations; ++generation) {
        std::vector<std::pair<HandleSeq, double>> population;
        
        // Generate population
        for (size_t i = 0; i < _population_size; ++i) {
            HandleSeq candidate;
            
            if (i == 0) {
                candidate = best_structure; // Keep best from previous generation
            } else if (i < _population_size * _mutation_rate) {
                candidate = mutatePolicyStructure(best_structure);
            } else if (i < _population_size * (_mutation_rate + _crossover_rate)) {
                // Crossover with random selection
                HandleSeq parent2 = best_structure; // Simplified parent selection
                candidate = crossoverPolicyStructures(best_structure, parent2);
            } else {
                candidate = initial_structure; // Random initialization
            }
            
            double fitness = calculateFitness(candidate, training_data, objective);
            population.push_back({candidate, fitness});
        }
        
        // Select best candidate
        auto best_candidate = std::max_element(population.begin(), population.end(),
                                             [](const auto& a, const auto& b) {
                                                 return a.second < b.second;
                                             });
        
        if (best_candidate->second > best_fitness) {
            best_fitness = best_candidate->second;
            best_structure = best_candidate->first;
        }
    }

    logger().debug() << "[PolicyOptimizer] MOSES optimization complete, best fitness: " << best_fitness;
    return best_structure;
}

double PolicyOptimizer::calculateFitness(const HandleSeq& policy_structure,
                                        const std::vector<Handle>& data,
                                        OptimizationObjective objective)
{
    if (policy_structure.empty() || data.empty()) {
        return 0.0;
    }

    // Simplified fitness calculation
    // In practice, this would involve sophisticated evaluation based on:
    // - Policy performance on tasks
    // - Resource utilization
    // - Learning speed
    // - Generalization ability
    
    double fitness = 0.5; // Base fitness
    
    // Bonus for structure complexity (up to a point)
    double complexity_bonus = std::min(0.3, policy_structure.size() * 0.05);
    fitness += complexity_bonus;
    
    // Bonus for data coverage
    double data_bonus = std::min(0.2, data.size() * 0.01);
    fitness += data_bonus;
    
    // Objective-specific adjustments
    switch (objective) {
        case OptimizationObjective::MAXIMIZE_EFFICIENCY:
            // Penalize overly complex structures
            if (policy_structure.size() > 10) {
                fitness -= (policy_structure.size() - 10) * 0.02;
            }
            break;
        case OptimizationObjective::MAXIMIZE_REWARD:
            // Bonus for larger structures (more potential for reward)
            fitness += policy_structure.size() * 0.01;
            break;
        default:
            break;
    }
    
    return std::max(0.0, std::min(1.0, fitness));
}

HandleSeq PolicyOptimizer::mutatePolicyStructure(const HandleSeq& structure)
{
    if (structure.empty()) {
        return structure;
    }

    HandleSeq mutated = structure;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(0.0, 1.0);

    // Random mutation: add, remove, or modify components
    if (dis(gen) < 0.3 && mutated.size() > 1) {
        // Remove a random component
        size_t index = gen() % mutated.size();
        mutated.erase(mutated.begin() + index);
    } else if (dis(gen) < 0.3) {
        // Add a random component (simplified)
        Handle new_component = _atomspace->add_node(CONCEPT_NODE, 
                                                   "MutatedComponent_" + std::to_string(gen()));
        mutated.push_back(new_component);
    } else if (!mutated.empty()) {
        // Modify a random component (simplified)
        size_t index = gen() % mutated.size();
        Handle modified = _atomspace->add_node(CONCEPT_NODE, 
                                              "ModifiedComponent_" + std::to_string(gen()));
        mutated[index] = modified;
    }

    return mutated;
}

HandleSeq PolicyOptimizer::crossoverPolicyStructures(const HandleSeq& parent1, const HandleSeq& parent2)
{
    if (parent1.empty()) return parent2;
    if (parent2.empty()) return parent1;

    HandleSeq offspring;
    std::random_device rd;
    std::mt19937 gen(rd());

    // Simple crossover: take elements alternately from parents
    size_t max_size = std::max(parent1.size(), parent2.size());
    for (size_t i = 0; i < max_size; ++i) {
        if (i % 2 == 0 && i < parent1.size()) {
            offspring.push_back(parent1[i]);
        } else if (i < parent2.size()) {
            offspring.push_back(parent2[i]);
        }
    }

    return offspring;
}

void PolicyOptimizer::recordOptimizationResult(Handle policy_handle, double fitness)
{
    _optimization_history[policy_handle].push_back(fitness);
    
    // Keep history manageable
    if (_optimization_history[policy_handle].size() > 1000) {
        _optimization_history[policy_handle].erase(
            _optimization_history[policy_handle].begin());
    }
}