/*
 * opencog/agentzero/ReasoningEngine.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * ReasoningEngine Implementation
 * PLN-based inference and reasoning for Agent-Zero
 * Part of the AGENT-ZERO-GENESIS project
 */

#include "ReasoningEngine.h"
#include "AgentZeroCore.h"

#include <opencog/atoms/base/NodeTypes.h>
#include <opencog/atoms/base/LinkTypes.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/util/random.h>

#include <sstream>
#include <algorithm>
#include <cmath>
#include <random>

using namespace opencog;
using namespace opencog::agentzero;

ReasoningEngine::ReasoningEngine(AgentZeroCore* agent_core, AtomSpacePtr atomspace)
    : _agent_core(agent_core)
    , _atomspace(atomspace)
    , _enable_pln_reasoning(true)
    , _enable_ure_integration(true)
    , _enable_uncertainty_propagation(true)
    , _max_inference_steps(15)
    , _confidence_threshold(0.6)
    , _truth_value_threshold(0.5)
{
    logger().info() << "[ReasoningEngine] Initializing PLN-based reasoning engine";
    
    // Initialize core reasoning structures
    initializePLNIntegration();
    initializeUREIntegration();
    loadDefaultReasoningRules();
    
    logger().info() << "[ReasoningEngine] Reasoning engine initialized with " 
                   << _reasoning_rules.size() << " rules";
}

ReasoningEngine::~ReasoningEngine()
{
    logger().info() << "[ReasoningEngine] Shutting down reasoning engine";
    clearReasoningCache();
}

void ReasoningEngine::initializePLNIntegration()
{
    logger().debug() << "[ReasoningEngine] Initializing PLN integration";
    
    // Create PLN reasoning context
    _pln_context = _atomspace->add_node(CONCEPT_NODE, "PLN_ReasoningContext");
    _pln_context->setTruthValue(SimpleTruthValue::createTV(1.0, 1.0));
    
    // Initialize inference history tracking
    _inference_history = _atomspace->add_node(CONCEPT_NODE, "InferenceHistory");
    
    // Create reasoning cache
    _reasoning_cache = _atomspace->add_node(CONCEPT_NODE, "ReasoningCache");
    
    logger().debug() << "[ReasoningEngine] PLN integration initialized";
}

void ReasoningEngine::initializeUREIntegration()
{
    logger().debug() << "[ReasoningEngine] Initializing URE integration";
    
    // Create URE rule base
    _rule_base = _atomspace->add_node(CONCEPT_NODE, "URE_RuleBase");
    _rule_base->setTruthValue(SimpleTruthValue::createTV(1.0, 1.0));
    
    logger().debug() << "[ReasoningEngine] URE integration initialized";
}

void ReasoningEngine::loadDefaultReasoningRules()
{
    logger().debug() << "[ReasoningEngine] Loading default reasoning rules";
    
    // Deduction rule: If A->B and B->C then A->C
    ReasoningRule deduction_rule;
    deduction_rule.name = "deduction";
    deduction_rule.weight = 0.9;
    deduction_rule.rule_type = "deduction";
    deduction_rule.preconditions = {"implication", "implication"};
    deduction_rule.applicability_check = [](const std::vector<Handle>& facts) -> bool {
        // Check if we have two implications that can chain
        int implication_count = 0;
        for (const auto& fact : facts) {
            if (fact->get_type() == IMPLICATION_LINK) {
                implication_count++;
            }
        }
        return implication_count >= 2;
    };
    addReasoningRule(deduction_rule);
    
    // Modus Ponens rule: If A->B and A then B
    ReasoningRule modus_ponens;
    modus_ponens.name = "modus_ponens";
    modus_ponens.weight = 0.95;
    modus_ponens.rule_type = "forward_chaining";
    modus_ponens.preconditions = {"implication", "antecedent"};
    modus_ponens.applicability_check = [](const std::vector<Handle>& facts) -> bool {
        bool has_implication = false;
        bool has_antecedent = false;
        for (const auto& fact : facts) {
            if (fact->get_type() == IMPLICATION_LINK) {
                has_implication = true;
            } else {
                has_antecedent = true;
            }
        }
        return has_implication && has_antecedent;
    };
    addReasoningRule(modus_ponens);
    
    // Abduction rule: If B and A->B then maybe A
    ReasoningRule abduction;
    abduction.name = "abduction";
    abduction.weight = 0.7;
    abduction.rule_type = "abductive";
    abduction.preconditions = {"consequent", "implication"};
    abduction.applicability_check = [](const std::vector<Handle>& facts) -> bool {
        return facts.size() >= 2; // Simple check for demonstration
    };
    addReasoningRule(abduction);
    
    // Inheritance rule: If A isa B and B has property P then A has property P
    ReasoningRule inheritance;
    inheritance.name = "inheritance";
    inheritance.weight = 0.8;
    inheritance.rule_type = "inheritance";
    inheritance.preconditions = {"inheritance", "property"};
    inheritance.applicability_check = [](const std::vector<Handle>& facts) -> bool {
        for (const auto& fact : facts) {
            if (fact->get_type() == INHERITANCE_LINK) {
                return true;
            }
        }
        return false;
    };
    addReasoningRule(inheritance);
    
    logger().info() << "[ReasoningEngine] Loaded " << _reasoning_rules.size() << " default rules";
}

std::vector<ReasoningEngine::ReasoningResult> 
ReasoningEngine::reason(const std::vector<Handle>& premises, ReasoningMode mode, int max_steps)
{
    logger().debug() << "[ReasoningEngine] Starting reasoning with " << premises.size() 
                    << " premises, mode=" << static_cast<int>(mode) << ", max_steps=" << max_steps;
    
    std::vector<ReasoningResult> results;
    
    try {
        // Check reasoning cache first
        auto cached_results = retrieveCachedResults(premises);
        if (!cached_results.empty()) {
            logger().debug() << "[ReasoningEngine] Found " << cached_results.size() << " cached results";
            return cached_results;
        }
        
        // Perform reasoning based on mode
        switch (mode) {
            case ReasoningMode::FORWARD_CHAINING:
                results = performForwardChaining(premises, max_steps);
                break;
            case ReasoningMode::BACKWARD_CHAINING:
                // For backward chaining without a specific goal, use premises as goals
                for (const auto& premise : premises) {
                    auto backward_results = performBackwardChaining(premise, max_steps);
                    results.insert(results.end(), backward_results.begin(), backward_results.end());
                }
                break;
            case ReasoningMode::MIXED_CHAINING:
                if (!premises.empty()) {
                    results = performMixedChaining(premises, premises[0], max_steps);
                }
                break;
            case ReasoningMode::ABDUCTIVE:
                // Abductive reasoning to find explanations
                results = generateHypotheses(premises);
                break;
            case ReasoningMode::ANALOGICAL:
                // For analogical reasoning, we need at least two sets to compare
                if (premises.size() >= 2) {
                    std::vector<Handle> source(premises.begin(), premises.begin() + premises.size()/2);
                    std::vector<Handle> target(premises.begin() + premises.size()/2, premises.end());
                    results = analogicalReasoning(source, target);
                }
                break;
            case ReasoningMode::CAUSAL:
                // Simple causal reasoning implementation
                results = performForwardChaining(premises, max_steps);
                // Filter for causal relationships
                results.erase(std::remove_if(results.begin(), results.end(),
                    [](const ReasoningResult& r) {
                        return r.reasoning_type.find("causal") == std::string::npos;
                    }), results.end());
                break;
        }
        
        // Cache successful results
        for (const auto& result : results) {
            cacheReasoningResult(result);
        }
        
        logger().debug() << "[ReasoningEngine] Reasoning completed with " << results.size() << " results";
        
    } catch (const std::exception& e) {
        logger().error() << "[ReasoningEngine] Error during reasoning: " << e.what();
    }
    
    return results;
}

std::vector<ReasoningEngine::ReasoningResult>
ReasoningEngine::performForwardChaining(const std::vector<Handle>& premises, int max_steps)
{
    logger().debug() << "[ReasoningEngine] Performing forward chaining with " << premises.size() << " premises";
    
    std::vector<ReasoningResult> results;
    std::set<Handle> derived_facts(premises.begin(), premises.end());
    std::set<Handle> new_facts = derived_facts;
    
    for (int step = 0; step < max_steps && !new_facts.empty(); ++step) {
        std::set<Handle> step_new_facts;
        
        // Find applicable rules
        std::vector<Handle> current_facts(new_facts.begin(), new_facts.end());
        auto applicable_rules = findApplicableRules(current_facts);
        
        // Apply each applicable rule
        for (const auto& rule_atom : applicable_rules) {
            // Find the corresponding reasoning rule
            auto rule_it = std::find_if(_reasoning_rules.begin(), _reasoning_rules.end(),
                [rule_atom](const ReasoningRule& rule) {
                    return rule.rule_atom == rule_atom;
                });
            
            if (rule_it != _reasoning_rules.end()) {
                try {
                    auto result = applyRule(*rule_it, current_facts);
                    
                    if (result.conclusion != Handle::UNDEFINED && 
                        result.confidence >= _confidence_threshold) {
                        
                        // Add to results
                        result.reasoning_type = "forward_chaining";
                        result.inference_steps = step + 1;
                        results.push_back(result);
                        
                        // Add new fact for next iteration
                        if (derived_facts.find(result.conclusion) == derived_facts.end()) {
                            step_new_facts.insert(result.conclusion);
                            derived_facts.insert(result.conclusion);
                        }
                    }
                } catch (const std::exception& e) {
                    logger().warn() << "[ReasoningEngine] Error applying rule " << rule_it->name 
                                   << ": " << e.what();
                }
            }
        }
        
        new_facts = step_new_facts;
        
        if (step_new_facts.empty()) {
            logger().debug() << "[ReasoningEngine] No new facts derived at step " << step;
            break;
        }
    }
    
    logger().debug() << "[ReasoningEngine] Forward chaining completed with " 
                    << results.size() << " inferences";
    
    return results;
}

std::vector<ReasoningEngine::ReasoningResult>
ReasoningEngine::performBackwardChaining(const Handle& goal, int max_steps)
{
    logger().debug() << "[ReasoningEngine] Performing backward chaining for goal";
    
    std::vector<ReasoningResult> results;
    std::set<Handle> goals_to_prove = {goal};
    std::set<Handle> proven_goals;
    
    for (int step = 0; step < max_steps && !goals_to_prove.empty(); ++step) {
        std::set<Handle> new_goals;
        
        for (const auto& current_goal : goals_to_prove) {
            // Find rules that could derive this goal
            std::vector<Handle> facts_context = {current_goal};
            auto applicable_rules = findApplicableRules(facts_context);
            
            for (const auto& rule_atom : applicable_rules) {
                auto rule_it = std::find_if(_reasoning_rules.begin(), _reasoning_rules.end(),
                    [rule_atom](const ReasoningRule& rule) {
                        return rule.rule_atom == rule_atom;
                    });
                
                if (rule_it != _reasoning_rules.end()) {
                    try {
                        // For backward chaining, we need to find what premises would lead to the goal
                        ReasoningResult result;
                        result.conclusion = current_goal;
                        result.premises = {current_goal}; // Simplified for demonstration
                        result.confidence = 0.7; // Default backward chaining confidence
                        result.reasoning_type = "backward_chaining";
                        result.inference_steps = step + 1;
                        result.rule_chain = {rule_atom};
                        result.explanation = generateExplanation(result);
                        
                        if (result.confidence >= _confidence_threshold) {
                            results.push_back(result);
                            proven_goals.insert(current_goal);
                        }
                    } catch (const std::exception& e) {
                        logger().warn() << "[ReasoningEngine] Error in backward chaining: " << e.what();
                    }
                }
            }
        }
        
        // Update goals for next iteration
        goals_to_prove = new_goals;
    }
    
    return results;
}

std::vector<ReasoningEngine::ReasoningResult>
ReasoningEngine::performMixedChaining(const std::vector<Handle>& premises, const Handle& goal, int max_steps)
{
    logger().debug() << "[ReasoningEngine] Performing mixed chaining";
    
    std::vector<ReasoningResult> results;
    
    // Combine forward and backward chaining
    int forward_steps = max_steps / 2;
    int backward_steps = max_steps - forward_steps;
    
    // Forward chaining from premises
    auto forward_results = performForwardChaining(premises, forward_steps);
    results.insert(results.end(), forward_results.begin(), forward_results.end());
    
    // Backward chaining from goal
    auto backward_results = performBackwardChaining(goal, backward_steps);
    results.insert(results.end(), backward_results.begin(), backward_results.end());
    
    // Mark as mixed chaining
    for (auto& result : results) {
        result.reasoning_type = "mixed_chaining";
    }
    
    return results;
}

std::vector<Handle> ReasoningEngine::findApplicableRules(const std::vector<Handle>& facts)
{
    std::vector<Handle> applicable_rules;
    
    for (const auto& rule : _reasoning_rules) {
        try {
            if (rule.applicability_check(facts)) {
                if (rule.rule_atom != Handle::UNDEFINED) {
                    applicable_rules.push_back(rule.rule_atom);
                }
            }
        } catch (const std::exception& e) {
            logger().warn() << "[ReasoningEngine] Error checking rule applicability: " << e.what();
        }
    }
    
    return applicable_rules;
}

ReasoningEngine::ReasoningResult 
ReasoningEngine::applyRule(const ReasoningRule& rule, const std::vector<Handle>& facts)
{
    ReasoningResult result;
    result.premises = facts;
    result.reasoning_type = rule.rule_type;
    result.rule_chain = {rule.rule_atom};
    
    try {
        // Simple rule application - create a conclusion based on rule type
        if (rule.name == "modus_ponens" && facts.size() >= 2) {
            // Find implication and antecedent
            Handle implication, antecedent;
            for (const auto& fact : facts) {
                if (fact->get_type() == IMPLICATION_LINK) {
                    implication = fact;
                } else {
                    antecedent = fact;
                }
            }
            
            if (implication != Handle::UNDEFINED && antecedent != Handle::UNDEFINED) {
                // Create conclusion - this is simplified for demonstration
                result.conclusion = _atomspace->add_node(CONCEPT_NODE, "modus_ponens_conclusion");
                result.confidence = 0.85;
            }
        } else if (rule.name == "deduction" && facts.size() >= 2) {
            // Chain implications
            result.conclusion = _atomspace->add_node(CONCEPT_NODE, "deduction_conclusion");
            result.confidence = 0.8;
        } else if (rule.name == "abduction") {
            // Generate hypothesis
            result.conclusion = _atomspace->add_node(CONCEPT_NODE, "abductive_hypothesis");
            result.confidence = 0.6;
        } else {
            // Default rule application
            result.conclusion = _atomspace->add_node(CONCEPT_NODE, "rule_conclusion");
            result.confidence = rule.weight * 0.8;
        }
        
        // Set truth value for the conclusion
        if (result.conclusion != Handle::UNDEFINED) {
            auto tv = computeResultingTruthValue(facts, rule);
            result.conclusion->setTruthValue(tv);
        }
        
        result.explanation = generateExplanation(result);
        
    } catch (const std::exception& e) {
        logger().error() << "[ReasoningEngine] Error applying rule " << rule.name << ": " << e.what();
        result.conclusion = Handle::UNDEFINED;
        result.confidence = 0.0;
    }
    
    return result;
}

TruthValuePtr ReasoningEngine::computeResultingTruthValue(const std::vector<Handle>& premises,
                                                        const ReasoningRule& rule)
{
    // Compute truth value based on premises and rule weight
    double mean_strength = 0.0;
    double mean_confidence = 0.0;
    int valid_premises = 0;
    
    for (const auto& premise : premises) {
        auto tv = premise->getTruthValue();
        if (tv) {
            mean_strength += tv->get_mean();
            mean_confidence += tv->get_confidence();
            valid_premises++;
        }
    }
    
    if (valid_premises > 0) {
        mean_strength /= valid_premises;
        mean_confidence /= valid_premises;
    } else {
        mean_strength = 0.5;
        mean_confidence = 0.5;
    }
    
    // Apply rule weight
    double result_strength = mean_strength * rule.weight;
    double result_confidence = mean_confidence * rule.weight;
    
    // Ensure values are within valid bounds
    result_strength = std::max(0.0, std::min(1.0, result_strength));
    result_confidence = std::max(0.0, std::min(1.0, result_confidence));
    
    return SimpleTruthValue::createTV(result_strength, result_confidence);
}

std::vector<ReasoningEngine::ReasoningResult> 
ReasoningEngine::generateHypotheses(const std::vector<Handle>& observations,
                                   const std::vector<Handle>& hypothesis_templates)
{
    logger().debug() << "[ReasoningEngine] Generating hypotheses from " 
                    << observations.size() << " observations";
    
    std::vector<ReasoningResult> hypotheses;
    
    // Use abductive reasoning to generate explanations
    for (const auto& observation : observations) {
        ReasoningResult hypothesis;
        hypothesis.conclusion = _atomspace->add_node(CONCEPT_NODE, 
                                                   "hypothesis_for_" + observation->get_name());
        hypothesis.premises = {observation};
        hypothesis.confidence = 0.6; // Hypotheses have moderate confidence
        hypothesis.reasoning_type = "abductive";
        hypothesis.inference_steps = 1;
        hypothesis.explanation = "Hypothesis generated to explain observation: " + observation->get_name();
        
        // Set truth value
        hypothesis.conclusion->setTruthValue(SimpleTruthValue::createTV(0.6, 0.8));
        
        hypotheses.push_back(hypothesis);
    }
    
    return hypotheses;
}

std::vector<ReasoningEngine::ReasoningResult>
ReasoningEngine::analogicalReasoning(const std::vector<Handle>& source_case,
                                    const std::vector<Handle>& target_case,
                                    const std::vector<Handle>& mapping_rules)
{
    logger().debug() << "[ReasoningEngine] Performing analogical reasoning";
    
    std::vector<ReasoningResult> results;
    
    // Simple analogical reasoning: if source and target share structure, 
    // transfer properties
    if (source_case.size() == target_case.size()) {
        ReasoningResult analogy;
        analogy.conclusion = _atomspace->add_node(CONCEPT_NODE, "analogical_conclusion");
        analogy.premises = source_case;
        analogy.premises.insert(analogy.premises.end(), target_case.begin(), target_case.end());
        analogy.confidence = 0.7;
        analogy.reasoning_type = "analogical";
        analogy.inference_steps = 1;
        analogy.explanation = "Analogical transfer from source case to target case";
        
        results.push_back(analogy);
    }
    
    return results;
}

bool ReasoningEngine::addReasoningRule(const ReasoningRule& rule)
{
    logger().debug() << "[ReasoningEngine] Adding reasoning rule: " << rule.name;
    
    try {
        // Create AtomSpace representation of the rule if not provided
        ReasoningRule new_rule = rule;
        if (new_rule.rule_atom == Handle::UNDEFINED) {
            new_rule.rule_atom = _atomspace->add_node(CONCEPT_NODE, "rule_" + rule.name);
        }
        
        _reasoning_rules.push_back(new_rule);
        _rules_by_type[rule.rule_type].push_back(new_rule);
        
        return true;
    } catch (const std::exception& e) {
        logger().error() << "[ReasoningEngine] Error adding rule " << rule.name << ": " << e.what();
        return false;
    }
}

std::string ReasoningEngine::generateExplanation(const ReasoningResult& result)
{
    std::ostringstream explanation;
    
    explanation << "Reasoning result: ";
    if (result.conclusion != Handle::UNDEFINED) {
        explanation << result.conclusion->get_name();
    } else {
        explanation << "undefined";
    }
    
    explanation << " (confidence: " << result.confidence << ")";
    explanation << " using " << result.reasoning_type << " reasoning";
    explanation << " in " << result.inference_steps << " steps";
    
    if (!result.premises.empty()) {
        explanation << " from premises: ";
        for (size_t i = 0; i < result.premises.size(); ++i) {
            if (i > 0) explanation << ", ";
            explanation << result.premises[i]->get_name();
        }
    }
    
    return explanation.str();
}

void ReasoningEngine::cacheReasoningResult(const ReasoningResult& result)
{
    // Simple caching - in a full implementation, this would use the AtomSpace
    logger().debug() << "[ReasoningEngine] Caching reasoning result";
}

std::vector<ReasoningEngine::ReasoningResult>
ReasoningEngine::retrieveCachedResults(const std::vector<Handle>& query)
{
    // Simple implementation - return empty for now
    return std::vector<ReasoningResult>();
}

std::map<std::string, int> ReasoningEngine::getReasoningStatistics()
{
    std::map<std::string, int> stats;
    
    stats["total_rules"] = static_cast<int>(_reasoning_rules.size());
    stats["pln_enabled"] = _enable_pln_reasoning ? 1 : 0;
    stats["ure_enabled"] = _enable_ure_integration ? 1 : 0;
    stats["max_inference_steps"] = _max_inference_steps;
    stats["cached_results"] = 0; // Would implement proper caching
    
    // Count rules by type
    for (const auto& type_rules : _rules_by_type) {
        stats["rules_" + type_rules.first] = static_cast<int>(type_rules.second.size());
    }
    
    return stats;
}

void ReasoningEngine::configurePLN(bool enable_pln, double confidence_threshold, double truth_threshold)
{
    logger().info() << "[ReasoningEngine] Configuring PLN: enabled=" << enable_pln
                   << ", confidence_threshold=" << confidence_threshold
                   << ", truth_threshold=" << truth_threshold;
    
    _enable_pln_reasoning = enable_pln;
    _confidence_threshold = confidence_threshold;
    _truth_value_threshold = truth_threshold;
}

void ReasoningEngine::configureURE(bool enable_ure, int max_iterations, double complexity_penalty)
{
    logger().info() << "[ReasoningEngine] Configuring URE: enabled=" << enable_ure
                   << ", max_iterations=" << max_iterations
                   << ", complexity_penalty=" << complexity_penalty;
    
    _enable_ure_integration = enable_ure;
    // In a full implementation, we would configure the actual URE here
}

std::string ReasoningEngine::getStatusInfo() const
{
    std::ostringstream status;
    
    status << "{";
    status << "\"reasoning_engine_status\": \"active\",";
    status << "\"pln_enabled\": " << (_enable_pln_reasoning ? "true" : "false") << ",";
    status << "\"ure_enabled\": " << (_enable_ure_integration ? "true" : "false") << ",";
    status << "\"total_rules\": " << _reasoning_rules.size() << ",";
    status << "\"max_inference_steps\": " << _max_inference_steps << ",";
    status << "\"confidence_threshold\": " << _confidence_threshold;
    status << "}";
    
    return status.str();
}

bool ReasoningEngine::processReasoningCycle()
{
    logger().debug() << "[ReasoningEngine] Processing reasoning cycle";
    
    try {
        // In a full implementation, this would perform ongoing reasoning tasks
        // such as maintaining consistency, updating truth values, etc.
        return true;
        
    } catch (const std::exception& e) {
        logger().error() << "[ReasoningEngine] Error in reasoning cycle: " << e.what();
        return false;
    }
}

void ReasoningEngine::clearReasoningCache()
{
    logger().debug() << "[ReasoningEngine] Clearing reasoning cache";
    _applied_rules_cache.clear();
    _active_reasoning_tasks.clear();
}