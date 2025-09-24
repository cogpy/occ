/*
 * src/ExperienceManager.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * ExperienceManager Implementation
 * Manages agent's experiential memory and learning from experiences
 * Part of the AGENT-ZERO-GENESIS project - Phase 5: Learning & Adaptation
 */

#include <sstream>
#include <algorithm>
#include <iomanip>
#include <ctime>
#include <random>

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>

#include "opencog/agentzero/learning/ExperienceManager.h"

// Forward declaration to avoid circular dependency
namespace opencog { namespace agentzero { class AgentZeroCore; } }

using namespace opencog;
using namespace opencog::agentzero::learning;

ExperienceManager::ExperienceManager(AgentZeroCore* agent_core, AtomSpacePtr atomspace)
    : _agent_core(agent_core)
    , _atomspace(atomspace)
    , _experience_base(Handle::UNDEFINED)
    , _episodic_memory(Handle::UNDEFINED)
    , _experience_patterns(Handle::UNDEFINED)
    , _learning_outcomes(Handle::UNDEFINED)
    , _skill_experiences(Handle::UNDEFINED)
    , _goal_experiences(Handle::UNDEFINED)
    , _temporal_context(Handle::UNDEFINED)
    , _enable_pattern_discovery(true)
    , _enable_moses_integration(false)
    , _enable_temporal_modeling(true)
    , _enable_emotional_learning(false)
    , _experience_retention_threshold(0.3)
    , _max_recent_experiences(100)
    , _pattern_significance_threshold(0.6)
    , _moses_available(false)
    , _moses_policy_space(Handle::UNDEFINED)
{
    logger().info() << "[ExperienceManager] Constructor: Initializing experience management";
    
    // Check for MOSES availability
    try {
        // Try to create a MOSES-related atom to test availability
        Handle moses_test = _atomspace->add_node(CONCEPT_NODE, "moses_test_node");
        if (moses_test != Handle::UNDEFINED) {
            _moses_available = true;
            _enable_moses_integration = true;
            logger().info() << "[ExperienceManager] MOSES integration available";
        }
    } catch (...) {
        logger().warn() << "[ExperienceManager] MOSES not available, disabling MOSES integration";
        _moses_available = false;
        _enable_moses_integration = false;
    }
    
    initializeExperienceStructures();
}

ExperienceManager::~ExperienceManager()
{
    logger().info() << "[ExperienceManager] Destructor: Cleaning up experience management resources";
    
    // Perform final consolidation
    if (_atomspace) {
        consolidateExperiences();
    }
}

void ExperienceManager::initializeExperienceStructures()
{
    logger().debug() << "[ExperienceManager] Initializing experience management structures";
    
    try {
        // Create core experience management atoms
        _experience_base = _atomspace->add_node(CONCEPT_NODE, "ExperienceBase");
        _episodic_memory = _atomspace->add_node(CONCEPT_NODE, "EpisodicMemory");
        _experience_patterns = _atomspace->add_node(CONCEPT_NODE, "ExperiencePatterns");
        _learning_outcomes = _atomspace->add_node(CONCEPT_NODE, "LearningOutcomes");
        _skill_experiences = _atomspace->add_node(CONCEPT_NODE, "SkillExperiences");
        _goal_experiences = _atomspace->add_node(CONCEPT_NODE, "GoalExperiences");
        _temporal_context = _atomspace->add_node(CONCEPT_NODE, "TemporalContext");
        
        if (_enable_moses_integration && _moses_available) {
            _moses_policy_space = _atomspace->add_node(CONCEPT_NODE, "MOSESPolicySpace");
        }
        
        // Create organizational structure
        Handle experience_hierarchy = _atomspace->add_link(INHERITANCE_LINK, {
            _episodic_memory, _experience_base
        });
        
        Handle pattern_hierarchy = _atomspace->add_link(INHERITANCE_LINK, {
            _experience_patterns, _experience_base
        });
        
        Handle outcome_hierarchy = _atomspace->add_link(INHERITANCE_LINK, {
            _learning_outcomes, _experience_base
        });
        
        // Set truth values for organizational structure
        TruthValuePtr high_confidence = SimpleTruthValue::createTV(0.9, 0.9);
        experience_hierarchy->setTruthValue(high_confidence);
        pattern_hierarchy->setTruthValue(high_confidence);
        outcome_hierarchy->setTruthValue(high_confidence);
        
        logger().info() << "[ExperienceManager] Experience structures initialized successfully";
        
    } catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Failed to initialize experience structures: " << e.what();
        throw;
    }
}

Handle ExperienceManager::recordExperience(const std::string& description,
                                         ExperienceType type,
                                         ExperienceOutcome outcome,
                                         const ExperienceContext& context,
                                         const std::vector<Handle>& actions,
                                         const std::vector<Handle>& consequences)
{
    logger().debug() << "[ExperienceManager] Recording experience: " << description;
    
    try {
        // Create experience record
        Experience exp;
        exp.description = description;
        exp.type = type;
        exp.outcome = outcome;
        exp.context = context;
        exp.actions = actions;
        exp.consequences = consequences;
        exp.learning_value = calculateLearningValue(exp);
        
        // Determine importance based on outcome and learning value
        if (outcome == ExperienceOutcome::SUCCESS && exp.learning_value > 0.8) {
            exp.importance = ExperienceImportance::CRITICAL;
        } else if (outcome == ExperienceOutcome::FAILURE && exp.learning_value > 0.6) {
            exp.importance = ExperienceImportance::HIGH;
        } else if (exp.learning_value > 0.4) {
            exp.importance = ExperienceImportance::MEDIUM;
        } else {
            exp.importance = ExperienceImportance::LOW;
        }
        
        // Create AtomSpace representation
        exp.experience_atom = createExperienceAtom(exp);
        
        if (exp.experience_atom == Handle::UNDEFINED) {
            logger().error() << "[ExperienceManager] Failed to create experience atom";
            return Handle::UNDEFINED;
        }
        
        // Store experience
        _experience_registry[exp.experience_atom] = exp;
        _experiences_by_type[type].push_back(exp);
        _recent_experiences.push_back(exp);
        
        // Manage recent experiences size
        if (_recent_experiences.size() > _max_recent_experiences) {
            _recent_experiences.erase(_recent_experiences.begin());
        }
        
        // Index and process experience
        indexExperience(exp);
        updateExperiencePatterns(exp);
        updateSkillExperienceMapping(exp);
        
        // Check if this is a significant experience
        if (exp.learning_value > _pattern_significance_threshold || 
            exp.importance >= ExperienceImportance::HIGH) {
            _significant_experiences.insert(exp.experience_atom);
        }
        
        logger().info() << "[ExperienceManager] Experience recorded successfully: " 
                       << exp.experience_atom->to_string();
        
        return exp.experience_atom;
        
    } catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Failed to record experience: " << e.what();
        return Handle::UNDEFINED;
    }
}

Handle ExperienceManager::recordExperience(const std::string& description,
                                         ExperienceType type,
                                         ExperienceOutcome outcome)
{
    // Create default context with current timestamp
    ExperienceContext ctx;
    ctx.timestamp = std::chrono::system_clock::now();
    ctx.confidence_level = 0.5;
    
    // Try to capture current agent state (simplified)
    try {
        Handle current_state = _atomspace->add_node(CONCEPT_NODE, "CurrentAgentState");
        ctx.agent_state.push_back(current_state);
    } catch (...) {
        logger().warn() << "[ExperienceManager] Could not capture current agent state";
    }
    
    return recordExperience(description, type, outcome, ctx, {}, {});
}

Handle ExperienceManager::createExperienceAtom(const Experience& exp)
{
    try {
        // Create unique experience ID
        auto timestamp = std::chrono::duration_cast<std::chrono::milliseconds>(
            exp.context.timestamp.time_since_epoch()).count();
        
        std::string exp_id = "Experience_" + std::to_string(timestamp) + "_" + 
                            std::to_string(static_cast<int>(exp.type));
        
        Handle experience_atom = _atomspace->add_node(CONCEPT_NODE, exp_id);
        
        // Create description atom
        Handle desc_atom = _atomspace->add_node(CONCEPT_NODE, exp.description);
        Handle desc_link = _atomspace->add_link(INHERITANCE_LINK, {desc_atom, experience_atom});
        
        // Create type atom
        std::string type_str = "ExperienceType_" + std::to_string(static_cast<int>(exp.type));
        Handle type_atom = _atomspace->add_node(CONCEPT_NODE, type_str);
        Handle type_link = _atomspace->add_link(INHERITANCE_LINK, {experience_atom, type_atom});
        
        // Create outcome atom
        std::string outcome_str = "ExperienceOutcome_" + std::to_string(static_cast<int>(exp.outcome));
        Handle outcome_atom = _atomspace->add_node(CONCEPT_NODE, outcome_str);
        Handle outcome_link = _atomspace->add_link(EVALUATION_LINK, {
            _atomspace->add_node(PREDICATE_NODE, "hasOutcome"),
            _atomspace->add_link(LIST_LINK, {experience_atom, outcome_atom})
        });
        
        // Create timestamp atom
        Handle timestamp_atom = _atomspace->add_node(NUMBER_NODE, std::to_string(timestamp));
        Handle timestamp_link = _atomspace->add_link(EVALUATION_LINK, {
            _atomspace->add_node(PREDICATE_NODE, "timestamp"),
            _atomspace->add_link(LIST_LINK, {experience_atom, timestamp_atom})
        });
        
        // Create learning value atom
        Handle learning_value_atom = _atomspace->add_node(NUMBER_NODE, 
                                                         std::to_string(exp.learning_value));
        Handle learning_link = _atomspace->add_link(EVALUATION_LINK, {
            _atomspace->add_node(PREDICATE_NODE, "learningValue"),
            _atomspace->add_link(LIST_LINK, {experience_atom, learning_value_atom})
        });
        
        // Link to episodic memory
        Handle memory_link = _atomspace->add_link(MEMBER_LINK, {experience_atom, _episodic_memory});
        
        // Set truth value based on learning value and importance
        double truth_strength = std::min(0.9, 0.5 + exp.learning_value * 0.4);
        double confidence = 0.8; // High confidence in recorded experiences
        TruthValuePtr tv = SimpleTruthValue::createTV(truth_strength, confidence);
        experience_atom->setTruthValue(tv);
        
        return experience_atom;
        
    } catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Failed to create experience atom: " << e.what();
        return Handle::UNDEFINED;
    }
}

void ExperienceManager::indexExperience(const Experience& exp)
{
    // Add to temporal index
    if (_enable_temporal_modeling) {
        _temporal_index[exp.context.timestamp].push_back(exp.experience_atom);
    }
    
    // Update experience sequences for pattern discovery
    if (!_experience_sequences.empty()) {
        // Add to the most recent sequence
        _experience_sequences.back().push_back(exp.experience_atom);
    } else {
        // Create first sequence
        _experience_sequences.push_back({exp.experience_atom});
    }
    
    // Start new sequence if this experience represents a significant break
    if (exp.type == ExperienceType::GOAL_PURSUIT || 
        exp.importance >= ExperienceImportance::HIGH) {
        _experience_sequences.push_back({exp.experience_atom});
    }
}

void ExperienceManager::updateExperiencePatterns(const Experience& exp)
{
    if (!_enable_pattern_discovery) return;
    
    try {
        // Extract patterns from this experience
        std::vector<Handle> patterns = extractPatternsFromExperience(exp);
        
        for (const Handle& pattern : patterns) {
            std::string pattern_str = pattern->to_string();
            _pattern_library[pattern_str].push_back(exp.experience_atom);
            
            // Update success rate for this pattern
            if (exp.outcome == ExperienceOutcome::SUCCESS) {
                _pattern_success_rates[pattern] = 
                    (_pattern_success_rates[pattern] * (_pattern_library[pattern_str].size() - 1) + 1.0) /
                    _pattern_library[pattern_str].size();
            } else if (exp.outcome == ExperienceOutcome::FAILURE) {
                _pattern_success_rates[pattern] = 
                    (_pattern_success_rates[pattern] * (_pattern_library[pattern_str].size() - 1)) /
                    _pattern_library[pattern_str].size();
            }
        }
        
    } catch (const std::exception& e) {
        logger().warn() << "[ExperienceManager] Error updating experience patterns: " << e.what();
    }
}

std::vector<Handle> ExperienceManager::extractPatternsFromExperience(const Experience& exp)
{
    std::vector<Handle> patterns;
    
    try {
        // Pattern 1: Action-Outcome pattern
        if (!exp.actions.empty() && !exp.consequences.empty()) {
            Handle action_pattern = _atomspace->add_link(IMPLICATION_LINK, {
                _atomspace->add_link(AND_LINK, exp.actions),
                _atomspace->add_link(AND_LINK, exp.consequences)
            });
            patterns.push_back(action_pattern);
        }
        
        // Pattern 2: Context-Action pattern (what actions work in which contexts)
        if (!exp.context.environmental_state.empty() && !exp.actions.empty()) {
            Handle context_pattern = _atomspace->add_link(IMPLICATION_LINK, {
                _atomspace->add_link(AND_LINK, exp.context.environmental_state),
                _atomspace->add_link(AND_LINK, exp.actions)
            });
            patterns.push_back(context_pattern);
        }
        
        // Pattern 3: Goal-Achievement pattern
        if (!exp.context.active_goals.empty() && exp.outcome == ExperienceOutcome::SUCCESS) {
            Handle goal_pattern = _atomspace->add_link(IMPLICATION_LINK, {
                _atomspace->add_link(AND_LINK, exp.context.active_goals),
                _atomspace->add_node(CONCEPT_NODE, "SuccessfulOutcome")
            });
            patterns.push_back(goal_pattern);
        }
        
        // Pattern 4: Skill application pattern
        if (!exp.context.applied_skills.empty()) {
            Handle skill_pattern = _atomspace->add_link(EVALUATION_LINK, {
                _atomspace->add_node(PREDICATE_NODE, "skillApplicationPattern"),
                _atomspace->add_link(LIST_LINK, exp.context.applied_skills)
            });
            patterns.push_back(skill_pattern);
        }
        
    } catch (const std::exception& e) {
        logger().warn() << "[ExperienceManager] Error extracting patterns: " << e.what();
    }
    
    return patterns;
}

void ExperienceManager::updateSkillExperienceMapping(const Experience& exp)
{
    for (const Handle& skill : exp.context.applied_skills) {
        _skill_experience_map[skill].push_back(exp.experience_atom);
    }
}

double ExperienceManager::calculateLearningValue(const Experience& exp)
{
    double learning_value = 0.0;
    
    // Base value depends on outcome
    switch (exp.outcome) {
        case ExperienceOutcome::SUCCESS:
            learning_value = 0.7;
            break;
        case ExperienceOutcome::FAILURE:
            learning_value = 0.8; // Failures often provide more learning
            break;
        case ExperienceOutcome::UNEXPECTED_OUTCOME:
            learning_value = 0.9; // Unexpected outcomes are very valuable for learning
            break;
        case ExperienceOutcome::PARTIAL_SUCCESS:
            learning_value = 0.6;
            break;
        case ExperienceOutcome::LEARNING_OPPORTUNITY:
            learning_value = 0.85;
            break;
        default:
            learning_value = 0.4;
    }
    
    // Adjust based on experience type
    switch (exp.type) {
        case ExperienceType::PROBLEM_SOLVING:
            learning_value += 0.1;
            break;
        case ExperienceType::LEARNING_EPISODE:
            learning_value += 0.15;
            break;
        case ExperienceType::UNEXPECTED:
            learning_value += 0.1;
            break;
        default:
            break;
    }
    
    // Adjust based on context complexity
    double context_complexity = 
        (exp.context.environmental_state.size() + 
         exp.context.agent_state.size() + 
         exp.context.active_goals.size()) / 10.0;
    learning_value += std::min(0.1, context_complexity * 0.02);
    
    // Ensure value is within bounds
    return std::max(0.0, std::min(1.0, learning_value));
}

std::vector<Handle> ExperienceManager::getSimilarExperiences(const std::vector<Handle>& current_context,
                                                           ExperienceType experience_type,
                                                           size_t max_results)
{
    std::vector<std::pair<Handle, double>> similarity_scores;
    
    // Get experiences of the specified type
    if (_experiences_by_type.find(experience_type) == _experiences_by_type.end()) {
        return {};
    }
    
    const std::vector<Experience>& experiences = _experiences_by_type[experience_type];
    
    for (const Experience& exp : experiences) {
        // Calculate context similarity
        std::vector<Handle> exp_context_features = getContextualFeatures(exp.context);
        
        // Simple similarity metric based on common context elements
        std::set<Handle> current_set(current_context.begin(), current_context.end());
        std::set<Handle> exp_set(exp_context_features.begin(), exp_context_features.end());
        
        std::vector<Handle> intersection;
        std::set_intersection(current_set.begin(), current_set.end(),
                            exp_set.begin(), exp_set.end(),
                            std::back_inserter(intersection));
        
        double similarity = 0.0;
        if (!current_set.empty() && !exp_set.empty()) {
            similarity = static_cast<double>(intersection.size()) / 
                        std::max(current_set.size(), exp_set.size());
        }
        
        if (similarity > 0.1) { // Minimum similarity threshold
            similarity_scores.push_back({exp.experience_atom, similarity});
        }
    }
    
    // Sort by similarity score
    std::sort(similarity_scores.begin(), similarity_scores.end(),
              [](const auto& a, const auto& b) { return a.second > b.second; });
    
    // Return top results
    std::vector<Handle> results;
    for (size_t i = 0; i < std::min(max_results, similarity_scores.size()); ++i) {
        results.push_back(similarity_scores[i].first);
    }
    
    return results;
}

std::vector<Handle> ExperienceManager::getContextualFeatures(const ExperienceContext& context)
{
    std::vector<Handle> features;
    
    // Add environmental state features
    features.insert(features.end(), context.environmental_state.begin(), context.environmental_state.end());
    
    // Add agent state features
    features.insert(features.end(), context.agent_state.begin(), context.agent_state.end());
    
    // Add goal features
    features.insert(features.end(), context.active_goals.begin(), context.active_goals.end());
    
    // Add skill features
    features.insert(features.end(), context.applied_skills.begin(), context.applied_skills.end());
    
    return features;
}

std::vector<Handle> ExperienceManager::getExperiencesByOutcome(ExperienceOutcome outcome, size_t limit)
{
    std::vector<Handle> results;
    
    for (const auto& type_experiences : _experiences_by_type) {
        for (const Experience& exp : type_experiences.second) {
            if (exp.outcome == outcome) {
                results.push_back(exp.experience_atom);
                if (results.size() >= limit) {
                    return results;
                }
            }
        }
    }
    
    return results;
}

size_t ExperienceManager::discoverExperiencePatterns(ExperienceType experience_type)
{
    if (!_enable_pattern_discovery) return 0;
    
    logger().info() << "[ExperienceManager] Discovering patterns for experience type: " 
                   << static_cast<int>(experience_type);
    
    size_t patterns_discovered = 0;
    
    try {
        std::vector<Experience> target_experiences;
        
        if (experience_type == ExperienceType::ACTION_OUTCOME) {
            // Get all experiences for pattern discovery
            for (const auto& type_exp_pair : _experiences_by_type) {
                target_experiences.insert(target_experiences.end(),
                                        type_exp_pair.second.begin(),
                                        type_exp_pair.second.end());
            }
        } else {
            // Get experiences of specific type
            if (_experiences_by_type.find(experience_type) != _experiences_by_type.end()) {
                target_experiences = _experiences_by_type[experience_type];
            }
        }
        
        if (target_experiences.size() < 3) {
            logger().debug() << "[ExperienceManager] Insufficient experiences for pattern discovery";
            return 0;
        }
        
        // Discover sequential patterns
        std::vector<Handle> sequential_patterns = discoverSequentialPatterns(target_experiences);
        patterns_discovered += sequential_patterns.size();
        
        // Discover causal patterns
        std::vector<Handle> causal_patterns = discoverCausalPatterns(target_experiences);
        patterns_discovered += causal_patterns.size();
        
        // Analyze pattern outcomes
        std::vector<Handle> all_patterns;
        all_patterns.insert(all_patterns.end(), sequential_patterns.begin(), sequential_patterns.end());
        all_patterns.insert(all_patterns.end(), causal_patterns.begin(), causal_patterns.end());
        
        std::map<Handle, double> pattern_outcomes = analyzePatternOutcomes(all_patterns);
        learnFromPatternAnalysis(pattern_outcomes);
        
        logger().info() << "[ExperienceManager] Discovered " << patterns_discovered << " new patterns";
        
    } catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Error in pattern discovery: " << e.what();
    }
    
    return patterns_discovered;
}

std::vector<Handle> ExperienceManager::discoverSequentialPatterns(const std::vector<Experience>& experiences)
{
    std::vector<Handle> patterns;
    
    // Simple sequential pattern discovery
    std::map<std::string, std::vector<Handle>> action_sequences;
    
    for (const Experience& exp : experiences) {
        if (exp.actions.size() >= 2) {
            // Create sequence signature
            std::stringstream seq_sig;
            for (size_t i = 0; i < exp.actions.size(); ++i) {
                if (i > 0) seq_sig << "->";
                seq_sig << exp.actions[i]->get_name();
            }
            
            action_sequences[seq_sig.str()].push_back(exp.experience_atom);
        }
    }
    
    // Create patterns for sequences that appear multiple times
    for (const auto& seq_pair : action_sequences) {
        if (seq_pair.second.size() >= 2) { // Pattern must appear at least twice
            Handle pattern = _atomspace->add_node(CONCEPT_NODE, "SequentialPattern_" + seq_pair.first);
            
            // Link pattern to experiences
            for (const Handle& exp_atom : seq_pair.second) {
                _atomspace->add_link(MEMBER_LINK, {exp_atom, pattern});
            }
            
            patterns.push_back(pattern);
        }
    }
    
    return patterns;
}

std::vector<Handle> ExperienceManager::discoverCausalPatterns(const std::vector<Experience>& experiences)
{
    std::vector<Handle> patterns;
    
    // Simple causal pattern discovery based on action-consequence relationships
    std::map<std::string, std::vector<std::pair<Handle, ExperienceOutcome>>> causal_map;
    
    for (const Experience& exp : experiences) {
        if (!exp.actions.empty() && !exp.consequences.empty()) {
            // Create causal signature (action -> consequence)
            std::stringstream causal_sig;
            for (const Handle& action : exp.actions) {
                causal_sig << action->get_name() << ";";
            }
            causal_sig << "->";
            for (const Handle& consequence : exp.consequences) {
                causal_sig << consequence->get_name() << ";";
            }
            
            causal_map[causal_sig.str()].push_back({exp.experience_atom, exp.outcome});
        }
    }
    
    // Create patterns for causal relationships that show consistent outcomes
    for (const auto& causal_pair : causal_map) {
        if (causal_pair.second.size() >= 2) {
            // Calculate success rate
            int successes = 0;
            for (const auto& exp_outcome : causal_pair.second) {
                if (exp_outcome.second == ExperienceOutcome::SUCCESS) {
                    successes++;
                }
            }
            
            double success_rate = static_cast<double>(successes) / causal_pair.second.size();
            
            if (success_rate > 0.6 || success_rate < 0.4) { // Either high success or high failure
                Handle pattern = _atomspace->add_node(CONCEPT_NODE, "CausalPattern_" + 
                                                     std::to_string(patterns.size()));
                
                // Set success rate as truth value
                TruthValuePtr tv = SimpleTruthValue::createTV(success_rate, 0.8);
                pattern->setTruthValue(tv);
                
                patterns.push_back(pattern);
            }
        }
    }
    
    return patterns;
}

std::map<Handle, double> ExperienceManager::analyzePatternOutcomes(const std::vector<Handle>& patterns)
{
    std::map<Handle, double> outcomes;
    
    for (const Handle& pattern : patterns) {
        TruthValuePtr tv = pattern->getTruthValue();
        if (tv) {
            outcomes[pattern] = tv->get_mean();
        } else {
            outcomes[pattern] = 0.5; // Default neutral outcome
        }
    }
    
    return outcomes;
}

void ExperienceManager::learnFromPatternAnalysis(const std::map<Handle, double>& pattern_outcomes)
{
    // Store successful patterns for future use
    for (const auto& pattern_outcome : pattern_outcomes) {
        if (pattern_outcome.second > _pattern_significance_threshold) {
            // Link successful pattern to learning outcomes
            Handle learning_link = _atomspace->add_link(MEMBER_LINK, {
                pattern_outcome.first, _learning_outcomes
            });
            
            TruthValuePtr high_confidence = SimpleTruthValue::createTV(pattern_outcome.second, 0.9);
            learning_link->setTruthValue(high_confidence);
        }
    }
}

std::vector<Handle> ExperienceManager::getSuccessfulPatterns(const std::vector<Handle>& context_atoms,
                                                           double min_success_rate)
{
    std::vector<Handle> successful_patterns;
    
    for (const auto& pattern_lib : _pattern_library) {
        for (const Handle& pattern : pattern_lib.second) {
            auto success_it = _pattern_success_rates.find(pattern);
            if (success_it != _pattern_success_rates.end() && 
                success_it->second >= min_success_rate) {
                successful_patterns.push_back(pattern);
            }
        }
    }
    
    return successful_patterns;
}

void ExperienceManager::consolidateExperiences()
{
    logger().debug() << "[ExperienceManager] Consolidating experiences";
    
    try {
        // Remove redundant experiences
        pruneRedundantExperiences();
        
        // Update experience importance based on recent outcomes
        updateExperienceImportance();
        
        // Perform pattern discovery if enabled
        if (_enable_pattern_discovery) {
            discoverExperiencePatterns();
        }
        
        logger().info() << "[ExperienceManager] Experience consolidation completed";
        
    } catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Error during experience consolidation: " << e.what();
    }
}

void ExperienceManager::pruneRedundantExperiences()
{
    // Simple redundancy removal based on experience similarity
    std::vector<Handle> to_remove;
    
    for (auto it1 = _experience_registry.begin(); it1 != _experience_registry.end(); ++it1) {
        for (auto it2 = std::next(it1); it2 != _experience_registry.end(); ++it2) {
            double similarity = calculateExperienceSimilarity(it1->second, it2->second);
            
            if (similarity > 0.9) { // Very similar experiences
                // Keep the one with higher learning value
                if (it1->second.learning_value < it2->second.learning_value) {
                    to_remove.push_back(it1->first);
                } else {
                    to_remove.push_back(it2->first);
                }
            }
        }
    }
    
    // Remove redundant experiences
    for (const Handle& exp_atom : to_remove) {
        _experience_registry.erase(exp_atom);
        _significant_experiences.erase(exp_atom);
    }
    
    logger().debug() << "[ExperienceManager] Removed " << to_remove.size() << " redundant experiences";
}

double ExperienceManager::calculateExperienceSimilarity(const Experience& exp1, const Experience& exp2)
{
    double similarity = 0.0;
    
    // Compare types
    if (exp1.type == exp2.type) similarity += 0.2;
    
    // Compare outcomes
    if (exp1.outcome == exp2.outcome) similarity += 0.2;
    
    // Compare context similarity
    double context_sim = calculateContextSimilarity(exp1.context, exp2.context);
    similarity += context_sim * 0.3;
    
    // Compare actions similarity
    std::set<Handle> actions1(exp1.actions.begin(), exp1.actions.end());
    std::set<Handle> actions2(exp2.actions.begin(), exp2.actions.end());
    
    std::vector<Handle> action_intersection;
    std::set_intersection(actions1.begin(), actions1.end(),
                         actions2.begin(), actions2.end(),
                         std::back_inserter(action_intersection));
    
    if (!actions1.empty() && !actions2.empty()) {
        double action_sim = static_cast<double>(action_intersection.size()) / 
                           std::max(actions1.size(), actions2.size());
        similarity += action_sim * 0.3;
    }
    
    return similarity;
}

double ExperienceManager::calculateContextSimilarity(const ExperienceContext& ctx1, const ExperienceContext& ctx2)
{
    double similarity = 0.0;
    double weight_sum = 0.0;
    
    // Compare environmental state
    if (!ctx1.environmental_state.empty() && !ctx2.environmental_state.empty()) {
        std::set<Handle> env1(ctx1.environmental_state.begin(), ctx1.environmental_state.end());
        std::set<Handle> env2(ctx2.environmental_state.begin(), ctx2.environmental_state.end());
        
        std::vector<Handle> env_intersection;
        std::set_intersection(env1.begin(), env1.end(), env2.begin(), env2.end(),
                             std::back_inserter(env_intersection));
        
        double env_sim = static_cast<double>(env_intersection.size()) / 
                        std::max(env1.size(), env2.size());
        similarity += env_sim * 0.4;
        weight_sum += 0.4;
    }
    
    // Compare agent state
    if (!ctx1.agent_state.empty() && !ctx2.agent_state.empty()) {
        std::set<Handle> agent1(ctx1.agent_state.begin(), ctx1.agent_state.end());
        std::set<Handle> agent2(ctx2.agent_state.begin(), ctx2.agent_state.end());
        
        std::vector<Handle> agent_intersection;
        std::set_intersection(agent1.begin(), agent1.end(), agent2.begin(), agent2.end(),
                             std::back_inserter(agent_intersection));
        
        double agent_sim = static_cast<double>(agent_intersection.size()) / 
                          std::max(agent1.size(), agent2.size());
        similarity += agent_sim * 0.3;
        weight_sum += 0.3;
    }
    
    // Compare goals
    if (!ctx1.active_goals.empty() && !ctx2.active_goals.empty()) {
        std::set<Handle> goals1(ctx1.active_goals.begin(), ctx1.active_goals.end());
        std::set<Handle> goals2(ctx2.active_goals.begin(), ctx2.active_goals.end());
        
        std::vector<Handle> goal_intersection;
        std::set_intersection(goals1.begin(), goals1.end(), goals2.begin(), goals2.end(),
                             std::back_inserter(goal_intersection));
        
        double goal_sim = static_cast<double>(goal_intersection.size()) / 
                         std::max(goals1.size(), goals2.size());
        similarity += goal_sim * 0.3;
        weight_sum += 0.3;
    }
    
    return weight_sum > 0 ? similarity / weight_sum : 0.0;
}

void ExperienceManager::updateExperienceImportance()
{
    // Update importance based on recent pattern analysis
    for (auto& exp_pair : _experience_registry) {
        Experience& exp = exp_pair.second;
        
        // Increase importance if experience contributed to successful patterns
        if (_significant_experiences.count(exp.experience_atom) > 0) {
            if (exp.importance < ExperienceImportance::HIGH) {
                exp.importance = static_cast<ExperienceImportance>(
                    static_cast<int>(exp.importance) + 25);
            }
        }
    }
}

std::map<ExperienceManager::ExperienceType, size_t> ExperienceManager::getExperienceStatistics() const
{
    std::map<ExperienceType, size_t> stats;
    
    for (const auto& type_exp_pair : _experiences_by_type) {
        stats[type_exp_pair.first] = type_exp_pair.second.size();
    }
    
    return stats;
}

std::string ExperienceManager::getConfigurationStatus() const
{
    std::stringstream ss;
    ss << "ExperienceManager Configuration Status:\n";
    ss << "  Pattern Discovery: " << (_enable_pattern_discovery ? "Enabled" : "Disabled") << "\n";
    ss << "  MOSES Integration: " << (_enable_moses_integration ? "Enabled" : "Disabled") << "\n";
    ss << "  Temporal Modeling: " << (_enable_temporal_modeling ? "Enabled" : "Disabled") << "\n";
    ss << "  Max Recent Experiences: " << _max_recent_experiences << "\n";
    ss << "  Pattern Significance Threshold: " << _pattern_significance_threshold << "\n";
    ss << "  Experience Retention Threshold: " << _experience_retention_threshold << "\n";
    ss << "  Total Experiences: " << _experience_registry.size() << "\n";
    ss << "  Significant Experiences: " << _significant_experiences.size() << "\n";
    ss << "  Pattern Library Size: " << _pattern_library.size() << "\n";
    
    return ss.str();
}

bool ExperienceManager::processExperienceManagement()
{
    logger().debug() << "[ExperienceManager] Processing experience management cycle";
    
    try {
        // Periodic consolidation
        if (_experience_registry.size() > _max_recent_experiences * 2) {
            consolidateExperiences();
        }
        
        // Pattern discovery on accumulated experiences
        if (_enable_pattern_discovery && _experience_registry.size() >= 10) {
            discoverExperiencePatterns();
        }
        
        // MOSES integration if enabled
        if (_enable_moses_integration && _moses_available && !_recent_experiences.empty()) {
            integrateMOSESOptimization(_recent_experiences);
        }
        
        return true;
        
    } catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Error in experience management processing: " << e.what();
        return false;
    }
}

void ExperienceManager::integrateMOSESOptimization(const std::vector<Experience>& experiences)
{
    // Placeholder for MOSES integration - would require actual MOSES library integration
    logger().debug() << "[ExperienceManager] MOSES integration processing " << experiences.size() << " experiences";
    
    // This would involve:
    // 1. Converting experiences to fitness function
    // 2. Creating policy representations
    // 3. Running MOSES optimization
    // 4. Storing optimized policies
    
    // For now, just create placeholder policy atoms
    for (const Experience& exp : experiences) {
        if (exp.outcome == ExperienceOutcome::SUCCESS && exp.learning_value > 0.7) {
            std::string policy_name = "OptimizedPolicy_" + std::to_string(
                std::chrono::duration_cast<std::chrono::milliseconds>(
                    exp.context.timestamp.time_since_epoch()).count());
            
            Handle policy_atom = _atomspace->add_node(CONCEPT_NODE, policy_name);
            _experience_to_policy_map[exp.experience_atom] = policy_atom;
            
            // Link to MOSES policy space
            if (_moses_policy_space != Handle::UNDEFINED) {
                _atomspace->add_link(MEMBER_LINK, {policy_atom, _moses_policy_space});
            }
        }
    }
}

// Placeholder implementations for remaining public methods
std::vector<Handle> ExperienceManager::analyzeExperienceForLearning(const Handle& experience_atom)
{
    std::vector<Handle> insights;
    
    auto exp_it = _experience_registry.find(experience_atom);
    if (exp_it != _experience_registry.end()) {
        const Experience& exp = exp_it->second;
        
        // Generate basic learning insights
        if (exp.learning_value > 0.7) {
            Handle insight = _atomspace->add_node(CONCEPT_NODE, 
                "HighLearningValue_" + experience_atom->get_name());
            insights.push_back(insight);
        }
        
        if (exp.outcome == ExperienceOutcome::UNEXPECTED_OUTCOME) {
            Handle insight = _atomspace->add_node(CONCEPT_NODE,
                "UnexpectedOutcome_" + experience_atom->get_name());
            insights.push_back(insight);
        }
    }
    
    return insights;
}

std::vector<Handle> ExperienceManager::getSkillExperiences(const Handle& skill_atom, bool include_failures)
{
    std::vector<Handle> skill_experiences;
    
    auto skill_it = _skill_experience_map.find(skill_atom);
    if (skill_it != _skill_experience_map.end()) {
        for (const Handle& exp_atom : skill_it->second) {
            auto exp_it = _experience_registry.find(exp_atom);
            if (exp_it != _experience_registry.end()) {
                const Experience& exp = exp_it->second;
                
                if (include_failures || exp.outcome == ExperienceOutcome::SUCCESS) {
                    skill_experiences.push_back(exp_atom);
                }
            }
        }
    }
    
    return skill_experiences;
}

std::vector<Handle> ExperienceManager::getExperienceSequence(
    const std::chrono::system_clock::time_point& start_time,
    const std::chrono::system_clock::time_point& end_time)
{
    std::vector<Handle> sequence;
    
    for (const auto& time_exp_pair : _temporal_index) {
        if (time_exp_pair.first >= start_time && time_exp_pair.first <= end_time) {
            sequence.insert(sequence.end(), 
                           time_exp_pair.second.begin(), 
                           time_exp_pair.second.end());
        }
    }
    
    return sequence;
}

Handle ExperienceManager::optimizePolicyFromExperience(const Handle& policy_atom,
                                                      const std::vector<Handle>& related_experiences)
{
    // Placeholder for policy optimization using MOSES
    logger().debug() << "[ExperienceManager] Optimizing policy from " << related_experiences.size() << " experiences";
    
    // Create an optimized policy variant
    std::string optimized_name = policy_atom->get_name() + "_optimized";
    Handle optimized_policy = _atomspace->add_node(CONCEPT_NODE, optimized_name);
    
    // Link to original policy
    _atomspace->add_link(INHERITANCE_LINK, {optimized_policy, policy_atom});
    
    return optimized_policy;
}

std::vector<Handle> ExperienceManager::getRecentLearningInsights(int days)
{
    std::vector<Handle> insights;
    
    auto cutoff_time = std::chrono::system_clock::now() - std::chrono::hours(24 * days);
    
    for (const Experience& exp : _recent_experiences) {
        if (exp.context.timestamp >= cutoff_time && exp.learning_value > 0.6) {
            std::vector<Handle> exp_insights = analyzeExperienceForLearning(exp.experience_atom);
            insights.insert(insights.end(), exp_insights.begin(), exp_insights.end());
        }
    }
    
    return insights;
}

size_t ExperienceManager::pruneOldExperiences()
{
    auto cutoff_time = std::chrono::system_clock::now() - std::chrono::hours(24 * 30); // 30 days
    size_t pruned_count = 0;
    
    std::vector<Handle> to_remove;
    
    for (const auto& exp_pair : _experience_registry) {
        const Experience& exp = exp_pair.second;
        
        // Don't prune significant experiences
        if (_significant_experiences.count(exp.experience_atom) > 0) {
            continue;
        }
        
        // Don't prune high learning value experiences
        if (exp.learning_value > _experience_retention_threshold) {
            continue;
        }
        
        // Prune old, low-value experiences
        if (exp.context.timestamp < cutoff_time) {
            to_remove.push_back(exp.experience_atom);
        }
    }
    
    for (const Handle& exp_atom : to_remove) {
        _experience_registry.erase(exp_atom);
        pruned_count++;
    }
    
    return pruned_count;
}

std::string ExperienceManager::exportExperiences(ExperienceType experience_type) const
{
    std::stringstream json;
    json << "{\n  \"experiences\": [\n";
    
    bool first = true;
    
    if (_experiences_by_type.find(experience_type) != _experiences_by_type.end()) {
        const std::vector<Experience>& experiences = _experiences_by_type.at(experience_type);
        
        for (const Experience& exp : experiences) {
            if (!first) json << ",\n";
            first = false;
            
            auto timestamp = std::chrono::duration_cast<std::chrono::milliseconds>(
                exp.context.timestamp.time_since_epoch()).count();
            
            json << "    {\n";
            json << "      \"description\": \"" << exp.description << "\",\n";
            json << "      \"type\": " << static_cast<int>(exp.type) << ",\n";
            json << "      \"outcome\": " << static_cast<int>(exp.outcome) << ",\n";
            json << "      \"importance\": " << static_cast<int>(exp.importance) << ",\n";
            json << "      \"learning_value\": " << exp.learning_value << ",\n";
            json << "      \"timestamp\": " << timestamp << ",\n";
            json << "      \"atom_handle\": \"" << exp.experience_atom->to_string() << "\"\n";
            json << "    }";
        }
    }
    
    json << "\n  ],\n";
    json << "  \"total_count\": " << ((_experiences_by_type.find(experience_type) != _experiences_by_type.end()) ? 
                                     _experiences_by_type.at(experience_type).size() : 0) << "\n";
    json << "}";
    
    return json.str();
}