/*
 * src/SkillAcquisition.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * SkillAcquisition Implementation
 * Core learning component of Agent-Zero Learning & Adaptation module
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
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/StringValue.h>

#include "opencog/agentzero/SkillAcquisition.h"
#include "opencog/agentzero/ExperienceManager.h"
#include "opencog/agentzero/PolicyOptimizer.h"
#include "opencog/agentzero/MetaLearning.h"

using namespace opencog;
using namespace opencog::agentzero;

SkillAcquisition::SkillAcquisition(AtomSpacePtr atomspace)
    : _atomspace(atomspace)
    , _skill_base(Handle::UNDEFINED)
    , _skill_hierarchy(Handle::UNDEFINED)
    , _enable_meta_learning(true)
    , _enable_skill_transfer(true)
    , _enable_incremental_learning(true)
    , _learning_rate(0.1)
    , _max_skill_complexity(10)
{
    if (!_atomspace) {
        throw std::runtime_error("SkillAcquisition requires valid AtomSpace");
    }

    logger().info() << "[SkillAcquisition] Initializing skill acquisition framework";
    
    initializeSkillBase();
    initializeComponents();
    
    logger().info() << "[SkillAcquisition] Skill acquisition framework initialized successfully";
}

SkillAcquisition::~SkillAcquisition()
{
    logger().info() << "[SkillAcquisition] Shutting down skill acquisition framework";
}

Handle SkillAcquisition::learnSkill(const std::string& skill_name,
                                   SkillType skill_type,
                                   LearningStrategy strategy,
                                   const std::vector<Handle>& experience_data)
{
    logger().info() << "[SkillAcquisition] Learning new skill: " << skill_name;

    // Check if skill already exists
    if (hasSkill(skill_name)) {
        Handle existing_skill = getSkill(skill_name);
        logger().warn() << "[SkillAcquisition] Skill '" << skill_name << "' already exists, refining instead";
        practiceSkill(existing_skill, experience_data);
        return existing_skill;
    }

    // Create new skill atom
    Handle skill_atom = createSkillAtom(skill_name, skill_type);
    
    // Register skill
    _skill_registry[skill_name] = skill_atom;
    _skill_types[skill_atom] = skill_type;
    _skill_proficiency[skill_atom] = ProficiencyLevel::NOVICE;

    // Extract skill components from experience data
    std::vector<Handle> skill_components = extractSkillComponents(experience_data);

    // Create skill structure based on learning strategy
    HandleSeq skill_structure;
    switch (strategy) {
        case LearningStrategy::IMITATION:
            // Learn by copying observed patterns
            skill_structure = skill_components;
            break;
            
        case LearningStrategy::REINFORCEMENT:
            // Use policy optimizer for reinforcement learning
            if (_policy_optimizer) {
                skill_structure = _policy_optimizer->optimizeSkillStructure(skill_components, experience_data);
            } else {
                skill_structure = skill_components;
            }
            break;
            
        case LearningStrategy::EXPLORATORY:
            // Systematically explore variations
            skill_structure = skill_components; // Simplified for now
            break;
            
        case LearningStrategy::TRANSFER:
            // Transfer from similar existing skills
            if (_enable_skill_transfer) {
                // Find similar skills and adapt
                skill_structure = skill_components; // Simplified for now
            } else {
                skill_structure = skill_components;
            }
            break;
            
        default:
            skill_structure = skill_components;
            break;
    }

    // Create skill definition link
    Handle skill_definition = _atomspace->add_link(LIST_LINK, skill_structure);
    
    // Associate skill with its definition
    _atomspace->add_link(INHERITANCE_LINK, {skill_atom, skill_definition});

    // Set initial truth value based on experience quality
    double initial_confidence = std::min(0.8, experience_data.size() * 0.1);
    skill_atom->setTruthValue(SimpleTruthValue::createTV(0.5, initial_confidence));

    // Initialize performance tracking
    _skill_performance_history[skill_atom] = {0.5}; // Start with neutral performance
    _skill_practice_counts[skill_atom] = 1;
    _skill_confidence_scores[skill_atom] = initial_confidence;

    // Use meta-learning to optimize learning approach
    if (_enable_meta_learning && _meta_learning) {
        _meta_learning->adaptLearningStrategy(skill_atom, strategy, experience_data);
    }

    logger().info() << "[SkillAcquisition] Successfully learned skill '" << skill_name 
                   << "' with " << skill_components.size() << " components";

    return skill_atom;
}

SkillAcquisition::ProficiencyLevel SkillAcquisition::practiceSkill(Handle skill_handle,
                                                                  const std::vector<Handle>& practice_data)
{
    if (skill_handle == Handle::UNDEFINED) {
        logger().error() << "[SkillAcquisition] Invalid skill handle for practice";
        return ProficiencyLevel::NOVICE;
    }

    logger().debug() << "[SkillAcquisition] Practicing skill with " << practice_data.size() << " practice examples";

    // Increment practice count
    _skill_practice_counts[skill_handle]++;

    // Calculate performance improvement based on practice data
    double performance_improvement = 0.0;
    if (!practice_data.empty()) {
        // Simplified performance calculation - in real implementation,
        // this would involve more sophisticated skill assessment
        performance_improvement = std::min(0.1, practice_data.size() * _learning_rate * 0.01);
    }

    // Update skill performance
    double current_performance = 0.5; // Default
    if (!_skill_performance_history[skill_handle].empty()) {
        current_performance = _skill_performance_history[skill_handle].back();
    }
    
    double new_performance = std::min(1.0, current_performance + performance_improvement);
    recordSkillPerformance(skill_handle, new_performance);

    // Update proficiency level
    updateSkillProficiency(skill_handle, new_performance);

    // Update truth value
    double confidence = std::min(0.9, _skill_confidence_scores[skill_handle] + 0.05);
    skill_handle->setTruthValue(SimpleTruthValue::createTV(new_performance, confidence));
    _skill_confidence_scores[skill_handle] = confidence;

    // Use incremental learning if enabled
    if (_enable_incremental_learning) {
        // Refine skill structure based on new practice data
        // This is a simplified implementation
        std::vector<Handle> refined_components = extractSkillComponents(practice_data);
        if (!refined_components.empty()) {
            // Add new components to skill definition if they improve performance
            // Implementation would be more sophisticated in practice
        }
    }

    ProficiencyLevel new_proficiency = _skill_proficiency[skill_handle];
    logger().debug() << "[SkillAcquisition] Skill practice complete, new performance: " 
                    << new_performance << ", proficiency: " << static_cast<int>(new_proficiency);

    return new_proficiency;
}

std::pair<bool, std::vector<Handle>> SkillAcquisition::applySkill(Handle skill_handle,
                                                                 const std::vector<Handle>& task_context,
                                                                 const std::map<std::string, ValuePtr>& parameters)
{
    if (skill_handle == Handle::UNDEFINED) {
        logger().error() << "[SkillAcquisition] Cannot apply undefined skill";
        return {false, {}};
    }

    logger().debug() << "[SkillAcquisition] Applying skill to task with " << task_context.size() << " context atoms";

    // Validate parameters
    if (!validateSkillParameters(parameters)) {
        logger().error() << "[SkillAcquisition] Invalid skill parameters";
        return {false, {}};
    }

    // Get skill proficiency to determine execution quality
    ProficiencyLevel proficiency = getSkillProficiency(skill_handle);
    double proficiency_factor = static_cast<double>(proficiency) / 100.0;

    // Retrieve skill definition
    HandleSeq skill_definition;
    IncomingSet skill_links = skill_handle->getIncomingSetByType(INHERITANCE_LINK);
    for (Handle link : skill_links) {
        HandleSeq outgoing = link->getOutgoingSet();
        if (outgoing.size() == 2 && outgoing[0] == skill_handle) {
            Handle definition_link = outgoing[1];
            if (definition_link->get_type() == LIST_LINK) {
                skill_definition = definition_link->getOutgoingSet();
                break;
            }
        }
    }

    if (skill_definition.empty()) {
        logger().error() << "[SkillAcquisition] No skill definition found";
        return {false, {}};
    }

    // Execute skill components
    std::vector<Handle> execution_results;
    bool execution_success = true;

    for (Handle component : skill_definition) {
        // Simplified skill execution - in practice, this would involve
        // more sophisticated action execution and monitoring
        Handle result = _atomspace->add_node(CONCEPT_NODE, 
            "SkillResult_" + std::to_string(component.value()));
        
        // Set result truth value based on proficiency
        double result_confidence = proficiency_factor * 0.8 + 0.2;
        result->setTruthValue(SimpleTruthValue::createTV(proficiency_factor, result_confidence));
        
        execution_results.push_back(result);
    }

    // Record skill application for learning
    double application_performance = proficiency_factor;
    recordSkillPerformance(skill_handle, application_performance);

    logger().debug() << "[SkillAcquisition] Skill application complete, success: " << execution_success
                    << ", results: " << execution_results.size();

    return {execution_success, execution_results};
}

Handle SkillAcquisition::transferSkill(Handle source_skill,
                                      const std::string& target_skill_name,
                                      const std::vector<Handle>& adaptation_rules)
{
    if (!_enable_skill_transfer) {
        logger().warn() << "[SkillAcquisition] Skill transfer is disabled";
        return Handle::UNDEFINED;
    }

    if (source_skill == Handle::UNDEFINED) {
        logger().error() << "[SkillAcquisition] Cannot transfer from undefined source skill";
        return Handle::UNDEFINED;
    }

    logger().info() << "[SkillAcquisition] Transferring skill to create: " << target_skill_name;

    // Get source skill type and components
    SkillType source_type = _skill_types[source_skill];
    
    // Create target skill atom
    Handle target_skill = createSkillAtom(target_skill_name, source_type);
    
    // Register target skill
    _skill_registry[target_skill_name] = target_skill;
    _skill_types[target_skill] = source_type;
    _skill_proficiency[target_skill] = ProficiencyLevel::BEGINNER; // Start with beginner level

    // Get source skill definition
    HandleSeq source_definition;
    IncomingSet skill_links = source_skill->getIncomingSetByType(INHERITANCE_LINK);
    for (Handle link : skill_links) {
        HandleSeq outgoing = link->getOutgoingSet();
        if (outgoing.size() == 2 && outgoing[0] == source_skill) {
            Handle definition_link = outgoing[1];
            if (definition_link->get_type() == LIST_LINK) {
                source_definition = definition_link->getOutgoingSet();
                break;
            }
        }
    }

    // Apply adaptation rules to create target skill definition
    HandleSeq target_definition = source_definition; // Start with copy
    
    // Apply adaptation rules (simplified implementation)
    for (Handle rule : adaptation_rules) {
        // In practice, this would involve sophisticated rule application
        // For now, we just add the rule to the definition
        target_definition.push_back(rule);
    }

    // Create target skill definition
    Handle target_def_link = _atomspace->add_link(LIST_LINK, target_definition);
    _atomspace->add_link(INHERITANCE_LINK, {target_skill, target_def_link});

    // Set initial truth value based on source skill performance
    double source_performance = 0.5;
    if (!_skill_performance_history[source_skill].empty()) {
        source_performance = _skill_performance_history[source_skill].back();
    }
    
    // Transfer typically starts with lower performance due to adaptation needs
    double transfer_performance = source_performance * 0.7;
    double transfer_confidence = 0.6;
    
    target_skill->setTruthValue(SimpleTruthValue::createTV(transfer_performance, transfer_confidence));

    // Initialize performance tracking
    _skill_performance_history[target_skill] = {transfer_performance};
    _skill_practice_counts[target_skill] = 1;
    _skill_confidence_scores[target_skill] = transfer_confidence;

    logger().info() << "[SkillAcquisition] Skill transfer complete: " << target_skill_name
                   << " (performance: " << transfer_performance << ")";

    return target_skill;
}

std::vector<Handle> SkillAcquisition::getLearnedSkills() const
{
    std::vector<Handle> skills;
    skills.reserve(_skill_registry.size());
    
    for (const auto& pair : _skill_registry) {
        skills.push_back(pair.second);
    }
    
    return skills;
}

std::vector<Handle> SkillAcquisition::getSkillsByType(SkillType skill_type) const
{
    std::vector<Handle> matching_skills;
    
    for (const auto& pair : _skill_types) {
        if (pair.second == skill_type) {
            matching_skills.push_back(pair.first);
        }
    }
    
    return matching_skills;
}

SkillAcquisition::ProficiencyLevel SkillAcquisition::getSkillProficiency(Handle skill_handle) const
{
    auto it = _skill_proficiency.find(skill_handle);
    if (it != _skill_proficiency.end()) {
        return it->second;
    }
    return ProficiencyLevel::NOVICE;
}

std::vector<double> SkillAcquisition::getSkillPerformanceHistory(Handle skill_handle) const
{
    auto it = _skill_performance_history.find(skill_handle);
    if (it != _skill_performance_history.end()) {
        return it->second;
    }
    return {};
}

bool SkillAcquisition::hasSkill(const std::string& skill_name) const
{
    return _skill_registry.find(skill_name) != _skill_registry.end();
}

Handle SkillAcquisition::getSkill(const std::string& skill_name) const
{
    auto it = _skill_registry.find(skill_name);
    if (it != _skill_registry.end()) {
        return it->second;
    }
    return Handle::UNDEFINED;
}

void SkillAcquisition::setLearningRate(double rate)
{
    _learning_rate = std::max(0.0, std::min(1.0, rate));
    logger().debug() << "[SkillAcquisition] Learning rate set to: " << _learning_rate;
}

void SkillAcquisition::setMetaLearningEnabled(bool enable)
{
    _enable_meta_learning = enable;
    logger().info() << "[SkillAcquisition] Meta-learning " << (enable ? "enabled" : "disabled");
}

void SkillAcquisition::setSkillTransferEnabled(bool enable)
{
    _enable_skill_transfer = enable;
    logger().info() << "[SkillAcquisition] Skill transfer " << (enable ? "enabled" : "disabled");
}

void SkillAcquisition::setMaxSkillComplexity(size_t complexity)
{
    _max_skill_complexity = complexity;
    logger().debug() << "[SkillAcquisition] Maximum skill complexity set to: " << complexity;
}

bool SkillAcquisition::optimizeLearningParameters()
{
    if (!_meta_learning) {
        logger().warn() << "[SkillAcquisition] Meta-learning not available for parameter optimization";
        return false;
    }

    logger().info() << "[SkillAcquisition] Optimizing learning parameters using meta-learning";
    
    // Collect performance data from all skills
    std::vector<double> all_performances;
    for (const auto& history_pair : _skill_performance_history) {
        const std::vector<double>& history = history_pair.second;
        if (!history.empty()) {
            all_performances.push_back(history.back());
        }
    }

    if (all_performances.empty()) {
        logger().warn() << "[SkillAcquisition] No performance data available for optimization";
        return false;
    }

    // Calculate average performance
    double avg_performance = 0.0;
    for (double perf : all_performances) {
        avg_performance += perf;
    }
    avg_performance /= all_performances.size();

    // Adjust learning rate based on performance
    if (avg_performance < 0.5) {
        // Poor performance - increase learning rate
        _learning_rate = std::min(1.0, _learning_rate * 1.1);
    } else if (avg_performance > 0.8) {
        // Good performance - decrease learning rate for fine-tuning
        _learning_rate = std::max(0.01, _learning_rate * 0.9);
    }

    logger().info() << "[SkillAcquisition] Parameter optimization complete, new learning rate: " << _learning_rate;
    return true;
}

std::map<std::string, double> SkillAcquisition::getLearningStatistics() const
{
    std::map<std::string, double> stats;
    
    stats["total_skills"] = static_cast<double>(_skill_registry.size());
    stats["learning_rate"] = _learning_rate;
    stats["max_skill_complexity"] = static_cast<double>(_max_skill_complexity);
    
    // Calculate average proficiency
    double total_proficiency = 0.0;
    for (const auto& pair : _skill_proficiency) {
        total_proficiency += static_cast<double>(pair.second);
    }
    stats["average_proficiency"] = _skill_proficiency.empty() ? 0.0 : total_proficiency / _skill_proficiency.size();
    
    // Calculate total practice sessions
    size_t total_practice = 0;
    for (const auto& pair : _skill_practice_counts) {
        total_practice += pair.second;
    }
    stats["total_practice_sessions"] = static_cast<double>(total_practice);
    
    // Feature availability
    stats["meta_learning_enabled"] = _enable_meta_learning ? 1.0 : 0.0;
    stats["skill_transfer_enabled"] = _enable_skill_transfer ? 1.0 : 0.0;
    stats["incremental_learning_enabled"] = _enable_incremental_learning ? 1.0 : 0.0;
    
    return stats;
}

std::string SkillAcquisition::getStatusInfo() const
{
    std::ostringstream oss;
    oss << "SkillAcquisition Status:\n";
    oss << "  Total Skills: " << _skill_registry.size() << "\n";
    oss << "  Learning Rate: " << _learning_rate << "\n";
    oss << "  Meta-Learning: " << (_enable_meta_learning ? "Enabled" : "Disabled") << "\n";
    oss << "  Skill Transfer: " << (_enable_skill_transfer ? "Enabled" : "Disabled") << "\n";
    oss << "  Incremental Learning: " << (_enable_incremental_learning ? "Enabled" : "Disabled") << "\n";
    
    // Skill type breakdown
    std::map<SkillType, int> type_counts;
    for (const auto& pair : _skill_types) {
        type_counts[pair.second]++;
    }
    
    oss << "  Skills by Type:\n";
    for (const auto& pair : type_counts) {
        oss << "    " << static_cast<int>(pair.first) << ": " << pair.second << "\n";
    }
    
    return oss.str();
}

void SkillAcquisition::reset()
{
    logger().info() << "[SkillAcquisition] Resetting skill acquisition system";
    
    _skill_registry.clear();
    _skill_types.clear();
    _skill_proficiency.clear();
    _skill_performance_history.clear();
    _skill_practice_counts.clear();
    _skill_confidence_scores.clear();
    
    initializeSkillBase();
    
    logger().info() << "[SkillAcquisition] Skill acquisition system reset complete";
}

// Private helper methods

void SkillAcquisition::initializeSkillBase()
{
    // Create base skill hierarchy in AtomSpace
    _skill_base = _atomspace->add_node(CONCEPT_NODE, "SkillBase");
    _skill_hierarchy = _atomspace->add_node(CONCEPT_NODE, "SkillHierarchy");
    
    // Create relationship between base and hierarchy
    _atomspace->add_link(INHERITANCE_LINK, {_skill_hierarchy, _skill_base});
    
    logger().debug() << "[SkillAcquisition] Skill base initialized in AtomSpace";
}

void SkillAcquisition::initializeComponents()
{
    // Initialize sub-components
    _experience_manager = std::make_unique<ExperienceManager>(_atomspace);
    _policy_optimizer = std::make_unique<PolicyOptimizer>(_atomspace);
    _meta_learning = std::make_unique<MetaLearning>(_atomspace);
    
    logger().debug() << "[SkillAcquisition] Components initialized successfully";
}

Handle SkillAcquisition::createSkillAtom(const std::string& name, SkillType type)
{
    std::string skill_name = "Skill_" + name;
    Handle skill_atom = _atomspace->add_node(CONCEPT_NODE, skill_name);
    
    // Link to skill hierarchy
    _atomspace->add_link(INHERITANCE_LINK, {skill_atom, _skill_hierarchy});
    
    // Add type information as a value
    Handle type_node = _atomspace->add_node(CONCEPT_NODE, "SkillType_" + std::to_string(static_cast<int>(type)));
    _atomspace->add_link(EVALUATION_LINK, {
        _atomspace->add_node(PREDICATE_NODE, "hasSkillType"),
        _atomspace->add_link(LIST_LINK, {skill_atom, type_node})
    });
    
    return skill_atom;
}

void SkillAcquisition::updateSkillProficiency(Handle skill_handle, double performance_score)
{
    // Update proficiency based on performance score and practice count
    size_t practice_count = _skill_practice_counts[skill_handle];
    
    // Calculate proficiency based on performance and experience
    int proficiency_value = static_cast<int>(performance_score * 100);
    
    // Bonus for extensive practice
    if (practice_count > 10) {
        proficiency_value += std::min(25, static_cast<int>(practice_count - 10));
    }
    
    proficiency_value = std::min(100, proficiency_value);
    
    ProficiencyLevel new_level;
    if (proficiency_value >= 100) new_level = ProficiencyLevel::EXPERT;
    else if (proficiency_value >= 75) new_level = ProficiencyLevel::ADVANCED;
    else if (proficiency_value >= 50) new_level = ProficiencyLevel::INTERMEDIATE;
    else if (proficiency_value >= 25) new_level = ProficiencyLevel::BEGINNER;
    else new_level = ProficiencyLevel::NOVICE;
    
    _skill_proficiency[skill_handle] = new_level;
}

void SkillAcquisition::recordSkillPerformance(Handle skill_handle, double score)
{
    _skill_performance_history[skill_handle].push_back(score);
    
    // Keep history manageable
    if (_skill_performance_history[skill_handle].size() > 100) {
        _skill_performance_history[skill_handle].erase(
            _skill_performance_history[skill_handle].begin());
    }
}

bool SkillAcquisition::validateSkillParameters(const std::map<std::string, ValuePtr>& parameters)
{
    // Basic parameter validation
    for (const auto& param : parameters) {
        if (param.first.empty() || !param.second) {
            return false;
        }
    }
    return true;
}

std::vector<Handle> SkillAcquisition::extractSkillComponents(const std::vector<Handle>& experience_data)
{
    std::vector<Handle> components;
    
    // Extract meaningful components from experience data
    // This is a simplified implementation - in practice, this would involve
    // sophisticated pattern recognition and component extraction
    for (Handle experience : experience_data) {
        if (experience != Handle::UNDEFINED) {
            components.push_back(experience);
        }
    }
    
    // Limit complexity
    if (components.size() > _max_skill_complexity) {
        components.resize(_max_skill_complexity);
    }
    
    return components;
}

double SkillAcquisition::calculateSkillComplexity(Handle skill_handle)
{
    // Get skill definition and calculate complexity
    IncomingSet skill_links = skill_handle->getIncomingSetByType(INHERITANCE_LINK);
    for (Handle link : skill_links) {
        HandleSeq outgoing = link->getOutgoingSet();
        if (outgoing.size() == 2 && outgoing[0] == skill_handle) {
            Handle definition_link = outgoing[1];
            if (definition_link->get_type() == LIST_LINK) {
                return static_cast<double>(definition_link->getOutgoingSet().size());
            }
        }
    }
    return 1.0; // Default complexity
}

double SkillAcquisition::calculateTransferSimilarity(Handle source_skill, Handle target_context)
{
    // Simplified similarity calculation
    // In practice, this would involve sophisticated similarity metrics
    // between skill structures and target contexts
    return 0.5; // Default similarity
}