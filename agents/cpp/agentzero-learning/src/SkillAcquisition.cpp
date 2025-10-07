/*
 * src/SkillAcquisition.cpp
 * opencog/agentzero/SkillAcquisition.cpp
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
 * SkillAcquisition - Learns new capabilities through experience
 * Part of AZ-LEARN-004: Implement MetaLearning capabilities
 */

#include "opencog/agentzero/SkillAcquisition.h"
#include "opencog/agentzero/ExperienceManager.h"

#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/util/random.h>

#include <algorithm>
#include <numeric>
/**
 * SkillAcquisition.cpp
 *
 * Learns new capabilities through experience
 * Part of Agent-Zero Learning & Adaptation Phase 5
 *
 * Copyright (C) 2024 OpenCog Foundation
 */

#include "agentzero-learning/SkillAcquisition.h"
#include <opencog/util/Logger.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/atom_types/atom_types.h>

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
// Constructor
SkillAcquisition::SkillAcquisition(AtomSpacePtr atomspace, const SkillAcquisitionConfig& config)
    : _atomspace(atomspace)
    , _config(config)
    , _current_learning_context(Handle::UNDEFINED)
    , _skill_context(Handle::UNDEFINED)
    , _learning_link(Handle::UNDEFINED)
{
    logger().info() << "[SkillAcquisition] Initializing skill acquisition system";
}

// Destructor
SkillAcquisition::~SkillAcquisition()
{
    logger().info() << "[SkillAcquisition] Shutting down with " << _skills.size() << " skills learned";
}

// Initialize skill acquisition system
void SkillAcquisition::initialize()
{
    logger().info() << "[SkillAcquisition] Initializing skill acquisition components";
    
    // Create skill context in AtomSpace
    _skill_context = _atomspace->add_node(CONCEPT_NODE, "SkillContext");
    
    // Create learning link
    _learning_link = _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "SkillLearning"),
        _skill_context);
    
    logger().info() << "[SkillAcquisition] Skill acquisition system initialized";
}

// Learn a new skill from demonstration
Handle SkillAcquisition::learnSkillFromDemonstration(const Handle& demonstration, const Handle& context,
                                                    const std::string& skill_name)
{
    if (demonstration == Handle::UNDEFINED) {
        logger().warn() << "[SkillAcquisition] Cannot learn from undefined demonstration";
        return Handle::UNDEFINED;
    }
    
    // Create new skill
    Skill skill;
    skill.id = _atomspace->add_node(CONCEPT_NODE, 
        "Skill_" + (skill_name.empty() ? std::to_string(rand()) : skill_name));
    skill.name = skill_name.empty() ? "LearnedSkill_" + std::to_string(rand()) : skill_name;
    skill.description = "Skill learned from demonstration";
    skill.preconditions = context;
    skill.actions = demonstration;
    skill.proficiency = 0.3; // Initial proficiency from demonstration
    skill.confidence = 0.5;
    skill.practice_count = 1;
    skill.last_used = std::chrono::system_clock::now();
    if (context != Handle::UNDEFINED) {
        skill.contexts.push_back(context);
    }
    
    // Add to storage
    size_t index = _skills.size();
    _skills.push_back(skill);
    indexSkill(skill, index);
    
    // Create AtomSpace representation
    Handle skill_atom = createSkillAtom(skill);
    
    logger().info() << "[SkillAcquisition] Learned skill '" << skill.name 
                    << "' from demonstration with proficiency " << skill.proficiency;
    
    return skill.id;
}

// Learn a skill through exploration and practice
Handle SkillAcquisition::learnSkillThroughPractice(const Handle& task, const Handle& context,
                                                  const std::string& skill_name)
{
    if (task == Handle::UNDEFINED) {
        logger().warn() << "[SkillAcquisition] Cannot learn undefined task";
        return Handle::UNDEFINED;
    }
    
    // Create skill through practice
    Skill skill;
    skill.id = _atomspace->add_node(CONCEPT_NODE, 
        "Skill_" + (skill_name.empty() ? std::to_string(rand()) : skill_name));
    skill.name = skill_name.empty() ? "PracticedSkill_" + std::to_string(rand()) : skill_name;
    skill.description = "Skill learned through practice";
    skill.preconditions = identifySkillPreconditions(task, context);
    skill.actions = decomposeTaskIntoActions(task);
    skill.postconditions = identifySkillPostconditions(task, context);
    skill.proficiency = 0.1; // Start with low proficiency
    skill.confidence = 0.2;
    skill.practice_count = 0;
    skill.last_used = std::chrono::system_clock::now();
    if (context != Handle::UNDEFINED) {
        skill.contexts.push_back(context);
    }
    
    // Practice the skill to improve proficiency
    for (int i = 0; i < _config.max_practice_attempts && skill.proficiency < _config.min_proficiency_threshold; ++i) {
        refineSkillThroughPractice(_skills.size()); // Practice before adding to storage
        skill.practice_count++;
        skill.proficiency = std::min(1.0, skill.proficiency + _config.learning_rate);
        skill.confidence = std::min(1.0, skill.confidence + _config.learning_rate * 0.5);
    }
    
    // Add to storage
    size_t index = _skills.size();
    _skills.push_back(skill);
    indexSkill(skill, index);
    
    // Create AtomSpace representation
    Handle skill_atom = createSkillAtom(skill);
    
    logger().info() << "[SkillAcquisition] Learned skill '" << skill.name 
                    << "' through practice with proficiency " << skill.proficiency;
    
    return skill.id;
}

// Execute a skill in a given context
Handle SkillAcquisition::executeSkill(const Handle& skill_handle, const Handle& context,
                                     const Handle& parameters)
{
    auto it = _skill_index.find(skill_handle);
    if (it == _skill_index.end() || it->second >= _skills.size()) {
        logger().warn() << "[SkillAcquisition] Cannot execute unknown skill";
        return Handle::UNDEFINED;
    }
    
    Skill& skill = _skills[it->second];
    
    // Check if skill is ready for execution
    if (skill.confidence < _config.confidence_threshold) {
        logger().warn() << "[SkillAcquisition] Skill confidence too low for execution: " 
                        << skill.confidence;
        return Handle::UNDEFINED;
    }
    
    // Update skill usage
    skill.last_used = std::chrono::system_clock::now();
    skill.practice_count++;
    
    // Simulate skill execution (would integrate with actual execution system)
    bool execution_success = (randGen().randdouble() < skill.proficiency);
    double performance = execution_success ? 0.8 : 0.3;
    
    // Update proficiency based on execution
    updateSkillProficiency(it->second, execution_success, performance);
    
    // Create execution result
    Handle result = _atomspace->add_node(CONCEPT_NODE, 
        "SkillExecution_" + std::to_string(rand()));
    
    // Link result to skill and context
    _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "ExecutionResult"),
        _atomspace->add_link(LIST_LINK, skill_handle, result));
    
    logger().debug() << "[SkillAcquisition] Executed skill '" << skill.name 
                     << "' with success: " << execution_success;
    
    return result;
}

// Practice an existing skill
bool SkillAcquisition::practiceSkill(const Handle& skill_handle, const Handle& context)
{
    auto it = _skill_index.find(skill_handle);
    if (it == _skill_index.end() || it->second >= _skills.size()) {
        return false;
    }
    
    refineSkillThroughPractice(it->second);
    _skills[it->second].practice_count++;
    _skills[it->second].last_used = std::chrono::system_clock::now();
    
    return true;
}

// Get skills applicable to a context
std::vector<Skill> SkillAcquisition::getApplicableSkills(const Handle& context, double min_proficiency) const
{
    std::vector<Skill> applicable;
    
    for (const auto& skill : _skills) {
        if (skill.proficiency >= min_proficiency) {
            if (context == Handle::UNDEFINED || 
                std::find(skill.contexts.begin(), skill.contexts.end(), context) != skill.contexts.end()) {
                applicable.push_back(skill);
            }
        }
    }
    
    // Sort by proficiency (descending)
    std::sort(applicable.begin(), applicable.end(),
              [](const Skill& a, const Skill& b) {
                  return a.proficiency > b.proficiency;
              });
    
    return applicable;
}

// Get skill by handle
Skill SkillAcquisition::getSkill(const Handle& skill_handle) const
{
    auto it = _skill_index.find(skill_handle);
    if (it != _skill_index.end() && it->second < _skills.size()) {
        return _skills[it->second];
    }
    return Skill(); // Return empty skill if not found
}

// Update skill proficiency based on execution results
bool SkillAcquisition::updateSkillProficiency(const Handle& skill_handle, bool success, double performance)
{
    auto it = _skill_index.find(skill_handle);
    if (it != _skill_index.end() && it->second < _skills.size()) {
        updateSkillProficiency(it->second, success, performance);
        return true;
    }
    return false;
}

// Get skill acquisition statistics
std::map<std::string, double> SkillAcquisition::getSkillStatistics() const
{
    std::map<std::string, double> stats;
    
    stats["total_skills"] = static_cast<double>(_skills.size());
    
    if (_skills.empty()) {
        return stats;
    }
    
    double total_proficiency = 0.0;
    double total_confidence = 0.0;
    int proficient_skills = 0;
    
    for (const auto& skill : _skills) {
        total_proficiency += skill.proficiency;
        total_confidence += skill.confidence;
        if (skill.proficiency >= _config.min_proficiency_threshold) {
            proficient_skills++;
        }
    }
    
    stats["average_proficiency"] = total_proficiency / _skills.size();
    stats["average_confidence"] = total_confidence / _skills.size();
    stats["proficient_skills"] = static_cast<double>(proficient_skills);
    stats["proficiency_rate"] = (static_cast<double>(proficient_skills) / _skills.size()) * 100.0;
    
    return stats;
}

// Configuration and control
void SkillAcquisition::configure(const SkillAcquisitionConfig& config)
{
    _config = config;
    logger().info() << "[SkillAcquisition] Configuration updated";
}

void SkillAcquisition::setExperienceManager(std::shared_ptr<ExperienceManager> experience_manager)
{
    _experience_manager = experience_manager;
    logger().debug() << "[SkillAcquisition] Experience manager reference set";
}

void SkillAcquisition::reset()
{
    _skills.clear();
    _skill_index.clear();
    _name_index.clear();
    _context_index.clear();
    _opportunities.clear();
    _current_learning_context = Handle::UNDEFINED;
    
    logger().info() << "[SkillAcquisition] Skill acquisition system reset";
}

bool SkillAcquisition::isInitialized() const
{
    return _skill_context != Handle::UNDEFINED && _atomspace != nullptr;
}

// Private implementation methods
Handle SkillAcquisition::createSkillAtom(const Skill& skill)
{
    Handle skill_atom = skill.id;
    
    // Add skill properties
    if (skill.preconditions != Handle::UNDEFINED) {
        _atomspace->add_link(EVALUATION_LINK,
            _atomspace->add_node(PREDICATE_NODE, "SkillPreconditions"),
            _atomspace->add_link(LIST_LINK, skill_atom, skill.preconditions));
    }
    
    if (skill.actions != Handle::UNDEFINED) {
        _atomspace->add_link(EVALUATION_LINK,
            _atomspace->add_node(PREDICATE_NODE, "SkillActions"),
            _atomspace->add_link(LIST_LINK, skill_atom, skill.actions));
    }
    
    // Add proficiency
    Handle proficiency_atom = _atomspace->add_node(NUMBER_NODE, std::to_string(skill.proficiency));
    _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "SkillProficiency"),
        _atomspace->add_link(LIST_LINK, skill_atom, proficiency_atom));
    
    return skill_atom;
}

void SkillAcquisition::updateSkillProficiency(size_t skill_index, bool success, double performance)
{
    if (skill_index >= _skills.size()) return;
    
    Skill& skill = _skills[skill_index];
    
    // Update proficiency based on success and performance
    double adjustment = success ? _config.learning_rate : -_config.learning_rate * 0.5;
    adjustment *= performance;
    
    skill.proficiency = std::max(0.0, std::min(1.0, skill.proficiency + adjustment));
    skill.confidence = std::max(0.0, std::min(1.0, skill.confidence + adjustment * 0.5));
    
    logger().debug() << "[SkillAcquisition] Updated skill proficiency to " << skill.proficiency;
}

void SkillAcquisition::indexSkill(const Skill& skill, size_t index)
{
    _skill_index[skill.id] = index;
    _name_index[skill.name].push_back(index);
    
    for (const Handle& context : skill.contexts) {
        _context_index[context].push_back(index);
    }
}

Handle SkillAcquisition::decomposeTaskIntoActions(const Handle& task)
{
    // Simple implementation - return task as action sequence
    return _atomspace->add_link(LIST_LINK, task);
}

Handle SkillAcquisition::identifySkillPreconditions(const Handle& task, const Handle& context)
{
    // Simple implementation - use context as precondition
    return context;
}

Handle SkillAcquisition::identifySkillPostconditions(const Handle& task, const Handle& context)
{
    // Simple implementation - create success outcome
    return _atomspace->add_node(CONCEPT_NODE, "TaskCompleted");
}

void SkillAcquisition::refineSkillThroughPractice(size_t skill_index)
{
    if (skill_index >= _skills.size()) return;
    
    Skill& skill = _skills[skill_index];
    
    // Simulate practice improvement
    skill.proficiency = std::min(1.0, skill.proficiency + _config.learning_rate * 0.1);
    skill.confidence = std::min(1.0, skill.confidence + _config.learning_rate * 0.05);
}

std::vector<Skill> SkillAcquisition::getProficientSkills(double threshold) const
{
    std::vector<Skill> proficient;
    for (const auto& skill : _skills) {
        if (skill.proficiency >= threshold) {
            proficient.push_back(skill);
        }
    }
    return proficient;
}

bool SkillAcquisition::validateSkillIntegrity() const
{
    return _skill_index.size() <= _skills.size();
}

void SkillAcquisition::performMaintenance()
{
    decayUnusedSkills();
    logger().debug() << "[SkillAcquisition] Performed maintenance tasks";
}

void SkillAcquisition::decayUnusedSkills()
{
    auto now = std::chrono::system_clock::now();
    
    for (auto& skill : _skills) {
        auto age = std::chrono::duration_cast<std::chrono::hours>(now - skill.last_used);
        if (age > _config.skill_decay_period) {
            skill.proficiency *= 0.95; // Decay proficiency slightly
            skill.confidence *= 0.98;  // Decay confidence slightly
        }
    }
}
SkillAcquisition::SkillAcquisition(AtomSpacePtr atomspace)
    : _atomspace(atomspace), _initialized(false)
{
    logger().info() << "[SkillAcquisition] Creating skill acquisition module";
}

SkillAcquisition::~SkillAcquisition()
{
    logger().info() << "[SkillAcquisition] Destroyed skill acquisition module";
}

bool SkillAcquisition::initialize()
{
    if (_initialized) {
        return true;
    }

    if (!_atomspace) {
        logger().error() << "[SkillAcquisition] AtomSpace is null";
        return false;
    }

    _initialized = true;
    logger().info() << "[SkillAcquisition] Skill acquisition initialized";
    return true;
}

std::vector<Handle> SkillAcquisition::learnSkills(const std::vector<Handle>& experiences)
{
    std::vector<Handle> learned_skills;
    
    if (!_initialized) {
        logger().error() << "[SkillAcquisition] Not initialized";
        return learned_skills;
    }

    try {
        // Simple skill learning implementation
        for (const auto& experience : experiences) {
            if (experience != Handle::UNDEFINED) {
                // Create a skill based on the experience
                Handle skill = _atomspace->add_node(CONCEPT_NODE, 
                    "learned_skill_from_" + experience->get_name());
                learned_skills.push_back(skill);
            }
        }

        logger().info() << "[SkillAcquisition] Learned " << learned_skills.size() << " skills";
    }
    catch (const std::exception& e) {
        logger().error() << "[SkillAcquisition] Error learning skills: " << e.what();
    }

    return learned_skills;
}
 * SkillAcquisition.cpp - Implementation of Hierarchical Skill Learning
 * 
 * Part of AZ-LEARN-003: MOSES Policy Optimization Integration
 * Copyright (C) 2024 OpenCog Foundation
 */

#include <agentzero/learning/SkillAcquisition.h>
#include <agentzero/learning/PolicyOptimizer.h>
#include <agentzero/learning/ExperienceManager.h>
#include <agentzero/learning/LearningUtils.h>

#include <opencog/util/Logger.h>

namespace opencog {
namespace agentzero {
namespace learning {

SkillAcquisition::SkillAcquisition(AtomSpacePtr atomspace,
                                 std::shared_ptr<PolicyOptimizer> policy_optimizer,
                                 std::shared_ptr<ExperienceManager> experience_manager,
                                 const LearningConfig& config)
    : atomspace_(atomspace), policy_optimizer_(policy_optimizer), 
      experience_manager_(experience_manager), config_(config) {
    
    if (!atomspace_) {
        throw SkillAcquisitionException("AtomSpace cannot be null");
    }
    
    if (!policy_optimizer_) {
        throw SkillAcquisitionException("PolicyOptimizer cannot be null");
    }
    
    if (!experience_manager_) {
        throw SkillAcquisitionException("ExperienceManager cannot be null");
    }
    
    logger().info("SkillAcquisition: Initialized");
}

SkillAcquisition::~SkillAcquisition() {
    logger().info("SkillAcquisition: Destroyed with %zu skills", skill_cache_.size());
}

std::shared_ptr<Skill> SkillAcquisition::acquireSkill(const std::string& skill_name,
                                                     const std::vector<PolicyId>& component_policies) {
    try {
        SkillId skill_id = utils::generateUniqueId("skill_");
        
        auto skill = std::make_shared<Skill>(skill_id, Handle::UNDEFINED, skill_name);
        skill->sub_policies = component_policies;
        skill->description = "Skill composed of " + std::to_string(component_policies.size()) + " policies";
        
        // Store in cache
        {
            std::lock_guard<std::mutex> lock(skill_cache_mutex_);
            skill_cache_[skill_id] = skill;
        }
        
        // Store in AtomSpace
        storeSkillInAtomSpace(*skill);
        
        logger().info("SkillAcquisition: Acquired skill '%s' (%s)", 
                      skill_name.c_str(), skill_id.c_str());
        
        return skill;
        
    } catch (const std::exception& e) {
        logger().error("SkillAcquisition: Error acquiring skill '%s': %s", 
                       skill_name.c_str(), e.what());
        return nullptr;
    }
}

std::vector<std::shared_ptr<Skill>> SkillAcquisition::discoverSkillsFromExperience() {
    // Basic implementation - can be enhanced with pattern mining
    logger().info("SkillAcquisition: Discovering skills from experience (basic implementation)");
    
    std::vector<std::shared_ptr<Skill>> discovered_skills;
    
    // Get recent high-reward experiences
    auto recent_experiences = experience_manager_->getRecentExperiences(100);
    auto high_reward_experiences = experience_manager_->getExperiencesByRewardRange(0.5, 1.0);
    
    if (high_reward_experiences.size() >= config_.skill_complexity_threshold) {
        // Create a skill from successful experience patterns
        auto skill = acquireSkill("DiscoveredSkill_" + utils::generateUniqueId(), {});
        if (skill) {
            discovered_skills.push_back(skill);
        }
    }
    
    return discovered_skills;
}

bool SkillAcquisition::composeSkill(const SkillId& new_skill_id,
                                   const std::vector<SkillId>& component_skills) {
    try {
        // Get component skills
        std::vector<std::shared_ptr<Skill>> components;
        for (const auto& skill_id : component_skills) {
            auto skill = getSkill(skill_id);
            if (!skill) {
                logger().error("SkillAcquisition: Component skill '%s' not found", skill_id.c_str());
                return false;
            }
            components.push_back(skill);
        }
        
        // Create composed skill
        auto composed_skill = std::make_shared<Skill>(new_skill_id, Handle::UNDEFINED, "ComposedSkill");
        composed_skill->description = "Skill composed of " + std::to_string(components.size()) + " sub-skills";
        
        // Combine policies from all component skills
        for (const auto& component : components) {
            composed_skill->sub_policies.insert(
                composed_skill->sub_policies.end(),
                component->sub_policies.begin(),
                component->sub_policies.end()
            );
        }
        
        // Store in cache and AtomSpace
        {
            std::lock_guard<std::mutex> lock(skill_cache_mutex_);
            skill_cache_[new_skill_id] = composed_skill;
        }
        
        storeSkillInAtomSpace(*composed_skill);
        
        logger().info("SkillAcquisition: Composed skill '%s' from %zu components", 
                      new_skill_id.c_str(), components.size());
        
        return true;
        
    } catch (const std::exception& e) {
        logger().error("SkillAcquisition: Error composing skill '%s': %s", 
                       new_skill_id.c_str(), e.what());
        return false;
    }
}

std::shared_ptr<Skill> SkillAcquisition::getSkill(const SkillId& skill_id) {
    // Check cache first
    {
        std::lock_guard<std::mutex> lock(skill_cache_mutex_);
        auto it = skill_cache_.find(skill_id);
        if (it != skill_cache_.end()) {
            return it->second;
        }
    }
    
    // Try to retrieve from AtomSpace
    return retrieveSkillFromAtomSpace(skill_id);
}

std::vector<std::shared_ptr<Skill>> SkillAcquisition::getAllSkills() {
    std::vector<std::shared_ptr<Skill>> skills;
    
    std::lock_guard<std::mutex> lock(skill_cache_mutex_);
    for (const auto& pair : skill_cache_) {
        skills.push_back(pair.second);
    }
    
    return skills;
}

Handle SkillAcquisition::storeSkillInAtomSpace(const Skill& skill) {
    try {
        // Create skill node
        Handle skill_node = atomspace_->add_node(CONCEPT_NODE, 
            config_.skill_atom_prefix + skill.id);
        
        // Store skill metadata
        skill_node->setValue(createNode(PREDICATE_NODE, "skill_name"),
                           createStringValue(skill.skill_name));
        
        skill_node->setValue(createNode(PREDICATE_NODE, "description"),
                           createStringValue(skill.description));
        
        skill_node->setValue(createNode(PREDICATE_NODE, "success_rate"),
                           createFloatValue(skill.success_rate));
        
        skill_node->setValue(createNode(PREDICATE_NODE, "complexity_score"),
                           createFloatValue(skill.complexity_score));
        
        logger().debug("SkillAcquisition: Stored skill '%s' in AtomSpace", skill.id.c_str());
        
        return skill_node;
        
    } catch (const std::exception& e) {
        logger().error("SkillAcquisition: Error storing skill '%s' in AtomSpace: %s", 
                       skill.id.c_str(), e.what());
        return Handle::UNDEFINED;
    }
}

std::shared_ptr<Skill> SkillAcquisition::retrieveSkillFromAtomSpace(const SkillId& skill_id) {
    try {
        Handle skill_node = atomspace_->get_node(CONCEPT_NODE, 
            config_.skill_atom_prefix + skill_id);
        
        if (skill_node == Handle::UNDEFINED) {
            return nullptr;
        }
        
        // Basic reconstruction - can be enhanced
        auto skill = std::make_shared<Skill>(skill_id, skill_node, "RetrievedSkill");
        
        // Add to cache
        {
            std::lock_guard<std::mutex> lock(skill_cache_mutex_);
            skill_cache_[skill_id] = skill;
        }
        
        return skill;
        
    } catch (const std::exception& e) {
        logger().error("SkillAcquisition: Error retrieving skill '%s' from AtomSpace: %s", 
                       skill_id.c_str(), e.what());
        return nullptr;
    }
}

} // namespace learning
} // namespace agentzero
} // namespace opencog
