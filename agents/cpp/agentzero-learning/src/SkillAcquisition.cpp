/*
 * opencog/agentzero/SkillAcquisition.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
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

using namespace opencog;
using namespace opencog::agentzero;

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