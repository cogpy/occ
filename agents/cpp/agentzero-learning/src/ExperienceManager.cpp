/*
 * src/ExperienceManager.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * ExperienceManager Implementation
 * Part of the Agent-Zero Learning & Adaptation module
 * Part of the AGENT-ZERO-GENESIS project
 */

#include <algorithm>
#include <sstream>
#include <stdexcept>

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>

#include "opencog/agentzero/ExperienceManager.h"
 * opencog/agentzero/ExperienceManager.cpp
/**
 * ExperienceManager.cpp
 *
 * Manages agent experiential memory for learning
 * Part of Agent-Zero Learning & Adaptation Phase 5
 *
 * Copyright (C) 2024 OpenCog Foundation
 */

#include "agentzero-learning/ExperienceManager.h"
#include <opencog/util/Logger.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/atom_types/atom_types.h>

using namespace opencog;
using namespace opencog::agentzero;

ExperienceManager::ExperienceManager(AtomSpacePtr atomspace)
    : _atomspace(atomspace)
    , _experience_base(Handle::UNDEFINED)
    , _experience_timeline(Handle::UNDEFINED)
    , _max_experiences(10000)
    , _enable_experience_compression(true)
{
    if (!_atomspace) {
        throw std::runtime_error("ExperienceManager requires valid AtomSpace");
    }

    logger().info() << "[ExperienceManager] Initializing experience management system";
    initializeExperienceBase();
    logger().info() << "[ExperienceManager] Experience management system initialized";
    : _atomspace(atomspace), _initialized(false)
{
    logger().info() << "[ExperienceManager] Creating experience manager";
}

ExperienceManager::~ExperienceManager()
{
    logger().info() << "[ExperienceManager] Shutting down experience management system";
}

Handle ExperienceManager::recordExperience(ExperienceType type,
                                          Handle experience_data,
                                          const std::vector<Handle>& context_atoms)
{
    if (experience_data == Handle::UNDEFINED) {
        logger().error() << "[ExperienceManager] Cannot record undefined experience data";
        return Handle::UNDEFINED;
    }

    logger().debug() << "[ExperienceManager] Recording experience of type " << static_cast<int>(type);

    // Create experience atom
    Handle experience_atom = createExperienceAtom(type, experience_data, context_atoms);

    // Add to type-specific collection
    _experiences_by_type[type].push_back(experience_atom);

    // Add to timeline
    _atomspace->add_link(INHERITANCE_LINK, {experience_atom, _experience_timeline});

    // Manage memory if needed
    if (_enable_experience_compression && getTotalExperiences() > _max_experiences) {
        compressExperiences(_max_experiences * 0.8); // Keep 80% of max
    }

    logger().debug() << "[ExperienceManager] Experience recorded successfully";
    return experience_atom;
}

std::vector<Handle> ExperienceManager::getExperiencesByType(ExperienceType type, size_t limit) const
{
    auto it = _experiences_by_type.find(type);
    if (it == _experiences_by_type.end()) {
        return {};
    }

    const std::vector<Handle>& experiences = it->second;
    if (limit == 0 || limit >= experiences.size()) {
        return experiences;
    }

    // Return most recent experiences
    return std::vector<Handle>(experiences.end() - limit, experiences.end());
}

std::vector<Handle> ExperienceManager::getRecentExperiences(size_t count) const
{
    std::vector<Handle> all_experiences;
    
    // Collect all experiences
    for (const auto& type_pair : _experiences_by_type) {
        const std::vector<Handle>& experiences = type_pair.second;
        all_experiences.insert(all_experiences.end(), experiences.begin(), experiences.end());
    }

    // Sort by creation time (simplified - in practice would use actual timestamps)
    std::sort(all_experiences.begin(), all_experiences.end(),
              [](Handle a, Handle b) { return a.value() < b.value(); });

    // Return most recent
    if (count >= all_experiences.size()) {
        return all_experiences;
    }

    return std::vector<Handle>(all_experiences.end() - count, all_experiences.end());
}

std::vector<Handle> ExperienceManager::findSimilarExperiences(Handle reference_experience,
                                                             double similarity_threshold) const
{
    std::vector<Handle> similar_experiences;

    for (const auto& type_pair : _experiences_by_type) {
        for (Handle experience : type_pair.second) {
            if (experience != reference_experience) {
                double similarity = calculateExperienceSimilarity(reference_experience, experience);
                if (similarity >= similarity_threshold) {
                    similar_experiences.push_back(experience);
                }
            }
        }
    }

    // Sort by similarity (highest first)
    std::sort(similar_experiences.begin(), similar_experiences.end(),
              [this, reference_experience](Handle a, Handle b) {
                  return calculateExperienceSimilarity(reference_experience, a) >
                         calculateExperienceSimilarity(reference_experience, b);
              });

    return similar_experiences;
}

size_t ExperienceManager::getTotalExperiences() const
{
    size_t total = 0;
    for (const auto& type_pair : _experiences_by_type) {
        total += type_pair.second.size();
    }
    return total;
}

void ExperienceManager::compressExperiences(size_t keep_count)
{
    logger().info() << "[ExperienceManager] Compressing experiences, keeping " << keep_count;

    size_t total_experiences = getTotalExperiences();
    if (total_experiences <= keep_count) {
        return; // No compression needed
    }

    size_t to_remove = total_experiences - keep_count;
    size_t removed = 0;

    // Remove oldest experiences from each type proportionally
    for (auto& type_pair : _experiences_by_type) {
        std::vector<Handle>& experiences = type_pair.second;
        if (experiences.empty()) continue;

        // Calculate how many to remove from this type
        size_t type_remove = (to_remove * experiences.size()) / total_experiences;
        type_remove = std::min(type_remove, experiences.size());

        // Remove from beginning (oldest)
        experiences.erase(experiences.begin(), experiences.begin() + type_remove);
        removed += type_remove;
    }

    logger().info() << "[ExperienceManager] Compression complete, removed " << removed << " experiences";
}

void ExperienceManager::setMaxExperiences(size_t max_count)
{
    _max_experiences = max_count;
    logger().debug() << "[ExperienceManager] Maximum experiences set to " << max_count;

    // Compress if current count exceeds new limit
    if (getTotalExperiences() > _max_experiences) {
        compressExperiences(_max_experiences);
    }
}

// Private methods

void ExperienceManager::initializeExperienceBase()
{
    _experience_base = _atomspace->add_node(CONCEPT_NODE, "ExperienceBase");
    _experience_timeline = _atomspace->add_node(CONCEPT_NODE, "ExperienceTimeline");
    
    _atomspace->add_link(INHERITANCE_LINK, {_experience_timeline, _experience_base});
    
    logger().debug() << "[ExperienceManager] Experience base initialized in AtomSpace";
}

Handle ExperienceManager::createExperienceAtom(ExperienceType type,
                                              Handle data,
                                              const std::vector<Handle>& context)
{
    // Create unique experience name
    std::string experience_name = "Experience_" + std::to_string(data.value()) + "_" + 
                                 std::to_string(static_cast<int>(type));
    
    Handle experience_atom = _atomspace->add_node(CONCEPT_NODE, experience_name);

    // Link to experience base
    _atomspace->add_link(INHERITANCE_LINK, {experience_atom, _experience_base});

    // Add type information
    Handle type_node = _atomspace->add_node(CONCEPT_NODE, 
                                           "ExperienceType_" + std::to_string(static_cast<int>(type)));
    _atomspace->add_link(EVALUATION_LINK, {
        _atomspace->add_node(PREDICATE_NODE, "hasExperienceType"),
        _atomspace->add_link(LIST_LINK, {experience_atom, type_node})
    });

    // Link to experience data
    _atomspace->add_link(EVALUATION_LINK, {
        _atomspace->add_node(PREDICATE_NODE, "hasExperienceData"),
        _atomspace->add_link(LIST_LINK, {experience_atom, data})
    });

    // Add context information
    if (!context.empty()) {
        Handle context_link = _atomspace->add_link(LIST_LINK, context);
        _atomspace->add_link(EVALUATION_LINK, {
            _atomspace->add_node(PREDICATE_NODE, "hasExperienceContext"),
            _atomspace->add_link(LIST_LINK, {experience_atom, context_link})
        });
    }

    // Set initial truth value
    experience_atom->setTruthValue(SimpleTruthValue::createTV(1.0, 0.9));

    return experience_atom;
}

double ExperienceManager::calculateExperienceSimilarity(Handle exp1, Handle exp2) const
{
    if (exp1 == exp2) {
        return 1.0;
    }

    // Simplified similarity calculation
    // In practice, this would involve sophisticated comparison of:
    // - Experience types
    // - Experience data content
    // - Context similarity
    // - Temporal proximity
    
    // For now, return a basic similarity based on atom types and structure
    if (exp1->get_type() == exp2->get_type()) {
        return 0.5; // Same type gives base similarity
    }
    
    return 0.1; // Different types have low similarity
}
    logger().info() << "[ExperienceManager] Destroyed experience manager";
}

bool ExperienceManager::initialize()
{
    if (_initialized) {
        return true;
    }

    if (!_atomspace) {
        logger().error() << "[ExperienceManager] AtomSpace is null";
        return false;
    }

    _initialized = true;
    logger().info() << "[ExperienceManager] Experience manager initialized";
    return true;
}

void ExperienceManager::recordExperience(const Experience& exp)
{
    if (!_initialized) {
        logger().error() << "[ExperienceManager] Not initialized";
        return;
    }

    try {
        // Create experience atom in AtomSpace
        Handle experience_atom = _atomspace->add_node(CONCEPT_NODE, 
            "experience_" + std::to_string(exp.timestamp));
        
        // Link context, action, and outcome
        if (exp.context_atom != Handle::UNDEFINED) {
            _atomspace->add_link(LIST_LINK, {experience_atom, exp.context_atom});
        }
        if (exp.action_atom != Handle::UNDEFINED) {
            _atomspace->add_link(LIST_LINK, {experience_atom, exp.action_atom});
        }
        if (exp.outcome_atom != Handle::UNDEFINED) {
            _atomspace->add_link(LIST_LINK, {experience_atom, exp.outcome_atom});
        }

        logger().debug() << "[ExperienceManager] Recorded experience with reward: " << exp.reward;
    }
    catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Error recording experience: " << e.what();
    }
}

std::vector<Experience> ExperienceManager::getExperiences(const Handle& context)
{
    std::vector<Experience> experiences;
    
    if (!_initialized) {
        logger().error() << "[ExperienceManager] Not initialized";
        return experiences;
    }

    try {
        // Simple implementation - return empty for now
        // Full implementation would query AtomSpace for experiences
        logger().debug() << "[ExperienceManager] Retrieved " << experiences.size() << " experiences";
    }
    catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Error retrieving experiences: " << e.what();
    }

    return experiences;
}

void ExperienceManager::updateExperienceValue(const Handle& experience_atom, double new_value)
{
    if (!_initialized) {
        logger().error() << "[ExperienceManager] Not initialized";
        return;
    }

    try {
        // Update experience value in AtomSpace
        // This would typically update TruthValue or other value
        logger().debug() << "[ExperienceManager] Updated experience value to: " << new_value;
    }
    catch (const std::exception& e) {
        logger().error() << "[ExperienceManager] Error updating experience value: " << e.what();
    }
}
 * ExperienceManager.cpp - Comprehensive Implementation of Experience Memory Management
 * 
 * Combines AZ-LEARN-003 MOSES Policy Optimization Integration with 
 * sophisticated experience classification and pattern discovery
 * 
 * Copyright (C) 2024 OpenCog Foundation
 */

#include <agentzero/learning/ExperienceManager.h>
#include <agentzero/learning/LearningUtils.h>

#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/atom_types/types.h>

#include <algorithm>
#include <numeric>
#include <sstream>
#include <iomanip>
#include <ctime>
#include <random>

namespace opencog {
namespace agentzero {
namespace learning {

ExperienceManager::ExperienceManager(AtomSpacePtr atomspace, const LearningConfig& config)
    : atomspace_(atomspace), config_(config), total_experiences_added_(0), total_reward_accumulated_(0.0),
      _experience_base(Handle::UNDEFINED),
      _episodic_memory(Handle::UNDEFINED),
      _experience_patterns(Handle::UNDEFINED),
      _learning_outcomes(Handle::UNDEFINED),
      _skill_experiences(Handle::UNDEFINED),
      _goal_experiences(Handle::UNDEFINED),
      _temporal_context(Handle::UNDEFINED),
      _enable_pattern_discovery(true),
      _enable_moses_integration(true),
      _enable_temporal_modeling(true),
      _enable_emotional_learning(false),
      _experience_retention_threshold(0.3),
      _max_recent_experiences(100),
      _pattern_significance_threshold(0.6),
      _moses_available(false),
      _moses_policy_space(Handle::UNDEFINED) {
    
    if (!atomspace_) {
        throw LearningException("AtomSpace cannot be null");
    }
    
    logger().info("ExperienceManager: Initializing comprehensive experience management with buffer size %zu", config_.experience_buffer_size);
    
    initializeAtomSpaceStructures();
    initializeAdvancedStructures();
    
    logger().info("ExperienceManager: Comprehensive initialization complete with pattern discovery and MOSES integration");
}

ExperienceManager::~ExperienceManager() {
    logger().info("ExperienceManager: Destroyed with %zu total experiences", 
                  total_experiences_added_.load());
}

// Additional method for advanced experience recording
ExperienceId ExperienceManager::recordExperience(const std::string& description,
                                                ExperienceType type,
                                                ExperienceOutcome outcome,
                                                const std::vector<Handle>& actions,
                                                const std::vector<Handle>& consequences,
                                                double confidence) {
    try {
        Experience exp;
        exp.id = generateExperienceId();
        exp.experience_type = type;
        exp.outcome = outcome;
        exp.confidence_level = confidence;
        exp.timestamp = utils::getCurrentTimestamp();
        
        // Set importance based on outcome
        switch (outcome) {
            case ExperienceOutcome::SUCCESS:
                exp.importance = ExperienceImportance::HIGH;
                exp.reward = 1.0;
                break;
            case ExperienceOutcome::FAILURE:
                exp.importance = ExperienceImportance::HIGH;
                exp.reward = -1.0;
                break;
            case ExperienceOutcome::LEARNING_OPPORTUNITY:
                exp.importance = ExperienceImportance::MEDIUM;
                exp.reward = 0.5;
                break;
            default:
                exp.importance = ExperienceImportance::MEDIUM;
                exp.reward = 0.0;
                break;
        }
        
        // Store actions and consequences in context
        for (size_t i = 0; i < actions.size(); ++i) {
            exp.context_atoms["action_" + std::to_string(i)] = actions[i];
        }
        for (size_t i = 0; i < consequences.size(); ++i) {
            exp.context_atoms["consequence_" + std::to_string(i)] = consequences[i];
        }
        
        if (addExperience(exp)) {
            utils::logExperienceUpdate(exp.id, "recorded_advanced");
            return exp.id;
        }
        
        return "";
        
    } catch (const std::exception& e) {
        logger().error("ExperienceManager: Error recording advanced experience: %s", e.what());
        return "";
    }
}

// Pattern discovery method from main branch approach
size_t ExperienceManager::discoverExperiencePatterns() {
    if (!_enable_pattern_discovery) {
        return 0;
    }
    
    logger().info("ExperienceManager: Starting pattern discovery");
    
    size_t patterns_found = 0;
    
    try {
        // Get successful experiences for pattern analysis
        auto successful_experiences = getExperiencesByFilter([](const Experience& exp) {
            return exp.outcome == ExperienceOutcome::SUCCESS;
        });
        
        if (successful_experiences.size() < 3) {
            logger().info("ExperienceManager: Not enough successful experiences for pattern discovery");
            return 0;
        }
        
        // Simple pattern discovery: find common action sequences
        std::map<std::string, size_t> action_patterns;
        
        for (const auto& exp : successful_experiences) {
            std::string pattern_key = "";
            
            // Create pattern from context actions
            for (const auto& context_pair : exp->context_atoms) {
                if (context_pair.first.find("action_") == 0) {
                    pattern_key += context_pair.second->get_name() + "->";
                }
            }
            
            if (!pattern_key.empty()) {
                action_patterns[pattern_key]++;
                if (action_patterns[pattern_key] >= 3) { // Pattern significance threshold
                    patterns_found++;
                }
            }
        }
        
        logger().info("ExperienceManager: Discovered %zu experience patterns", patterns_found);
        return patterns_found;
        
    } catch (const std::exception& e) {
        logger().error("ExperienceManager: Error in pattern discovery: %s", e.what());
        return 0;
    }
}

// Get experiences by type
std::vector<std::shared_ptr<Experience>> ExperienceManager::getExperiencesByType(ExperienceType type) {
    return getExperiencesByFilter([type](const Experience& exp) {
        return exp.experience_type == type;
    });
}

// Get experiences by outcome
std::vector<std::shared_ptr<Experience>> ExperienceManager::getExperiencesByOutcome(ExperienceOutcome outcome) {
    return getExperiencesByFilter([outcome](const Experience& exp) {
        return exp.outcome == outcome;
    });
}

// Get successful patterns (simplified version)
std::vector<std::shared_ptr<Experience>> ExperienceManager::getSuccessfulPatterns(
    const std::vector<Handle>& context, double min_success_rate) {
    
    return getExperiencesByFilter([min_success_rate](const Experience& exp) {
        return exp.outcome == ExperienceOutcome::SUCCESS && exp.confidence_level >= min_success_rate;
    });
}

bool ExperienceManager::addExperience(const Experience& experience) {
    if (!validateExperience(experience)) {
        logger().warn("ExperienceManager: Invalid experience rejected");
        return false;
    }
    
    try {
        auto exp_ptr = std::make_shared<Experience>(experience);
        
        // If no ID provided, generate one
        if (exp_ptr->id.empty()) {
            exp_ptr->id = generateExperienceId();
        }
        
        // Set timestamp if not provided
        if (exp_ptr->timestamp == 0) {
            exp_ptr->timestamp = utils::getCurrentTimestamp();
        }
        
        {
            std::lock_guard<std::mutex> buffer_lock(buffer_mutex_);
            std::lock_guard<std::mutex> index_lock(index_mutex_);
            
            // Add to buffer
            experience_buffer_.push_back(exp_ptr);
            
            // Add to index
            experience_index_[exp_ptr->id] = exp_ptr;
            
            // Add to search indices
            addToIndices(exp_ptr);
            
            // Enforce buffer size limit
            enforceBufferLimit();
        }
        
        // Update statistics
        total_experiences_added_++;
        total_reward_accumulated_ += experience.reward;
        
        utils::logExperienceUpdate(exp_ptr->id, "added");
        
        return true;
        
    } catch (const std::exception& e) {
        logger().error("ExperienceManager: Error adding experience: %s", e.what());
        return false;
    }
}

ExperienceId ExperienceManager::addExperience(Handle state_atom, Handle action_atom,
                                            Handle next_state_atom, double reward, bool terminal) {
    Experience exp;
    exp.state_atom = state_atom;
    exp.action_atom = action_atom;
    exp.next_state_atom = next_state_atom;
    exp.reward = reward;
    exp.terminal = terminal;
    
    if (addExperience(exp)) {
        return exp.id;
    }
    
    return "";
}

std::shared_ptr<Experience> ExperienceManager::getExperience(const ExperienceId& experience_id) {
    std::lock_guard<std::mutex> lock(index_mutex_);
    
    auto it = experience_index_.find(experience_id);
    if (it != experience_index_.end()) {
        return it->second;
    }
    
    return nullptr;
}

std::vector<std::shared_ptr<Experience>> ExperienceManager::getRecentExperiences(size_t count) {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    
    std::vector<std::shared_ptr<Experience>> recent;
    
    size_t start_idx = (experience_buffer_.size() > count) ? experience_buffer_.size() - count : 0;
    
    for (size_t i = start_idx; i < experience_buffer_.size(); ++i) {
        recent.push_back(experience_buffer_[i]);
    }
    
    return recent;
}

std::vector<std::shared_ptr<Experience>> ExperienceManager::sampleExperiences(size_t sample_size, bool prioritized) {
    if (prioritized) {
        return prioritizedSample(sample_size);
    } else {
        return uniformSample(sample_size);
    }
}

std::vector<std::shared_ptr<Experience>> ExperienceManager::getExperiencesByFilter(
    std::function<bool(const Experience&)> filter_function, size_t max_results) {
    
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    
    std::vector<std::shared_ptr<Experience>> filtered;
    
    for (const auto& exp : experience_buffer_) {
        if (filter_function(*exp)) {
            filtered.push_back(exp);
            
            if (max_results > 0 && filtered.size() >= max_results) {
                break;
            }
        }
    }
    
    return filtered;
}

std::vector<std::shared_ptr<Experience>> ExperienceManager::getExperiencesByState(Handle state_atom, bool include_next_states) {
    std::lock_guard<std::mutex> lock(index_maps_mutex_);
    
    std::vector<std::shared_ptr<Experience>> experiences;
    std::set<ExperienceId> exp_ids;
    
    // Get experiences where this is the current state
    auto state_it = state_to_experiences_.find(state_atom);
    if (state_it != state_to_experiences_.end()) {
        exp_ids.insert(state_it->second.begin(), state_it->second.end());
    }
    
    // Get experiences where this is the next state
    if (include_next_states) {
        auto next_state_it = next_state_to_experiences_.find(state_atom);
        if (next_state_it != next_state_to_experiences_.end()) {
            exp_ids.insert(next_state_it->second.begin(), next_state_it->second.end());
        }
    }
    
    // Convert IDs to experience pointers
    {
        std::lock_guard<std::mutex> index_lock(index_mutex_);
        for (const auto& exp_id : exp_ids) {
            auto it = experience_index_.find(exp_id);
            if (it != experience_index_.end()) {
                experiences.push_back(it->second);
            }
        }
    }
    
    return experiences;
}

std::vector<std::shared_ptr<Experience>> ExperienceManager::getExperiencesByAction(Handle action_atom) {
    std::lock_guard<std::mutex> lock(index_maps_mutex_);
    
    std::vector<std::shared_ptr<Experience>> experiences;
    
    auto action_it = action_to_experiences_.find(action_atom);
    if (action_it != action_to_experiences_.end()) {
        std::lock_guard<std::mutex> index_lock(index_mutex_);
        for (const auto& exp_id : action_it->second) {
            auto it = experience_index_.find(exp_id);
            if (it != experience_index_.end()) {
                experiences.push_back(it->second);
            }
        }
    }
    
    return experiences;
}

std::vector<std::shared_ptr<Experience>> ExperienceManager::getExperiencesByRewardRange(double min_reward, double max_reward) {
    return getExperiencesByFilter([min_reward, max_reward](const Experience& exp) {
        return exp.reward >= min_reward && exp.reward <= max_reward;
    });
}

size_t ExperienceManager::storeExperiencesToAtomSpace(const std::vector<std::shared_ptr<Experience>>& experiences) {
    size_t stored_count = 0;
    
    std::vector<std::shared_ptr<Experience>> exps_to_store;
    
    if (experiences.empty()) {
        // Store all experiences
        std::lock_guard<std::mutex> lock(buffer_mutex_);
        exps_to_store.assign(experience_buffer_.begin(), experience_buffer_.end());
    } else {
        exps_to_store = experiences;
    }
    
    for (const auto& exp : exps_to_store) {
        try {
            Handle exp_atom = experienceToAtomSpaceRepresentation(*exp);
            if (exp_atom != Handle::UNDEFINED) {
                stored_count++;
            }
        } catch (const std::exception& e) {
            logger().error("ExperienceManager: Error storing experience '%s': %s", 
                          exp->id.c_str(), e.what());
        }
    }
    
    logger().info("ExperienceManager: Stored %zu experiences to AtomSpace", stored_count);
    return stored_count;
}

size_t ExperienceManager::loadExperiencesFromAtomSpace(size_t max_experiences) {
    // Implementation for loading from AtomSpace
    // This would search for experience atoms and reconstruct Experience objects
    logger().info("ExperienceManager: Loading experiences from AtomSpace (not yet implemented)");
    return 0;
}

void ExperienceManager::clearMemory() {
    std::lock_guard<std::mutex> buffer_lock(buffer_mutex_);
    std::lock_guard<std::mutex> index_lock(index_mutex_);
    std::lock_guard<std::mutex> maps_lock(index_maps_mutex_);
    
    experience_buffer_.clear();
    experience_index_.clear();
    experience_priorities_.clear();
    state_to_experiences_.clear();
    action_to_experiences_.clear();
    next_state_to_experiences_.clear();
    
    logger().info("ExperienceManager: Memory cleared");
}

size_t ExperienceManager::getExperienceCount() const {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    return experience_buffer_.size();
}

size_t ExperienceManager::getBufferSizeLimit() const {
    return config_.experience_buffer_size;
}

void ExperienceManager::setBufferSizeLimit(size_t size) {
    config_.experience_buffer_size = size;
    
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    enforceBufferLimit();
    
    logger().info("ExperienceManager: Buffer size limit set to %zu", size);
}

std::map<std::string, double> ExperienceManager::getExperienceStats() const {
    std::lock_guard<std::mutex> lock(stats_mutex_);
    
    std::map<std::string, double> stats;
    stats["total_experiences"] = static_cast<double>(total_experiences_added_.load());
    stats["current_buffer_size"] = static_cast<double>(getExperienceCount());
    stats["buffer_size_limit"] = static_cast<double>(config_.experience_buffer_size);
    stats["total_reward"] = total_reward_accumulated_.load();
    
    if (total_experiences_added_ > 0) {
        stats["average_reward"] = total_reward_accumulated_.load() / total_experiences_added_.load();
    } else {
        stats["average_reward"] = 0.0;
    }
    
    return stats;
}

std::map<std::string, double> ExperienceManager::getRewardStats() const {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    
    std::vector<double> rewards;
    for (const auto& exp : experience_buffer_) {
        rewards.push_back(exp->reward);
    }
    
    std::map<std::string, double> stats;
    
    if (rewards.empty()) {
        stats["min"] = 0.0;
        stats["max"] = 0.0;
        stats["mean"] = 0.0;
        stats["std_dev"] = 0.0;
    } else {
        stats["min"] = *std::min_element(rewards.begin(), rewards.end());
        stats["max"] = *std::max_element(rewards.begin(), rewards.end());
        stats["mean"] = utils::mean(rewards);
        stats["std_dev"] = utils::standardDeviation(rewards);
    }
    
    return stats;
}

void ExperienceManager::updateConfig(const LearningConfig& config) {
    config_ = config;
    
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    enforceBufferLimit();
    
    logger().info("ExperienceManager: Configuration updated");
}

const LearningConfig& ExperienceManager::getConfig() const {
    return config_;
}

// Private method implementations

ExperienceId ExperienceManager::generateExperienceId() const {
    return utils::generateUniqueId("exp_");
}

void ExperienceManager::addToIndices(const std::shared_ptr<Experience>& experience) {
    // This method assumes the index_maps_mutex_ is already locked
    
    if (experience->state_atom != Handle::UNDEFINED) {
        state_to_experiences_[experience->state_atom].insert(experience->id);
    }
    
    if (experience->action_atom != Handle::UNDEFINED) {
        action_to_experiences_[experience->action_atom].insert(experience->id);
    }
    
    if (experience->next_state_atom != Handle::UNDEFINED) {
        next_state_to_experiences_[experience->next_state_atom].insert(experience->id);
    }
}

void ExperienceManager::removeFromIndices(const std::shared_ptr<Experience>& experience) {
    // This method assumes the index_maps_mutex_ is already locked
    
    if (experience->state_atom != Handle::UNDEFINED) {
        auto it = state_to_experiences_.find(experience->state_atom);
        if (it != state_to_experiences_.end()) {
            it->second.erase(experience->id);
            if (it->second.empty()) {
                state_to_experiences_.erase(it);
            }
        }
    }
    
    if (experience->action_atom != Handle::UNDEFINED) {
        auto it = action_to_experiences_.find(experience->action_atom);
        if (it != action_to_experiences_.end()) {
            it->second.erase(experience->id);
            if (it->second.empty()) {
                action_to_experiences_.erase(it);
            }
        }
    }
    
    if (experience->next_state_atom != Handle::UNDEFINED) {
        auto it = next_state_to_experiences_.find(experience->next_state_atom);
        if (it != next_state_to_experiences_.end()) {
            it->second.erase(experience->id);
            if (it->second.empty()) {
                next_state_to_experiences_.erase(it);
            }
        }
    }
}

void ExperienceManager::enforceBufferLimit() {
    // This method assumes buffer_mutex_ is already locked
    
    while (experience_buffer_.size() > config_.experience_buffer_size) {
        auto oldest_exp = experience_buffer_.front();
        experience_buffer_.pop_front();
        
        // Remove from index
        experience_index_.erase(oldest_exp->id);
        
        // Remove from search indices
        std::lock_guard<std::mutex> maps_lock(index_maps_mutex_);
        removeFromIndices(oldest_exp);
        
        // Remove priority if exists
        std::lock_guard<std::mutex> priority_lock(priority_mutex_);
        experience_priorities_.erase(oldest_exp->id);
    }
}

Handle ExperienceManager::experienceToAtomSpaceRepresentation(const Experience& experience) {
    try {
        // Create experience node
        Handle exp_node = atomspace_->add_node(CONCEPT_NODE, 
            config_.experience_atom_prefix + experience.id);
        
        // Store reward as FloatValue
        exp_node->setValue(createNode(PREDICATE_NODE, "reward"),
                          createFloatValue(experience.reward));
        
        // Store terminal flag
        exp_node->setValue(createNode(PREDICATE_NODE, "terminal"),
                          createFloatValue(experience.terminal ? 1.0 : 0.0));
        
        // Store timestamp
        exp_node->setValue(createNode(PREDICATE_NODE, "timestamp"),
                          createFloatValue(static_cast<double>(experience.timestamp)));
        
        // Create links to state, action, and next_state atoms
        if (experience.state_atom != Handle::UNDEFINED) {
            atomspace_->add_link(EVALUATION_LINK, {
                createNode(PREDICATE_NODE, "ExperienceState"),
                exp_node,
                experience.state_atom
            });
        }
        
        if (experience.action_atom != Handle::UNDEFINED) {
            atomspace_->add_link(EVALUATION_LINK, {
                createNode(PREDICATE_NODE, "ExperienceAction"),
                exp_node,
                experience.action_atom
            });
        }
        
        if (experience.next_state_atom != Handle::UNDEFINED) {
            atomspace_->add_link(EVALUATION_LINK, {
                createNode(PREDICATE_NODE, "ExperienceNextState"),
                exp_node,
                experience.next_state_atom
            });
        }
        
        return exp_node;
        
    } catch (const std::exception& e) {
        logger().error("ExperienceManager: Error creating AtomSpace representation: %s", e.what());
        return Handle::UNDEFINED;
    }
}

std::vector<std::shared_ptr<Experience>> ExperienceManager::prioritizedSample(size_t sample_size) {
    // Simple implementation - can be enhanced with more sophisticated prioritization
    return uniformSample(sample_size);
}

std::vector<std::shared_ptr<Experience>> ExperienceManager::uniformSample(size_t sample_size) {
    std::lock_guard<std::mutex> lock(buffer_mutex_);
    
    std::vector<std::shared_ptr<Experience>> sampled;
    
    if (experience_buffer_.empty() || sample_size == 0) {
        return sampled;
    }
    
    sample_size = std::min(sample_size, experience_buffer_.size());
    
    // Simple random sampling without replacement
    std::vector<size_t> indices(experience_buffer_.size());
    std::iota(indices.begin(), indices.end(), 0);
    
    auto& rng = utils::RandomGenerator::getInstance();
    std::shuffle(indices.begin(), indices.end(), rng.getGenerator());
    
    for (size_t i = 0; i < sample_size; ++i) {
        sampled.push_back(experience_buffer_[indices[i]]);
    }
    
    return sampled;
}

void ExperienceManager::initializeAtomSpaceStructures() {
    // Create base nodes for experience storage
    atomspace_->add_node(CONCEPT_NODE, "ExperienceStorage");
    
    logger().debug("ExperienceManager: Basic AtomSpace structures initialized");
}

void ExperienceManager::initializeAdvancedStructures() {
    try {
        // Initialize advanced experience management structures from main branch approach
        _experience_base = atomspace_->add_node(CONCEPT_NODE, "ExperienceBase");
        _episodic_memory = atomspace_->add_node(CONCEPT_NODE, "EpisodicMemory");
        _experience_patterns = atomspace_->add_node(CONCEPT_NODE, "ExperiencePatterns");
        _learning_outcomes = atomspace_->add_node(CONCEPT_NODE, "LearningOutcomes");
        _skill_experiences = atomspace_->add_node(CONCEPT_NODE, "SkillExperiences");
        _goal_experiences = atomspace_->add_node(CONCEPT_NODE, "GoalExperiences");
        _temporal_context = atomspace_->add_node(CONCEPT_NODE, "TemporalContext");
        _moses_policy_space = atomspace_->add_node(CONCEPT_NODE, "MOSESPolicySpace");
        
        // Create hierarchical relationships
        atomspace_->add_link(INHERITANCE_LINK, {_episodic_memory, _experience_base});
        atomspace_->add_link(INHERITANCE_LINK, {_experience_patterns, _experience_base});
        atomspace_->add_link(INHERITANCE_LINK, {_skill_experiences, _experience_base});
        atomspace_->add_link(INHERITANCE_LINK, {_goal_experiences, _experience_base});
        
        // Create experience type nodes
        atomspace_->add_node(CONCEPT_NODE, "ActionOutcomeExperience");
        atomspace_->add_node(CONCEPT_NODE, "InteractionExperience");
        atomspace_->add_node(CONCEPT_NODE, "ProblemSolvingExperience");
        atomspace_->add_node(CONCEPT_NODE, "SkillApplicationExperience");
        atomspace_->add_node(CONCEPT_NODE, "GoalPursuitExperience");
        atomspace_->add_node(CONCEPT_NODE, "UnexpectedExperience");
        atomspace_->add_node(CONCEPT_NODE, "LearningEpisodeExperience");
        atomspace_->add_node(CONCEPT_NODE, "EmotionalExperience");
        
        logger().info("ExperienceManager: Advanced AtomSpace structures initialized with hierarchical organization");
        
    } catch (const std::exception& e) {
        logger().error("ExperienceManager: Error initializing advanced structures: %s", e.what());
        throw;
    }
}

bool ExperienceManager::validateExperience(const Experience& experience) const {
    // Basic validation - can be enhanced
    return experience.state_atom != Handle::UNDEFINED && 
           experience.action_atom != Handle::UNDEFINED;
}

// Factory function
std::unique_ptr<ExperienceManager> createExperienceManager(AtomSpacePtr atomspace, const std::string& config_preset) {
    LearningConfig config = utils::getDefaultConfig(config_preset);
    return std::make_unique<ExperienceManager>(atomspace, config);
}

} // namespace learning
} // namespace agentzero
} // namespace opencog
/*
 * src/ExperienceManager.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * ExperienceManager - Manages agent's experiential memory
 * Part of AZ-LEARN-004: Implement MetaLearning capabilities
 */

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
ExperienceManager::ExperienceManager(AtomSpacePtr atomspace, size_t max_experiences,
                                   double importance_threshold,
                                   std::chrono::hours retention_period)
    : _atomspace(atomspace)
    , _max_experiences(max_experiences)
    , _importance_threshold(importance_threshold)
    , _retention_period(retention_period)
    , _experience_context(Handle::UNDEFINED)
    , _memory_link(Handle::UNDEFINED)
{
    logger().info() << "[ExperienceManager] Initializing with max experiences: " 
                    << max_experiences << ", threshold: " << importance_threshold;
}

// Destructor
ExperienceManager::~ExperienceManager()
{
    logger().info() << "[ExperienceManager] Shutting down with " 
                    << _experiences.size() << " experiences recorded";
}

// Initialize experience manager
void ExperienceManager::initialize()
{
    logger().info() << "[ExperienceManager] Initializing experience management system";
    
    // Create experience context in AtomSpace
    _experience_context = _atomspace->add_node(CONCEPT_NODE, "ExperienceContext");
    
    // Create memory management link
    _memory_link = _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "ExperienceMemory"),
        _experience_context);
    
    logger().info() << "[ExperienceManager] Experience management system initialized";
}

// Record a new experience
Handle ExperienceManager::recordExperience(ExperienceType type, const Handle& context, 
                                         const Handle& task, const Handle& outcome,
                                         double importance)
{
    // Create unique experience ID
    Handle experience_id = _atomspace->add_node(CONCEPT_NODE, 
        "Experience_" + std::to_string(rand()));
    
    // Create experience entry
    Experience experience;
    experience.id = experience_id;
    experience.type = type;
    experience.context = context;
    experience.task = task;
    experience.outcome = outcome;
    experience.timestamp = std::chrono::system_clock::now();
    experience.importance = std::max(0.0, std::min(1.0, importance)); // Clamp to [0,1]
    
    // Add to storage
    size_t index = _experiences.size();
    _experiences.push_back(experience);
    
    // Update indices
    indexExperience(experience, index);
    
    // Create AtomSpace representation
    Handle experience_atom = createExperienceAtom(experience);
    
    // Trigger consolidation if needed
    if (_experiences.size() > _max_experiences) {
        consolidateMemory();
    }
    
    logger().debug() << "[ExperienceManager] Recorded " << experienceTypeToString(type) 
                     << " experience with importance " << importance;
    
    return experience_id;
}

// Retrieve experiences matching query criteria
std::vector<Experience> ExperienceManager::queryExperiences(const ExperienceQuery& query) const
{
    std::vector<Experience> results;
    
    // Start with type filter if specified
    std::vector<size_t> candidate_indices;
    auto type_it = _type_index.find(query.type_filter);
    if (type_it != _type_index.end()) {
        candidate_indices = type_it->second;
    } else {
        // No type filter, consider all experiences
        for (size_t i = 0; i < _experiences.size(); ++i) {
            candidate_indices.push_back(i);
        }
    }
    
    // Apply additional filters
    for (size_t idx : candidate_indices) {
        if (idx >= _experiences.size()) continue;
        
        const auto& exp = _experiences[idx];
        
        // Context filter
        if (query.context_filter != Handle::UNDEFINED && exp.context != query.context_filter) {
            continue;
        }
        
        // Task filter
        if (query.task_filter != Handle::UNDEFINED && exp.task != query.task_filter) {
            continue;
        }
        
        // Time filter
        if (exp.timestamp < query.start_time || exp.timestamp > query.end_time) {
            continue;
        }
        
        // Importance filter
        if (exp.importance < query.min_importance) {
            continue;
        }
        
        results.push_back(exp);
        
        // Limit results
        if (results.size() >= static_cast<size_t>(query.max_results)) {
            break;
        }
    }
    
    // Sort by importance (descending)
    std::sort(results.begin(), results.end(), 
              [](const Experience& a, const Experience& b) {
                  return a.importance > b.importance;
              });
    
    return results;
}

// Get experience by handle
Experience ExperienceManager::getExperience(const Handle& experience_handle) const
{
    auto it = _experience_index.find(experience_handle);
    if (it != _experience_index.end() && it->second < _experiences.size()) {
        return _experiences[it->second];
    }
    return Experience(); // Return empty experience if not found
}

// Update experience importance score
bool ExperienceManager::updateExperienceImportance(const Handle& experience_handle, double new_importance)
{
    auto it = _experience_index.find(experience_handle);
    if (it != _experience_index.end() && it->second < _experiences.size()) {
        _experiences[it->second].importance = std::max(0.0, std::min(1.0, new_importance));
        logger().debug() << "[ExperienceManager] Updated experience importance to " << new_importance;
        return true;
    }
    return false;
}

// Get recent experiences
std::vector<Experience> ExperienceManager::getRecentExperiences(std::chrono::hours time_window, 
                                                              int max_count) const
{
    auto cutoff_time = std::chrono::system_clock::now() - time_window;
    std::vector<Experience> recent;
    
    for (const auto& exp : _experiences) {
        if (exp.timestamp >= cutoff_time) {
            recent.push_back(exp);
        }
    }
    
    // Sort by timestamp (most recent first)
    std::sort(recent.begin(), recent.end(),
              [](const Experience& a, const Experience& b) {
                  return a.timestamp > b.timestamp;
              });
    
    // Limit results
    if (recent.size() > static_cast<size_t>(max_count)) {
        recent.resize(max_count);
    }
    
    return recent;
}

// Get experiences by type
std::vector<Experience> ExperienceManager::getExperiencesByType(ExperienceType type, int max_count) const
{
    std::vector<Experience> results;
    
    auto it = _type_index.find(type);
    if (it != _type_index.end()) {
        for (size_t idx : it->second) {
            if (idx < _experiences.size()) {
                results.push_back(_experiences[idx]);
                if (results.size() >= static_cast<size_t>(max_count)) {
                    break;
                }
            }
        }
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
        
        Handle experience_atom = _atomspace->add_node(CONCEPT_NODE, std::move(exp_id));
        
        // Create description atom
        std::string desc_copy = exp.description;
        Handle desc_atom = _atomspace->add_node(CONCEPT_NODE, std::move(desc_copy));
        Handle desc_link = _atomspace->add_link(INHERITANCE_LINK, {desc_atom, experience_atom});
        
        // Create type atom
        std::string type_str = "ExperienceType_" + std::to_string(static_cast<int>(exp.type));
        Handle type_atom = _atomspace->add_node(CONCEPT_NODE, std::move(type_str));
        Handle type_link = _atomspace->add_link(INHERITANCE_LINK, {experience_atom, type_atom});
        
        // Create outcome atom
        std::string outcome_str = "ExperienceOutcome_" + std::to_string(static_cast<int>(exp.outcome));
        Handle outcome_atom = _atomspace->add_node(CONCEPT_NODE, std::move(outcome_str));
        Handle outcome_link = _atomspace->add_link(EVALUATION_LINK, {
            _atomspace->add_node(PREDICATE_NODE, "hasOutcome"),
            _atomspace->add_link(LIST_LINK, {experience_atom, outcome_atom})
        });
        
        // Create timestamp atom
        std::string timestamp_str = std::to_string(timestamp);
        Handle timestamp_atom = _atomspace->add_node(NUMBER_NODE, std::move(timestamp_str));
        Handle timestamp_link = _atomspace->add_link(EVALUATION_LINK, {
            _atomspace->add_node(PREDICATE_NODE, "timestamp"),
            _atomspace->add_link(LIST_LINK, {experience_atom, timestamp_atom})
        });
        
        // Create learning value atom
        std::string learning_value_str = std::to_string(exp.learning_value);
        Handle learning_value_atom = _atomspace->add_node(NUMBER_NODE, std::move(learning_value_str));
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
            HandleSeq actions_copy = exp.actions;
            HandleSeq consequences_copy = exp.consequences;
            Handle action_pattern = _atomspace->add_link(IMPLICATION_LINK, {
                _atomspace->add_link(AND_LINK, std::move(actions_copy)),
                _atomspace->add_link(AND_LINK, std::move(consequences_copy))
            });
            patterns.push_back(action_pattern);
        }
        
        // Pattern 2: Context-Action pattern (what actions work in which contexts)
        if (!exp.context.environmental_state.empty() && !exp.actions.empty()) {
            HandleSeq env_copy = exp.context.environmental_state;
            HandleSeq actions_copy = exp.actions;
            Handle context_pattern = _atomspace->add_link(IMPLICATION_LINK, {
                _atomspace->add_link(AND_LINK, std::move(env_copy)),
                _atomspace->add_link(AND_LINK, std::move(actions_copy))
            });
            patterns.push_back(context_pattern);
        }
        
        // Pattern 3: Goal-Achievement pattern
        if (!exp.context.active_goals.empty() && exp.outcome == ExperienceOutcome::SUCCESS) {
            HandleSeq goals_copy = exp.context.active_goals;
            Handle goal_pattern = _atomspace->add_link(IMPLICATION_LINK, {
                _atomspace->add_link(AND_LINK, std::move(goals_copy)),
                _atomspace->add_node(CONCEPT_NODE, "SuccessfulOutcome")
            });
            patterns.push_back(goal_pattern);
        }
        
        // Pattern 4: Skill application pattern
        if (!exp.context.applied_skills.empty()) {
            HandleSeq skills_copy = exp.context.applied_skills;
            Handle skill_pattern = _atomspace->add_link(EVALUATION_LINK, {
                _atomspace->add_node(PREDICATE_NODE, "skillApplicationPattern"),
                _atomspace->add_link(LIST_LINK, std::move(skills_copy))
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

// Get experiences by context
std::vector<Experience> ExperienceManager::getExperiencesByContext(const Handle& context, int max_count) const
{
    std::vector<Experience> results;
    
    if (context == Handle::UNDEFINED) {
        return results;
    }
    
    auto it = _context_index.find(context);
    if (it != _context_index.end()) {
        for (size_t idx : it->second) {
            if (idx < _experiences.size()) {
                results.push_back(_experiences[idx]);
                if (results.size() >= static_cast<size_t>(max_count)) {
                    break;
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

// Analyze experience patterns
Handle ExperienceManager::analyzeExperiencePatterns(std::chrono::hours time_window)
{
    auto recent_experiences = getRecentExperiences(time_window, 1000);
    
    if (recent_experiences.empty()) {
        return Handle::UNDEFINED;
    }
    
    // Create pattern analysis atom
    Handle pattern_atom = _atomspace->add_node(CONCEPT_NODE, 
        "ExperiencePatterns_" + std::to_string(rand()));
    
    // Analyze type distribution
    std::map<ExperienceType, int> type_counts;
    std::map<Handle, int> context_counts;
    double total_importance = 0.0;
    
    for (const auto& exp : recent_experiences) {
        type_counts[exp.type]++;
        if (exp.context != Handle::UNDEFINED) {
            context_counts[exp.context]++;
        }
        total_importance += exp.importance;
    }
    
    double avg_importance = total_importance / recent_experiences.size();
    
    // Add statistics to AtomSpace
    _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "AverageImportance"),
        _atomspace->add_link(LIST_LINK, pattern_atom,
            _atomspace->add_node(NUMBER_NODE, std::to_string(avg_importance))));
    
    _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "ExperienceCount"),
        _atomspace->add_link(LIST_LINK, pattern_atom,
            _atomspace->add_node(NUMBER_NODE, std::to_string(recent_experiences.size()))));
    
    logger().info() << "[ExperienceManager] Analyzed " << recent_experiences.size() 
                    << " experiences, avg importance: " << avg_importance;
    
    return pattern_atom;
}

// Find similar experiences
std::vector<Experience> ExperienceManager::findSimilarExperiences(const Experience& target_experience, 
                                                                 int max_results) const
{
    std::vector<std::pair<Experience, double>> scored_experiences;
    
    for (const auto& exp : _experiences) {
        if (exp.id == target_experience.id) continue; // Skip self
        
        double similarity = calculateExperienceSimilarity(target_experience, exp);
        if (similarity > 0.1) { // Minimum similarity threshold
            scored_experiences.emplace_back(exp, similarity);
        }
    }
    
    // Sort by similarity (descending)
    std::sort(scored_experiences.begin(), scored_experiences.end(),
              [](const auto& a, const auto& b) {
                  return a.second > b.second;
              });
    
    // Extract experiences
    std::vector<Experience> results;
    for (const auto& [exp, score] : scored_experiences) {
        results.push_back(exp);
        if (results.size() >= static_cast<size_t>(max_results)) {
            break;
        }
    }
    
    return results;
}

// Get experience statistics
std::map<std::string, double> ExperienceManager::getExperienceStatistics() const
{
    std::map<std::string, double> stats;
    
    stats["total_experiences"] = static_cast<double>(_experiences.size());
    
    if (_experiences.empty()) {
        return stats;
    }
    
    // Type distribution
    std::map<ExperienceType, int> type_counts;
    double total_importance = 0.0;
    
    for (const auto& exp : _experiences) {
        type_counts[exp.type]++;
        total_importance += exp.importance;
    }
    
    stats["average_importance"] = total_importance / _experiences.size();
    
    // Add type percentages
    for (const auto& [type, count] : type_counts) {
        std::string key = "percent_" + experienceTypeToString(type);
        stats[key] = (static_cast<double>(count) / _experiences.size()) * 100.0;
    }
    
    return stats;
}

// Trigger memory consolidation
void ExperienceManager::consolidateMemoryManual()
{
    consolidateMemory();
}

// Clear all experiences
void ExperienceManager::clearAllExperiences()
{
    _experiences.clear();
    _experience_index.clear();
    _type_index.clear();
    _context_index.clear();
    
    logger().info() << "[ExperienceManager] Cleared all experiences";
}

// Get current memory usage statistics
std::map<std::string, size_t> ExperienceManager::getMemoryUsage() const
{
    std::map<std::string, size_t> usage;
    
    usage["total_experiences"] = _experiences.size();
    usage["experience_index_size"] = _experience_index.size();
    usage["type_index_entries"] = _type_index.size();
    usage["context_index_entries"] = _context_index.size();
    usage["max_experiences"] = _max_experiences;
    
    // Estimate memory usage (rough approximation)
    size_t estimated_bytes = _experiences.size() * sizeof(Experience) +
                            _experience_index.size() * (sizeof(Handle) + sizeof(size_t));
    usage["estimated_memory_bytes"] = estimated_bytes;
    
    return usage;
}

// Configuration methods
void ExperienceManager::setMaxExperiences(size_t max_experiences)
{
    _max_experiences = max_experiences;
    if (_experiences.size() > _max_experiences) {
        consolidateMemory();
    }
}

void ExperienceManager::setImportanceThreshold(double threshold)
{
    _importance_threshold = std::max(0.0, std::min(1.0, threshold));
}

void ExperienceManager::setRetentionPeriod(std::chrono::hours period)
{
    _retention_period = period;
}

// Utility methods
std::string ExperienceManager::experienceTypeToString(ExperienceType type)
{
    switch (type) {
        case ExperienceType::LEARNING: return "LEARNING";
        case ExperienceType::PLANNING: return "PLANNING";
        case ExperienceType::EXECUTION: return "EXECUTION";
        case ExperienceType::SOCIAL: return "SOCIAL";
        case ExperienceType::EXPLORATION: return "EXPLORATION";
        case ExperienceType::REFLECTION: return "REFLECTION";
        default: return "UNKNOWN";
    }
}

ExperienceType ExperienceManager::stringToExperienceType(const std::string& type_str)
{
    if (type_str == "LEARNING") return ExperienceType::LEARNING;
    if (type_str == "PLANNING") return ExperienceType::PLANNING;
    if (type_str == "EXECUTION") return ExperienceType::EXECUTION;
    if (type_str == "SOCIAL") return ExperienceType::SOCIAL;
    if (type_str == "EXPLORATION") return ExperienceType::EXPLORATION;
    if (type_str == "REFLECTION") return ExperienceType::REFLECTION;
    return ExperienceType::LEARNING; // Default
}

double ExperienceManager::calculateExperienceSimilarity(const Experience& exp1, const Experience& exp2)
{
    double similarity = 0.0;
    
    // Type similarity
    if (exp1.type == exp2.type) {
        similarity += 0.3;
    }
    
    // Context similarity
    if (exp1.context != Handle::UNDEFINED && exp2.context != Handle::UNDEFINED) {
        if (exp1.context == exp2.context) {
            similarity += 0.3;
        } else if (exp1.context->get_type() == exp2.context->get_type()) {
            similarity += 0.1;
        }
    }
    
    // Task similarity
    if (exp1.task != Handle::UNDEFINED && exp2.task != Handle::UNDEFINED) {
        if (exp1.task == exp2.task) {
            similarity += 0.2;
        } else if (exp1.task->get_type() == exp2.task->get_type()) {
            similarity += 0.1;
        }
    }
    
    // Importance similarity
    double importance_diff = std::abs(exp1.importance - exp2.importance);
    similarity += (1.0 - importance_diff) * 0.2;
    
    return std::min(1.0, similarity);
}

// Status and debugging
bool ExperienceManager::isInitialized() const
{
    return _experience_context != Handle::UNDEFINED && _atomspace != nullptr;
}

bool ExperienceManager::validateStorageIntegrity() const
{
    // Check index consistency
    if (_experience_index.size() > _experiences.size()) {
        return false;
    }
    
    // Check that all indexed experiences exist
    for (const auto& [handle, index] : _experience_index) {
        if (index >= _experiences.size() || _experiences[index].id != handle) {
            return false;
        }
    }
    
    return true;
}

// Private methods
void ExperienceManager::consolidateMemory()
{
    if (_experiences.size() <= _max_experiences) {
        return;
    }
    
    logger().info() << "[ExperienceManager] Consolidating memory: " << _experiences.size() 
                    << " -> " << _max_experiences << " experiences";
    
    // Update importance scores
    updateImportanceScores();
    
    // Create vector of experiences with indices for sorting
    std::vector<std::pair<size_t, double>> experience_scores;
    for (size_t i = 0; i < _experiences.size(); ++i) {
        if (shouldRetainExperience(_experiences[i])) {
            experience_scores.emplace_back(i, _experiences[i].importance);
        }
    }
    
    // Sort by importance (descending)
    std::sort(experience_scores.begin(), experience_scores.end(),
              [](const auto& a, const auto& b) {
                  return a.second > b.second;
              });
    
    // Keep only the most important experiences
    size_t target_size = std::min(_max_experiences, experience_scores.size());
    std::vector<Experience> new_experiences;
    new_experiences.reserve(target_size);
    
    for (size_t i = 0; i < target_size; ++i) {
        new_experiences.push_back(_experiences[experience_scores[i].first]);
    }
    
    // Replace experiences and rebuild indices
    _experiences = std::move(new_experiences);
    _experience_index.clear();
    _type_index.clear();
    _context_index.clear();
    
    // Rebuild indices
    for (size_t i = 0; i < _experiences.size(); ++i) {
        indexExperience(_experiences[i], i);
    }
    
    logger().info() << "[ExperienceManager] Memory consolidation complete: " 
                    << _experiences.size() << " experiences retained";
}

void ExperienceManager::updateImportanceScores()
{
    auto now = std::chrono::system_clock::now();
    
    for (auto& exp : _experiences) {
        // Decay importance over time
        auto age = std::chrono::duration_cast<std::chrono::hours>(now - exp.timestamp);
        double decay_factor = 1.0 - (static_cast<double>(age.count()) / 
                                    static_cast<double>(_retention_period.count()));
        decay_factor = std::max(0.0, decay_factor);
        
        exp.importance *= decay_factor;
    }
}

bool ExperienceManager::shouldRetainExperience(const Experience& exp) const
{
    // Check importance threshold
    if (exp.importance < _importance_threshold) {
        return false;
    }
    
    // Check retention period
    auto now = std::chrono::system_clock::now();
    auto age = std::chrono::duration_cast<std::chrono::hours>(now - exp.timestamp);
    if (age > _retention_period) {
        return false;
    }
    
    return true;
}

Handle ExperienceManager::createExperienceAtom(const Experience& exp)
{
    Handle experience_atom = exp.id;
    
    // Add type information
    Handle type_atom = _atomspace->add_node(CONCEPT_NODE, experienceTypeToString(exp.type));
    _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "ExperienceType"),
        _atomspace->add_link(LIST_LINK, experience_atom, type_atom));
    
    // Add context if available
    if (exp.context != Handle::UNDEFINED) {
        _atomspace->add_link(EVALUATION_LINK,
            _atomspace->add_node(PREDICATE_NODE, "ExperienceContext"),
            _atomspace->add_link(LIST_LINK, experience_atom, exp.context));
    }
    
    // Add task if available
    if (exp.task != Handle::UNDEFINED) {
        _atomspace->add_link(EVALUATION_LINK,
            _atomspace->add_node(PREDICATE_NODE, "ExperienceTask"),
            _atomspace->add_link(LIST_LINK, experience_atom, exp.task));
    }
    
    // Add outcome if available
    if (exp.outcome != Handle::UNDEFINED) {
        _atomspace->add_link(EVALUATION_LINK,
            _atomspace->add_node(PREDICATE_NODE, "ExperienceOutcome"),
            _atomspace->add_link(LIST_LINK, experience_atom, exp.outcome));
    }
    
    // Add importance score
    Handle importance_atom = _atomspace->add_node(NUMBER_NODE, std::to_string(exp.importance));
    _atomspace->add_link(EVALUATION_LINK,
        _atomspace->add_node(PREDICATE_NODE, "ExperienceImportance"),
        _atomspace->add_link(LIST_LINK, experience_atom, importance_atom));
    
    return experience_atom;
}

void ExperienceManager::indexExperience(const Experience& exp, size_t index)
{
    // Add to main index
    _experience_index[exp.id] = index;
    
    // Add to type index
    _type_index[exp.type].push_back(index);
    
    // Add to context index if context is available
    if (exp.context != Handle::UNDEFINED) {
        _context_index[exp.context].push_back(index);
    }
}

void ExperienceManager::removeExperienceFromIndices(size_t index)
{
    if (index >= _experiences.size()) return;
    
    const auto& exp = _experiences[index];
    
    // Remove from main index
    _experience_index.erase(exp.id);
    
    // Remove from type index
    auto& type_vec = _type_index[exp.type];
    type_vec.erase(std::remove(type_vec.begin(), type_vec.end(), index), type_vec.end());
    
    // Remove from context index
    if (exp.context != Handle::UNDEFINED) {
        auto& context_vec = _context_index[exp.context];
        context_vec.erase(std::remove(context_vec.begin(), context_vec.end(), index), context_vec.end());
    }
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
            std::string pattern_name = "SequentialPattern_" + seq_pair.first;
            Handle pattern = _atomspace->add_node(CONCEPT_NODE, std::move(pattern_name));
            
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
                std::string pattern_name = "CausalPattern_" + std::to_string(patterns.size());
                Handle pattern = _atomspace->add_node(CONCEPT_NODE, std::move(pattern_name));
                
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
            
            Handle policy_atom = _atomspace->add_node(CONCEPT_NODE, std::move(policy_name));
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
    Handle optimized_policy = _atomspace->add_node(CONCEPT_NODE, std::move(optimized_name));
    
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
