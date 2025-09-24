/**
 * ExperienceManager.cpp - Implementation of Experience Memory Management
 * 
 * Part of AZ-LEARN-003: MOSES Policy Optimization Integration
 * Copyright (C) 2024 OpenCog Foundation
 */

#include <agentzero/learning/ExperienceManager.h>
#include <agentzero/learning/LearningUtils.h>

#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/StringValue.h>

#include <algorithm>
#include <numeric>

namespace opencog {
namespace agentzero {
namespace learning {

ExperienceManager::ExperienceManager(AtomSpacePtr atomspace, const LearningConfig& config)
    : atomspace_(atomspace), config_(config), total_experiences_added_(0), total_reward_accumulated_(0.0) {
    
    if (!atomspace_) {
        throw LearningException("AtomSpace cannot be null");
    }
    
    logger().info("ExperienceManager: Initializing with buffer size %zu", config_.experience_buffer_size);
    
    initializeAtomSpaceStructures();
    
    logger().info("ExperienceManager: Initialization complete");
}

ExperienceManager::~ExperienceManager() {
    logger().info("ExperienceManager: Destroyed with %zu total experiences", 
                  total_experiences_added_.load());
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
    
    logger().debug("ExperienceManager: AtomSpace structures initialized");
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