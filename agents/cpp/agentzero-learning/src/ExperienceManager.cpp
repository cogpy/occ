/*
 * opencog/agentzero/ExperienceManager.cpp
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