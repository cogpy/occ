/**
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