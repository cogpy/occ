/*
 * src/ActionScheduler.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Action Scheduler Implementation
 * Temporal coordination and scheduling system for Agent-Zero actions
 * Part of AZ-ACTION-001: Implement ActionScheduler for temporal coordination
 */

#include <sstream>
#include <algorithm>
#include <ctime>

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>

#include "opencog/agentzero/ActionScheduler.h"
#include "opencog/agentzero/ActionExecutor.h"
#include "opencog/agentzero/AgentZeroCore.h"

using namespace opencog;
using namespace opencog::agentzero;

ActionScheduler::ActionScheduler(AgentZeroCore* agent_core, AtomSpacePtr atomspace)
    : _agent_core(agent_core)
    , _atomspace(atomspace)
    , _executor(nullptr)
    , _schedule_context(Handle::UNDEFINED)
    , _temporal_context(Handle::UNDEFINED)
    , _dependency_context(Handle::UNDEFINED)
    , _resource_context(Handle::UNDEFINED)
    , _max_scheduled_actions(100)
    , _scheduling_resolution(std::chrono::milliseconds(100))
    , _enable_resource_management(true)
    , _enable_dependency_checking(true)
    , _enable_temporal_reasoning(true)
    , _last_update(std::chrono::steady_clock::now())
{
    logger().info() << "[ActionScheduler] Constructor: Initializing temporal coordination system";
    initializeScheduler();
}

ActionScheduler::~ActionScheduler()
{
    logger().info() << "[ActionScheduler] Destructor: Cleaning up scheduler resources";
    
    // Clear all scheduled actions
    _scheduled_actions.clear();
    _resource_locks.clear();
    _dependency_graph.clear();
}

void ActionScheduler::initializeScheduler()
{
    logger().debug() << "[ActionScheduler] Initializing scheduler system";
    
    try {
        // Create context atoms in AtomSpace
        _schedule_context = _atomspace->add_node(CONCEPT_NODE, std::string("ScheduleContext"));
        _temporal_context = _atomspace->add_node(CONCEPT_NODE, std::string("TemporalContext"));
        _dependency_context = _atomspace->add_node(CONCEPT_NODE, std::string("DependencyContext"));
        _resource_context = _atomspace->add_node(CONCEPT_NODE, std::string("ResourceContext"));
        
        // Set initial truth values
        TruthValuePtr context_tv = SimpleTruthValue::createTV(0.8, 0.9);
        _schedule_context->setTruthValue(context_tv);
        _temporal_context->setTruthValue(context_tv);
        _dependency_context->setTruthValue(context_tv);
        _resource_context->setTruthValue(context_tv);
        
        // Initialize basic resources
        registerResource("cpu");
        registerResource("memory");
        registerResource("network");
        registerResource("storage");
        
        logger().info() << "[ActionScheduler] Scheduler system initialized successfully";
        
    } catch (const std::exception& e) {
        logger().error() << "[ActionScheduler] Failed to initialize scheduler: " << e.what();
        throw;
    }
}

ActionScheduler::ScheduleResult ActionScheduler::scheduleAction(const Handle& action_atom,
                                                               const std::chrono::steady_clock::time_point& scheduled_time,
                                                               int priority)
{
    logger().debug() << "[ActionScheduler] Scheduling action: " << action_atom << " at time: " << 
                      std::chrono::duration_cast<std::chrono::milliseconds>(scheduled_time.time_since_epoch()).count();
    
    // Check if we've reached maximum scheduled actions
    if (_scheduled_actions.size() >= static_cast<size_t>(_max_scheduled_actions)) {
        logger().warn() << "[ActionScheduler] Schedule queue is full, rejecting action";
        return ScheduleResult::QUEUE_FULL;
    }
    
    // Create scheduled action entry
    ScheduledAction scheduled;
    scheduled.action_atom = action_atom;
    scheduled.scheduled_time = scheduled_time;
    scheduled.priority = priority;
    
    // Validate scheduling constraints
    if (!validateSchedulingConstraints(scheduled)) {
        logger().warn() << "[ActionScheduler] Invalid scheduling constraints for action: " << action_atom;
        return ScheduleResult::INVALID_CONSTRAINT;
    }
    
    // Add to scheduled actions map
    _scheduled_actions[action_atom] = scheduled;
    
    // Create scheduling atom in AtomSpace
    Handle schedule_atom = createScheduleAtom(scheduled);
    if (schedule_atom != Handle::UNDEFINED) {
        TruthValuePtr schedule_tv = SimpleTruthValue::createTV(0.7, 0.8);
        schedule_atom->setTruthValue(schedule_tv);
    }
    
    recordSchedulingDecision(action_atom, ScheduleResult::SCHEDULED);
    
    logger().info() << "[ActionScheduler] Action scheduled successfully: " << action_atom;
    return ScheduleResult::SCHEDULED;
}

ActionScheduler::ScheduleResult ActionScheduler::scheduleActionAfter(const Handle& action_atom,
                                                                    int delay_ms,
                                                                    int priority)
{
    auto scheduled_time = std::chrono::steady_clock::now() + std::chrono::milliseconds(delay_ms);
    return scheduleAction(action_atom, scheduled_time, priority);
}

ActionScheduler::ScheduleResult ActionScheduler::scheduleActionBefore(const Handle& action_atom,
                                                                     const std::chrono::steady_clock::time_point& deadline,
                                                                     int priority)
{
    logger().debug() << "[ActionScheduler] Scheduling action with deadline: " << action_atom;
    
    // Schedule immediately but set deadline
    auto now = std::chrono::steady_clock::now();
    
    ScheduledAction scheduled;
    scheduled.action_atom = action_atom;
    scheduled.scheduled_time = now;
    scheduled.deadline = deadline;
    scheduled.priority = priority;
    
    if (!validateSchedulingConstraints(scheduled)) {
        return ScheduleResult::INVALID_CONSTRAINT;
    }
    
    _scheduled_actions[action_atom] = scheduled;
    
    Handle schedule_atom = createScheduleAtom(scheduled);
    if (schedule_atom != Handle::UNDEFINED) {
        TruthValuePtr schedule_tv = SimpleTruthValue::createTV(0.8, 0.9);  // Higher confidence with deadline
        schedule_atom->setTruthValue(schedule_tv);
    }
    
    recordSchedulingDecision(action_atom, ScheduleResult::SCHEDULED);
    
    logger().info() << "[ActionScheduler] Action scheduled with deadline: " << action_atom;
    return ScheduleResult::SCHEDULED;
}

ActionScheduler::ScheduleResult ActionScheduler::schedulePeriodicAction(const Handle& action_atom,
                                                                       int period_ms,
                                                                       int repeat_count,
                                                                       int priority)
{
    logger().debug() << "[ActionScheduler] Scheduling periodic action: " << action_atom 
                    << " period: " << period_ms << "ms, repeats: " << repeat_count;
    
    auto now = std::chrono::steady_clock::now();
    
    ScheduledAction scheduled;
    scheduled.action_atom = action_atom;
    scheduled.scheduled_time = now;
    scheduled.priority = priority;
    scheduled.is_periodic = true;
    scheduled.period = std::chrono::milliseconds(period_ms);
    scheduled.repeat_count = repeat_count;
    
    if (!validateSchedulingConstraints(scheduled)) {
        return ScheduleResult::INVALID_CONSTRAINT;
    }
    
    _scheduled_actions[action_atom] = scheduled;
    
    Handle schedule_atom = createScheduleAtom(scheduled);
    if (schedule_atom != Handle::UNDEFINED) {
        TruthValuePtr schedule_tv = SimpleTruthValue::createTV(0.7, 0.8);
        schedule_atom->setTruthValue(schedule_tv);
    }
    
    recordSchedulingDecision(action_atom, ScheduleResult::SCHEDULED);
    
    logger().info() << "[ActionScheduler] Periodic action scheduled: " << action_atom;
    return ScheduleResult::SCHEDULED;
}

ActionScheduler::ScheduleResult ActionScheduler::scheduleActionWithDependencies(const Handle& action_atom,
                                                                               const std::vector<Handle>& dependencies,
                                                                               int priority)
{
    logger().debug() << "[ActionScheduler] Scheduling action with dependencies: " << action_atom 
                    << " deps count: " << dependencies.size();
    
    if (!_enable_dependency_checking) {
        // Fall back to simple scheduling if dependencies disabled
        return scheduleAction(action_atom, std::chrono::steady_clock::now(), priority);
    }
    
    // Check if dependencies are valid
    if (!checkDependencies(dependencies)) {
        logger().warn() << "[ActionScheduler] Dependencies not met for action: " << action_atom;
        return ScheduleResult::DEPENDENCY_UNMET;
    }
    
    auto now = std::chrono::steady_clock::now();
    
    ScheduledAction scheduled;
    scheduled.action_atom = action_atom;
    scheduled.scheduled_time = now;
    scheduled.dependencies = dependencies;
    scheduled.priority = priority;
    
    _scheduled_actions[action_atom] = scheduled;
    updateDependencyGraph(action_atom, dependencies);
    
    Handle schedule_atom = createScheduleAtom(scheduled);
    if (schedule_atom != Handle::UNDEFINED) {
        TruthValuePtr schedule_tv = SimpleTruthValue::createTV(0.6, 0.7);  // Lower confidence due to dependencies
        schedule_atom->setTruthValue(schedule_tv);
    }
    
    recordSchedulingDecision(action_atom, ScheduleResult::SCHEDULED);
    
    logger().info() << "[ActionScheduler] Action with dependencies scheduled: " << action_atom;
    return ScheduleResult::SCHEDULED;
}

ActionScheduler::ScheduleResult ActionScheduler::scheduleActionWithResources(const Handle& action_atom,
                                                                            const std::vector<std::string>& resources,
                                                                            int priority)
{
    logger().debug() << "[ActionScheduler] Scheduling action with resources: " << action_atom 
                    << " resources count: " << resources.size();
    
    if (!_enable_resource_management) {
        // Fall back to simple scheduling if resource management disabled
        return scheduleAction(action_atom, std::chrono::steady_clock::now(), priority);
    }
    
    // Check resource availability
    if (!checkResourceAvailability(resources)) {
        logger().warn() << "[ActionScheduler] Required resources not available for action: " << action_atom;
        return ScheduleResult::CONFLICT;
    }
    
    auto now = std::chrono::steady_clock::now();
    
    ScheduledAction scheduled;
    scheduled.action_atom = action_atom;
    scheduled.scheduled_time = now;
    scheduled.required_resources = resources;
    scheduled.priority = priority;
    
    _scheduled_actions[action_atom] = scheduled;
    
    Handle schedule_atom = createScheduleAtom(scheduled);
    if (schedule_atom != Handle::UNDEFINED) {
        TruthValuePtr schedule_tv = SimpleTruthValue::createTV(0.8, 0.9);
        schedule_atom->setTruthValue(schedule_tv);
    }
    
    recordSchedulingDecision(action_atom, ScheduleResult::SCHEDULED);
    
    logger().info() << "[ActionScheduler] Action with resources scheduled: " << action_atom;
    return ScheduleResult::SCHEDULED;
}

bool ActionScheduler::cancelScheduledAction(const Handle& action_atom)
{
    logger().debug() << "[ActionScheduler] Cancelling scheduled action: " << action_atom;
    
    auto iter = _scheduled_actions.find(action_atom);
    if (iter == _scheduled_actions.end()) {
        logger().warn() << "[ActionScheduler] Action not found for cancellation: " << action_atom;
        return false;
    }
    
    const ScheduledAction& scheduled = iter->second;
    
    // Release any locked resources
    updateResourceLocks(action_atom, false);
    
    // Remove from dependency graph
    _dependency_graph.erase(action_atom);
    
    // Remove from scheduled actions
    _scheduled_actions.erase(iter);
    
    // Record cancellation in AtomSpace
    try {
        Handle cancelled_node = _atomspace->add_node(CONCEPT_NODE, std::string("CancelledAction"));
        HandleSeq cancel_link;
        cancel_link.push_back(action_atom);
        cancel_link.push_back(cancelled_node);
        Handle cancel_atom = _atomspace->add_link(EVALUATION_LINK, std::move(cancel_link));
        
        TruthValuePtr cancel_tv = SimpleTruthValue::createTV(0.0, 0.9);  // Low strength indicates cancellation
        cancel_atom->setTruthValue(cancel_tv);
        
    } catch (const std::exception& e) {
        logger().warn() << "[ActionScheduler] Failed to record cancellation: " << e.what();
    }
    
    logger().info() << "[ActionScheduler] Scheduled action cancelled: " << action_atom;
    return true;
}

ActionScheduler::ScheduleResult ActionScheduler::rescheduleAction(const Handle& action_atom,
                                                                 const std::chrono::steady_clock::time_point& new_time)
{
    logger().debug() << "[ActionScheduler] Rescheduling action: " << action_atom;
    
    auto iter = _scheduled_actions.find(action_atom);
    if (iter == _scheduled_actions.end()) {
        logger().warn() << "[ActionScheduler] Action not found for rescheduling: " << action_atom;
        return ScheduleResult::INVALID_CONSTRAINT;
    }
    
    // Update scheduled time
    iter->second.scheduled_time = new_time;
    
    // Validate new constraints
    if (!validateSchedulingConstraints(iter->second)) {
        logger().warn() << "[ActionScheduler] Invalid constraints for rescheduled action: " << action_atom;
        return ScheduleResult::INVALID_CONSTRAINT;
    }
    
    recordSchedulingDecision(action_atom, ScheduleResult::SCHEDULED);
    
    logger().info() << "[ActionScheduler] Action rescheduled successfully: " << action_atom;
    return ScheduleResult::SCHEDULED;
}

int ActionScheduler::processScheduleQueue()
{
    auto now = std::chrono::steady_clock::now();
    int actions_dispatched = 0;
    
    // Get actions that are ready to execute
    std::vector<ScheduledAction> ready_actions = getReadyActions();
    
    // Sort by priority (higher priority first)
    std::sort(ready_actions.begin(), ready_actions.end(), 
              [](const ScheduledAction& a, const ScheduledAction& b) {
                  return a.priority > b.priority;
              });
    
    // Dispatch ready actions
    for (const ScheduledAction& action : ready_actions) {
        if (!_executor) {
            logger().warn() << "[ActionScheduler] No executor available for action dispatch";
            break;
        }
        
        logger().debug() << "[ActionScheduler] Dispatching ready action: " << action.action_atom;
        
        // Acquire resources if needed
        if (!action.required_resources.empty()) {
            updateResourceLocks(action.action_atom, true);
        }
        
        // Execute the action through the executor
        bool dispatched = _executor->executeAction(action.action_atom, ActionExecutor::Priority::MEDIUM);
        
        if (dispatched) {
            actions_dispatched++;
            
            // Handle periodic actions
            if (action.is_periodic && action.repeat_count != 0) {
                ScheduledAction next_iteration = action;
                next_iteration.scheduled_time = now + action.period;
                if (next_iteration.repeat_count > 0) {
                    next_iteration.repeat_count--;
                }
                
                if (next_iteration.repeat_count != 0) {
                    _scheduled_actions[action.action_atom] = next_iteration;
                } else {
                    _scheduled_actions.erase(action.action_atom);
                }
            } else {
                // Remove non-periodic actions after dispatch
                _scheduled_actions.erase(action.action_atom);
            }
        } else {
            // Release resources if dispatch failed
            if (!action.required_resources.empty()) {
                updateResourceLocks(action.action_atom, false);
            }
        }
    }
    
    return actions_dispatched;
}

int ActionScheduler::updateScheduler()
{
    auto now = std::chrono::steady_clock::now();
    int status_changes = 0;
    
    // Check for expired deadlines
    for (auto iter = _scheduled_actions.begin(); iter != _scheduled_actions.end(); ) {
        const ScheduledAction& action = iter->second;
        
        if (action.deadline != std::chrono::steady_clock::time_point{} && now > action.deadline) {
            logger().warn() << "[ActionScheduler] Action missed deadline: " << action.action_atom;
            
            // Record deadline miss in AtomSpace
            try {
                Handle deadline_miss = _atomspace->add_node(CONCEPT_NODE, std::string("DeadlineMissed"));
                HandleSeq miss_link;
                miss_link.push_back(action.action_atom);
                miss_link.push_back(deadline_miss);
                Handle miss_atom = _atomspace->add_link(EVALUATION_LINK, std::move(miss_link));
                
                TruthValuePtr miss_tv = SimpleTruthValue::createTV(0.0, 0.9);
                miss_atom->setTruthValue(miss_tv);
                
            } catch (const std::exception& e) {
                logger().warn() << "[ActionScheduler] Failed to record deadline miss: " << e.what();
            }
            
            iter = _scheduled_actions.erase(iter);
            status_changes++;
        } else {
            ++iter;
        }
    }
    
    _last_update = now;
    return status_changes;
}

std::vector<ActionScheduler::ScheduledAction> ActionScheduler::getScheduledActions() const
{
    std::vector<ScheduledAction> actions;
    for (const auto& pair : _scheduled_actions) {
        actions.push_back(pair.second);
    }
    return actions;
}

std::chrono::steady_clock::time_point ActionScheduler::getNextActionTime() const
{
    if (_scheduled_actions.empty()) {
        return std::chrono::steady_clock::time_point::max();
    }
    
    auto earliest = std::chrono::steady_clock::time_point::max();
    for (const auto& pair : _scheduled_actions) {
        if (pair.second.scheduled_time < earliest) {
            earliest = pair.second.scheduled_time;
        }
    }
    
    return earliest;
}

bool ActionScheduler::registerResource(const std::string& resource_name)
{
    logger().debug() << "[ActionScheduler] Registering resource: " << resource_name;
    
    auto iter = std::find(_available_resources.begin(), _available_resources.end(), resource_name);
    if (iter != _available_resources.end()) {
        logger().debug() << "[ActionScheduler] Resource already registered: " << resource_name;
        return true;  // Already registered
    }
    
    _available_resources.push_back(resource_name);
    
    // Create resource atom in AtomSpace
    try {
        Handle resource_node = _atomspace->add_node(CONCEPT_NODE, std::string(resource_name));
        HandleSeq resource_link;
        resource_link.push_back(resource_node);
        resource_link.push_back(_resource_context);
        Handle resource_atom = _atomspace->add_link(EVALUATION_LINK, std::move(resource_link));
        
        TruthValuePtr resource_tv = SimpleTruthValue::createTV(1.0, 0.9);  // Available
        resource_atom->setTruthValue(resource_tv);
        
    } catch (const std::exception& e) {
        logger().warn() << "[ActionScheduler] Failed to create resource atom: " << e.what();
    }
    
    logger().info() << "[ActionScheduler] Resource registered: " << resource_name;
    return true;
}

bool ActionScheduler::unregisterResource(const std::string& resource_name)
{
    logger().debug() << "[ActionScheduler] Unregistering resource: " << resource_name;
    
    auto iter = std::find(_available_resources.begin(), _available_resources.end(), resource_name);
    if (iter == _available_resources.end()) {
        logger().warn() << "[ActionScheduler] Resource not found: " << resource_name;
        return false;
    }
    
    // Check if resource is currently locked
    if (_resource_locks.find(resource_name) != _resource_locks.end()) {
        logger().warn() << "[ActionScheduler] Cannot unregister locked resource: " << resource_name;
        return false;
    }
    
    _available_resources.erase(iter);
    
    logger().info() << "[ActionScheduler] Resource unregistered: " << resource_name;
    return true;
}

bool ActionScheduler::isResourceAvailable(const std::string& resource_name) const
{
    // Check if resource exists
    auto resource_iter = std::find(_available_resources.begin(), _available_resources.end(), resource_name);
    if (resource_iter == _available_resources.end()) {
        return false;
    }
    
    // Check if resource is locked
    auto lock_iter = _resource_locks.find(resource_name);
    return lock_iter == _resource_locks.end();
}

std::vector<std::string> ActionScheduler::getAvailableResources() const
{
    std::vector<std::string> available;
    for (const std::string& resource : _available_resources) {
        if (isResourceAvailable(resource)) {
            available.push_back(resource);
        }
    }
    return available;
}

void ActionScheduler::configureFeatures(bool resources, bool dependencies, bool temporal)
{
    _enable_resource_management = resources;
    _enable_dependency_checking = dependencies;
    _enable_temporal_reasoning = temporal;
    
    logger().info() << "[ActionScheduler] Features configured - Resources: " << resources 
                    << ", Dependencies: " << dependencies << ", Temporal: " << temporal;
}

std::string ActionScheduler::getStatusInfo() const
{
    std::ostringstream oss;
    oss << "{\n";
    oss << "  \"scheduled_actions\": " << _scheduled_actions.size() << ",\n";
    oss << "  \"max_scheduled_actions\": " << _max_scheduled_actions << ",\n";
    oss << "  \"available_resources\": " << _available_resources.size() << ",\n";
    oss << "  \"locked_resources\": " << _resource_locks.size() << ",\n";
    oss << "  \"dependency_entries\": " << _dependency_graph.size() << ",\n";
    oss << "  \"resource_management_enabled\": " << (_enable_resource_management ? "true" : "false") << ",\n";
    oss << "  \"dependency_checking_enabled\": " << (_enable_dependency_checking ? "true" : "false") << ",\n";
    oss << "  \"temporal_reasoning_enabled\": " << (_enable_temporal_reasoning ? "true" : "false") << "\n";
    oss << "}";
    return oss.str();
}

// Private helper methods

bool ActionScheduler::validateSchedulingConstraints(const ScheduledAction& action)
{
    // Basic validation
    if (action.action_atom == Handle::UNDEFINED) {
        return false;
    }
    
    // Check deadline constraints
    if (action.deadline != std::chrono::steady_clock::time_point{} && 
        action.scheduled_time > action.deadline) {
        return false;
    }
    
    // Check periodic constraints
    if (action.is_periodic && action.period.count() <= 0) {
        return false;
    }
    
    return true;
}

bool ActionScheduler::checkResourceAvailability(const std::vector<std::string>& resources)
{
    for (const std::string& resource : resources) {
        if (!isResourceAvailable(resource)) {
            return false;
        }
    }
    return true;
}

bool ActionScheduler::checkDependencies(const std::vector<Handle>& dependencies)
{
    if (!_enable_dependency_checking) {
        return true;  // Dependencies disabled, always pass
    }
    
    // For now, assume all dependencies are met
    // In a real implementation, this would check the execution status of dependent actions
    // through the executor or AtomSpace queries
    
    for (const Handle& dep : dependencies) {
        if (dep == Handle::UNDEFINED) {
            return false;
        }
        // Additional dependency checking logic would go here
    }
    
    return true;
}

std::vector<ActionScheduler::ScheduledAction> ActionScheduler::getReadyActions()
{
    std::vector<ScheduledAction> ready;
    auto now = std::chrono::steady_clock::now();
    
    for (const auto& pair : _scheduled_actions) {
        const ScheduledAction& action = pair.second;
        
        // Check if time has come
        if (action.scheduled_time > now) {
            continue;
        }
        
        // Check dependencies
        if (!checkDependencies(action.dependencies)) {
            continue;
        }
        
        // Check resources
        if (!checkResourceAvailability(action.required_resources)) {
            continue;
        }
        
        ready.push_back(action);
    }
    
    return ready;
}

void ActionScheduler::updateResourceLocks(const Handle& action_atom, bool acquire)
{
    auto iter = _scheduled_actions.find(action_atom);
    if (iter == _scheduled_actions.end()) {
        return;
    }
    
    const std::vector<std::string>& resources = iter->second.required_resources;
    
    if (acquire) {
        // Acquire locks
        for (const std::string& resource : resources) {
            _resource_locks[resource] = action_atom;
            logger().debug() << "[ActionScheduler] Resource locked: " << resource << " by " << action_atom;
        }
    } else {
        // Release locks
        for (const std::string& resource : resources) {
            auto lock_iter = _resource_locks.find(resource);
            if (lock_iter != _resource_locks.end() && lock_iter->second == action_atom) {
                _resource_locks.erase(lock_iter);
                logger().debug() << "[ActionScheduler] Resource released: " << resource << " by " << action_atom;
            }
        }
    }
}

void ActionScheduler::updateDependencyGraph(const Handle& action_atom, const std::vector<Handle>& deps)
{
    _dependency_graph[action_atom] = deps;
}

Handle ActionScheduler::createScheduleAtom(const ScheduledAction& action)
{
    try {
        HandleSeq schedule_content;
        schedule_content.push_back(action.action_atom);
        schedule_content.push_back(_schedule_context);
        
        Handle schedule_atom = _atomspace->add_link(EVALUATION_LINK, std::move(schedule_content));
        return schedule_atom;
        
    } catch (const std::exception& e) {
        logger().warn() << "[ActionScheduler] Failed to create schedule atom: " << e.what();
        return Handle::UNDEFINED;
    }
}

void ActionScheduler::recordSchedulingDecision(const Handle& action_atom, ScheduleResult result)
{
    try {
        std::string result_name = "ScheduleResult" + std::to_string(static_cast<int>(result));
        Handle result_node = _atomspace->add_node(CONCEPT_NODE, std::string(result_name));
        
        HandleSeq decision_link;
        decision_link.push_back(action_atom);
        decision_link.push_back(result_node);
        Handle decision_atom = _atomspace->add_link(EVALUATION_LINK, std::move(decision_link));
        
        double strength = (result == ScheduleResult::SCHEDULED) ? 1.0 : 0.0;
        TruthValuePtr decision_tv = SimpleTruthValue::createTV(strength, 0.9);
        decision_atom->setTruthValue(decision_tv);
        
    } catch (const std::exception& e) {
        logger().warn() << "[ActionScheduler] Failed to record scheduling decision: " << e.what();
    }
}