/*
 * opencog/agentzero/ActionScheduler.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * ActionScheduler Implementation
 * Temporal coordination of actions within Agent-Zero cognitive architecture
 */

#include "opencog/agentzero/ActionScheduler.h"
#include "opencog/agentzero/AgentZeroCore.h"

#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atomspace/AtomSpace.h>

#include <sstream>
#include <iomanip>

using namespace opencog;
using namespace opencog::agentzero;

ActionScheduler::ActionScheduler(AgentZeroCore* agent_core, AtomSpacePtr atomspace)
    : _agent_core(agent_core)
    , _atomspace(atomspace)
    , _enabled(true)
    , _execution_window(std::chrono::milliseconds(1000))  // 1 second window
    , _default_action_duration(std::chrono::milliseconds(100))  // 100ms default
    , _scheduler_context(Handle::UNDEFINED)
    , _active_actions_context(Handle::UNDEFINED)
    , _temporal_context(Handle::UNDEFINED)
    , _max_concurrent_actions(3)
    , _use_priority_scheduling(true)
    , _enable_temporal_constraints(true)
    , _action_completion_threshold(0.8)
    , _actions_scheduled(0)
    , _actions_executed(0)
    , _actions_completed(0)
    , _actions_failed(0)
{
    logger().info() << "[ActionScheduler] Initializing ActionScheduler";
    initializeSchedulerContext();
    _last_execution_time = std::chrono::steady_clock::now();
}

ActionScheduler::~ActionScheduler()
{
    logger().info() << "[ActionScheduler] Shutting down ActionScheduler";
    cancelAllActions();
}

void ActionScheduler::initializeSchedulerContext()
{
    if (!_atomspace) {
        logger().error() << "[ActionScheduler] AtomSpace is null, cannot initialize context";
        return;
    }
    
    try {
        // Create scheduler context atoms
        _scheduler_context = _atomspace->add_node(CONCEPT_NODE, "ActionSchedulerContext");
        _active_actions_context = _atomspace->add_node(CONCEPT_NODE, "ActiveActionsContext");
        _temporal_context = _atomspace->add_node(CONCEPT_NODE, "TemporalCoordinationContext");
        
        // Set initial truth values
        TruthValuePtr scheduler_tv = SimpleTruthValue::createTV(0.9, 0.9);
        _scheduler_context->setTruthValue(scheduler_tv);
        _active_actions_context->setTruthValue(scheduler_tv);
        _temporal_context->setTruthValue(scheduler_tv);
        
        // Create relationship between contexts
        HandleSeq context_link;
        context_link.push_back(_scheduler_context);
        context_link.push_back(_active_actions_context);
        context_link.push_back(_temporal_context);
        _atomspace->add_link(LIST_LINK, std::move(context_link));
        
        logger().debug() << "[ActionScheduler] Scheduler context initialized";
        
    } catch (const std::exception& e) {
        logger().error() << "[ActionScheduler] Failed to initialize scheduler context: " << e.what();
    }
}

bool ActionScheduler::scheduleAction(Handle action_atom, 
                                   std::chrono::steady_clock::time_point execution_time,
                                   std::chrono::milliseconds duration,
                                   int priority,
                                   Handle context_atom)
{
    if (!_enabled.load()) {
        logger().debug() << "[ActionScheduler] Scheduler disabled, action not scheduled";
        return false;
    }
    
    if (!action_atom || action_atom == Handle::UNDEFINED) {
        logger().warn() << "[ActionScheduler] Invalid action atom provided";
        return false;
    }
    
    try {
        // Create action item
        ActionItem action(action_atom, execution_time, duration, priority, context_atom);
        
        // Add to scheduled actions queue
        _scheduled_actions.push(action);
        _actions_scheduled++;
        
        // Create AtomSpace representation of scheduled action
        createActionStatusAtom(action, "scheduled");
        
        logger().debug() << "[ActionScheduler] Action " << action.action_id 
                        << " scheduled for execution with priority " << priority;
        
        return true;
        
    } catch (const std::exception& e) {
        logger().error() << "[ActionScheduler] Failed to schedule action: " << e.what();
        return false;
    }
}

bool ActionScheduler::scheduleImmediateAction(Handle action_atom,
                                            std::chrono::milliseconds duration,
                                            int priority,
                                            Handle context_atom)
{
    auto immediate_time = std::chrono::steady_clock::now();
    return scheduleAction(action_atom, immediate_time, duration, priority, context_atom);
}

bool ActionScheduler::processScheduledActions()
{
    if (!_enabled.load()) {
        return true;  // Successfully "processed" (by doing nothing)
    }
    
    try {
        auto current_time = std::chrono::steady_clock::now();
        std::vector<ActionItem> ready_actions;
        
        // Find actions ready for execution
        while (!_scheduled_actions.empty()) {
            const ActionItem& next_action = _scheduled_actions.top();
            
            // Check if action is ready to execute
            if (next_action.scheduled_time <= current_time + _execution_window) {
                if (canExecuteAction(next_action)) {
                    ready_actions.push_back(next_action);
                    _scheduled_actions.pop();
                } else {
                    break;  // Cannot execute due to constraints
                }
            } else {
                break;  // No more ready actions
            }
        }
        
        // Execute ready actions
        bool all_executed = true;
        for (const auto& action : ready_actions) {
            if (!executeAction(action)) {
                all_executed = false;
            }
        }
        
        // Cleanup completed actions
        cleanupCompletedActions();
        
        _last_execution_time = current_time;
        return all_executed;
        
    } catch (const std::exception& e) {
        logger().error() << "[ActionScheduler] Error processing scheduled actions: " << e.what();
        return false;
    }
}

bool ActionScheduler::canExecuteAction(const ActionItem& action) const
{
    // Check concurrent action limit
    if (_executing_actions.size() >= _max_concurrent_actions) {
        return false;
    }
    
    // Check if action is already executing
    if (_executing_actions.find(action.action_id) != _executing_actions.end()) {
        return false;
    }
    
    // Check temporal constraints if enabled
    if (_enable_temporal_constraints) {
        auto current_time = std::chrono::steady_clock::now();
        if (action.scheduled_time > current_time + _execution_window) {
            return false;  // Too early
        }
    }
    
    return true;
}

bool ActionScheduler::executeAction(const ActionItem& action)
{
    try {
        logger().debug() << "[ActionScheduler] Executing action " << action.action_id;
        
        // Add to executing actions
        _executing_actions[action.action_id] = action;
        
        // Update action status in AtomSpace
        updateActionStatus(action.action_id, "executing");
        
        // Basic action execution - in a full implementation this would:
        // - Execute the actual action logic
        // - Monitor execution progress
        // - Handle execution failures
        // - Interface with external systems
        
        // For now, simulate execution by updating AtomSpace
        if (action.context_atom != Handle::UNDEFINED) {
            HandleSeq execution_link;
            execution_link.push_back(action.action_atom);
            execution_link.push_back(action.context_atom);
            execution_link.push_back(_temporal_context);
            Handle execution_atom = _atomspace->add_link(EVALUATION_LINK, std::move(execution_link));
            
            TruthValuePtr execution_tv = SimpleTruthValue::createTV(0.8, 0.9);
            execution_atom->setTruthValue(execution_tv);
        }
        
        // Mark as completed (in a real system, this would be done asynchronously)
        _completed_actions[action.action_id] = action.action_atom;
        updateActionStatus(action.action_id, "completed");
        
        _actions_executed++;
        _actions_completed++;
        
        logger().debug() << "[ActionScheduler] Action " << action.action_id << " completed successfully";
        return true;
        
    } catch (const std::exception& e) {
        logger().error() << "[ActionScheduler] Failed to execute action " << action.action_id 
                        << ": " << e.what();
        
        updateActionStatus(action.action_id, "failed");
        _actions_failed++;
        return false;
    }
}

void ActionScheduler::updateActionStatus(const std::string& action_id, const std::string& status)
{
    try {
        if (!_atomspace) return;
        
        Handle status_node = _atomspace->add_node(PREDICATE_NODE, "ActionStatus");
        Handle status_value = _atomspace->add_node(CONCEPT_NODE, std::string(status));
        Handle action_id_node = _atomspace->add_node(CONCEPT_NODE, std::string(action_id));
        
        HandleSeq status_link;
        status_link.push_back(status_node);
        status_link.push_back(action_id_node);
        status_link.push_back(status_value);
        
        Handle status_eval = _atomspace->add_link(EVALUATION_LINK, std::move(status_link));
        TruthValuePtr status_tv = SimpleTruthValue::createTV(0.9, 0.95);
        status_eval->setTruthValue(status_tv);
        
    } catch (const std::exception& e) {
        logger().error() << "[ActionScheduler] Failed to update action status: " << e.what();
    }
}

void ActionScheduler::cleanupCompletedActions()
{
    // Remove completed actions from executing map
    for (auto it = _executing_actions.begin(); it != _executing_actions.end();) {
        const std::string& action_id = it->first;
        if (_completed_actions.find(action_id) != _completed_actions.end()) {
            it = _executing_actions.erase(it);
        } else {
            ++it;
        }
    }
}

Handle ActionScheduler::createActionStatusAtom(const ActionItem& action, const std::string& status)
{
    if (!_atomspace) return Handle::UNDEFINED;
    
    try {
        Handle status_atom = _atomspace->add_node(CONCEPT_NODE, 
                                                 "ActionStatus_" + action.action_id + "_" + status);
        
        // Link to scheduler context
        HandleSeq context_link;
        context_link.push_back(_scheduler_context);
        context_link.push_back(status_atom);
        context_link.push_back(action.action_atom);
        
        Handle context_eval = _atomspace->add_link(EVALUATION_LINK, std::move(context_link));
        TruthValuePtr context_tv = SimpleTruthValue::createTV(0.8, 0.9);
        context_eval->setTruthValue(context_tv);
        
        return status_atom;
        
    } catch (const std::exception& e) {
        logger().error() << "[ActionScheduler] Failed to create action status atom: " << e.what();
        return Handle::UNDEFINED;
    }
}

std::chrono::steady_clock::time_point ActionScheduler::calculateNextExecutionTime(const ActionItem& action) const
{
    // In a more sophisticated implementation, this could consider:
    // - Resource availability
    // - Action dependencies
    // - Temporal constraints
    // - Priority-based scheduling
    return action.scheduled_time;
}

bool ActionScheduler::cancelAction(const std::string& action_id)
{
    try {
        // Remove from executing actions if present
        auto executing_it = _executing_actions.find(action_id);
        if (executing_it != _executing_actions.end()) {
            updateActionStatus(action_id, "cancelled");
            _executing_actions.erase(executing_it);
            return true;
        }
        
        // For scheduled actions, we'd need to rebuild the queue without this action
        // This is a limitation of std::priority_queue - in a real implementation,
        // we might use a different data structure
        
        updateActionStatus(action_id, "cancelled");
        return true;
        
    } catch (const std::exception& e) {
        logger().error() << "[ActionScheduler] Failed to cancel action " << action_id 
                        << ": " << e.what();
        return false;
    }
}

void ActionScheduler::cancelAllActions()
{
    try {
        // Clear scheduled actions
        while (!_scheduled_actions.empty()) {
            _scheduled_actions.pop();
        }
        
        // Cancel executing actions
        for (const auto& pair : _executing_actions) {
            updateActionStatus(pair.first, "cancelled");
        }
        _executing_actions.clear();
        
        // Clear completed actions
        _completed_actions.clear();
        
        logger().info() << "[ActionScheduler] All actions cancelled";
        
    } catch (const std::exception& e) {
        logger().error() << "[ActionScheduler] Error cancelling all actions: " << e.what();
    }
}

bool ActionScheduler::isActionExecuting(const std::string& action_id) const
{
    return _executing_actions.find(action_id) != _executing_actions.end();
}

void ActionScheduler::configureTemporalParameters(std::chrono::milliseconds execution_window,
                                                 std::chrono::milliseconds default_duration)
{
    _execution_window = execution_window;
    _default_action_duration = default_duration;
    
    logger().debug() << "[ActionScheduler] Temporal parameters configured - window: " 
                    << execution_window.count() << "ms, default duration: " 
                    << default_duration.count() << "ms";
}

void ActionScheduler::configureExecution(size_t max_concurrent, 
                                        bool use_priority,
                                        bool enable_temporal)
{
    _max_concurrent_actions = max_concurrent;
    _use_priority_scheduling = use_priority;
    _enable_temporal_constraints = enable_temporal;
    
    logger().debug() << "[ActionScheduler] Execution configured - max concurrent: " 
                    << max_concurrent << ", priority: " << use_priority 
                    << ", temporal: " << enable_temporal;
}

std::string ActionScheduler::getSchedulingStatistics() const
{
    std::ostringstream stats;
    stats << "{"
          << "\"actions_scheduled\": " << _actions_scheduled.load() << ","
          << "\"actions_executed\": " << _actions_executed.load() << ","
          << "\"actions_completed\": " << _actions_completed.load() << ","
          << "\"actions_failed\": " << _actions_failed.load() << ","
          << "\"pending_actions\": " << _scheduled_actions.size() << ","
          << "\"executing_actions\": " << _executing_actions.size() << ","
          << "\"completed_actions\": " << _completed_actions.size()
          << "}";
    return stats.str();
}

std::string ActionScheduler::getStatusInfo() const
{
    std::ostringstream status;
    status << "{"
           << "\"enabled\": " << (_enabled.load() ? "true" : "false") << ","
           << "\"max_concurrent\": " << _max_concurrent_actions << ","
           << "\"use_priority\": " << (_use_priority_scheduling ? "true" : "false") << ","
           << "\"temporal_constraints\": " << (_enable_temporal_constraints ? "true" : "false") << ","
           << "\"execution_window_ms\": " << _execution_window.count() << ","
           << "\"default_duration_ms\": " << _default_action_duration.count() << ","
           << "\"statistics\": " << getSchedulingStatistics()
           << "}";
    return status.str();
}

void ActionScheduler::resetStatistics()
{
    _actions_scheduled.store(0);
    _actions_executed.store(0);
    _actions_completed.store(0);
    _actions_failed.store(0);
    
    logger().info() << "[ActionScheduler] Statistics reset";
}