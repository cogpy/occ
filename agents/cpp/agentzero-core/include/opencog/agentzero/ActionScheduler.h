/*
 * opencog/agentzero/ActionScheduler.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * ActionScheduler for Temporal Coordination
 * Manages temporal coordination of actions within Agent-Zero cognitive architecture
 * Part of the AGENT-ZERO-GENESIS project
 */

#ifndef _OPENCOG_AGENTZERO_ACTION_SCHEDULER_H
#define _OPENCOG_AGENTZERO_ACTION_SCHEDULER_H

#include <memory>
#include <queue>
#include <chrono>
#include <atomic>
#include <unordered_map>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/util/Logger.h>

namespace opencog {
namespace agentzero {

// Forward declarations
class AgentZeroCore;
class CognitiveLoop;

/**
 * ActionItem - Represents a scheduled action with temporal constraints
 */
struct ActionItem {
    Handle action_atom;                          // AtomSpace representation of action
    std::chrono::steady_clock::time_point scheduled_time; // When to execute
    std::chrono::milliseconds duration;          // Expected duration
    int priority;                                // Execution priority (higher = more important)
    Handle context_atom;                         // Context for execution
    std::string action_id;                       // Unique identifier
    
    ActionItem(Handle atom, std::chrono::steady_clock::time_point time, 
               std::chrono::milliseconds dur, int prio = 0, Handle ctx = Handle::UNDEFINED)
        : action_atom(atom), scheduled_time(time), duration(dur), 
          priority(prio), context_atom(ctx), action_id(std::to_string(atom.value())) {}
    
    // Comparison for priority queue (higher priority first)
    bool operator<(const ActionItem& other) const {
        if (scheduled_time == other.scheduled_time) {
            return priority < other.priority;
        }
        return scheduled_time > other.scheduled_time;
    }
};

/**
 * ActionScheduler - Temporal coordination of actions in Agent-Zero
 *
 * This class manages the temporal scheduling and execution of actions
 * within the Agent-Zero cognitive architecture. It integrates with
 * AtomSpace for state representation and provides temporal coordination
 * capabilities for the action phase of the cognitive loop.
 *
 * Key Features:
 * - Temporal scheduling with time-based execution
 * - Priority-based action ordering
 * - AtomSpace integration for action representation
 * - Context-aware action execution
 * - Integration with CognitiveLoop action phase
 */
class ActionScheduler
{
private:
    // Core references
    AgentZeroCore* _agent_core;
    AtomSpacePtr _atomspace;
    
    // Action queue and tracking
    std::priority_queue<ActionItem> _scheduled_actions;
    std::unordered_map<std::string, ActionItem> _executing_actions;
    std::unordered_map<std::string, Handle> _completed_actions;
    
    // Temporal coordination state
    std::atomic<bool> _enabled;
    std::chrono::steady_clock::time_point _last_execution_time;
    std::chrono::milliseconds _execution_window;
    std::chrono::milliseconds _default_action_duration;
    
    // AtomSpace handles for scheduler state
    Handle _scheduler_context;
    Handle _active_actions_context;
    Handle _temporal_context;
    
    // Configuration
    size_t _max_concurrent_actions;
    bool _use_priority_scheduling;
    bool _enable_temporal_constraints;
    double _action_completion_threshold;
    
    // Statistics
    std::atomic<size_t> _actions_scheduled;
    std::atomic<size_t> _actions_executed;
    std::atomic<size_t> _actions_completed;
    std::atomic<size_t> _actions_failed;
    
    // Internal methods
    void initializeSchedulerContext();
    bool canExecuteAction(const ActionItem& action) const;
    bool executeAction(const ActionItem& action);
    void updateActionStatus(const std::string& action_id, const std::string& status);
    void cleanupCompletedActions();
    Handle createActionStatusAtom(const ActionItem& action, const std::string& status);
    std::chrono::steady_clock::time_point calculateNextExecutionTime(const ActionItem& action) const;
    
public:
    /**
     * Constructor
     * @param agent_core Pointer to the parent AgentZeroCore instance
     * @param atomspace Shared pointer to the AtomSpace
     */
    ActionScheduler(AgentZeroCore* agent_core, AtomSpacePtr atomspace);
    
    /**
     * Destructor - ensures cleanup of scheduled actions
     */
    ~ActionScheduler();
    
    // Core scheduling interface
    /**
     * Schedule an action for execution at a specific time
     * @param action_atom AtomSpace handle representing the action
     * @param execution_time When to execute the action
     * @param duration Expected duration of the action
     * @param priority Priority level (higher = more important)
     * @param context_atom Context for execution (optional)
     * @return true if successfully scheduled
     */
    bool scheduleAction(Handle action_atom, 
                       std::chrono::steady_clock::time_point execution_time,
                       std::chrono::milliseconds duration = std::chrono::milliseconds(100),
                       int priority = 0,
                       Handle context_atom = Handle::UNDEFINED);
    
    /**
     * Schedule an action for immediate execution
     * @param action_atom AtomSpace handle representing the action
     * @param duration Expected duration of the action
     * @param priority Priority level (higher = more important)
     * @param context_atom Context for execution (optional)
     * @return true if successfully scheduled
     */
    bool scheduleImmediateAction(Handle action_atom,
                               std::chrono::milliseconds duration = std::chrono::milliseconds(100),
                               int priority = 0,
                               Handle context_atom = Handle::UNDEFINED);
    
    /**
     * Process scheduled actions - called during cognitive loop action phase
     * @return true if processing completed successfully
     */
    bool processScheduledActions();
    
    /**
     * Cancel a scheduled action
     * @param action_id Unique identifier of the action
     * @return true if successfully cancelled
     */
    bool cancelAction(const std::string& action_id);
    
    /**
     * Cancel all scheduled actions
     */
    void cancelAllActions();
    
    // State queries
    /**
     * Check if scheduler is enabled
     * @return true if enabled
     */
    bool isEnabled() const { return _enabled.load(); }
    
    /**
     * Enable or disable the scheduler
     * @param enabled New enabled state
     */
    void setEnabled(bool enabled) { _enabled.store(enabled); }
    
    /**
     * Get number of pending scheduled actions
     * @return count of pending actions
     */
    size_t getPendingActionCount() const { return _scheduled_actions.size(); }
    
    /**
     * Get number of currently executing actions
     * @return count of executing actions
     */
    size_t getExecutingActionCount() const { return _executing_actions.size(); }
    
    /**
     * Check if an action is currently executing
     * @param action_id Unique identifier of the action
     * @return true if executing
     */
    bool isActionExecuting(const std::string& action_id) const;
    
    // Configuration
    /**
     * Configure temporal execution parameters
     * @param execution_window Maximum time window for action execution
     * @param default_duration Default duration for actions without explicit duration
     */
    void configureTemporalParameters(std::chrono::milliseconds execution_window,
                                   std::chrono::milliseconds default_duration);
    
    /**
     * Configure concurrency and priority settings
     * @param max_concurrent Maximum number of concurrent actions
     * @param use_priority Enable priority-based scheduling
     * @param enable_temporal Enable temporal constraint checking
     */
    void configureExecution(size_t max_concurrent = 3, 
                          bool use_priority = true,
                          bool enable_temporal = true);
    
    // AtomSpace integration
    /**
     * Get the scheduler context atom
     * @return Handle to scheduler context
     */
    Handle getSchedulerContext() const { return _scheduler_context; }
    
    /**
     * Get the active actions context atom
     * @return Handle to active actions context
     */
    Handle getActiveActionsContext() const { return _active_actions_context; }
    
    /**
     * Get the temporal context atom
     * @return Handle to temporal context
     */
    Handle getTemporalContext() const { return _temporal_context; }
    
    // Statistics and debugging
    /**
     * Get scheduling statistics
     * @return JSON string with statistics
     */
    std::string getSchedulingStatistics() const;
    
    /**
     * Get status information for debugging
     * @return JSON string with status details
     */
    std::string getStatusInfo() const;
    
    /**
     * Reset all statistics
     */
    void resetStatistics();
};

} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_ACTION_SCHEDULER_H