/*
 * opencog/agentzero/WorkingMemory.h
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * WorkingMemory - Active context and short-term memory management
 * Part of the Agent-Zero Memory & Context Management module
 * Task: AZ-MEM-002 - Create WorkingMemory management
 */

#ifndef _OPENCOG_AGENTZERO_WORKING_MEMORY_H
#define _OPENCOG_AGENTZERO_WORKING_MEMORY_H

#include <memory>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <deque>
#include <functional>
#include <mutex>
#include <atomic>
#include <chrono>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/util/Logger.h>

#ifdef HAVE_ATTENTION_BANK
#include <opencog/attentionbank/bank/AttentionBank.h>
#include <opencog/attentionbank/types/atom_types.h>
#endif

#ifdef HAVE_ATOMSPACE_ROCKS
#include <opencog/persist/rocks/RocksStorageNode.h>
#endif

namespace opencog {
namespace agentzero {

/**
 * Memory item structure for working memory entries
 */
struct MemoryItem {
    Handle atom;                                        // The atom being held in memory
    std::chrono::steady_clock::time_point timestamp;    // When it was added
    std::chrono::steady_clock::time_point last_access;  // When it was last accessed
    double importance;                                   // Importance/attention value
    double decay_rate;                                   // How quickly importance decays
    size_t access_count;                                // Number of times accessed
    std::string context;                                // Context tag
    
    MemoryItem(Handle h, double imp = 1.0, const std::string& ctx = "")
        : atom(h), importance(imp), decay_rate(0.1), access_count(1), context(ctx) {
        auto now = std::chrono::steady_clock::now();
        timestamp = now;
        last_access = now;
    }
};

/**
 * WorkingMemory - Active context and short-term memory management for Agent-Zero
 * 
 * This class provides comprehensive working memory management:
 * - Active context management using AtomSpace
 * - Short-term memory with configurable capacity and decay
 * - Attention-based memory retention using ECAN
 * - Integration with atomspace-rocks for persistence 
 * - Memory cleanup and garbage collection
 * - Thread-safe operations for concurrent access
 * - Performance monitoring and optimization
 * 
 * Key Features:
 * - Temporal decay of memory items based on time and access patterns
 * - Attention value integration with OpenCog's ECAN system
 * - Context-aware memory organization and retrieval
 * - Configurable memory capacity and retention policies
 * - Efficient access patterns with LRU-style management
 * - Integration with AtomSpace for semantic operations
 */
class WorkingMemory {
private:
    // Core memory storage
    AtomSpacePtr _atomspace;
    std::deque<std::shared_ptr<MemoryItem>> _memory_buffer;
    std::map<Handle, std::shared_ptr<MemoryItem>> _memory_index;
    std::multimap<std::string, std::shared_ptr<MemoryItem>> _context_index;
    std::multimap<double, std::shared_ptr<MemoryItem>> _importance_index;
    
    // Memory management parameters
    size_t _max_capacity;                    // Maximum number of items in working memory
    double _importance_threshold;            // Minimum importance for retention
    std::chrono::seconds _max_retention_time; // Maximum time to keep items
    std::chrono::seconds _decay_interval;   // How often to run decay process
    
    // AtomSpace integration handles
    Handle _working_memory_root;    // Root node for working memory atoms
    Handle _context_space;          // Context organization space
    Handle _active_goals;           // Current active goals
    Handle _recent_percepts;        // Recently perceived information
    Handle _temporary_conclusions;  // Temporary reasoning results
    
#ifdef HAVE_ATTENTION_BANK
    AttentionBankPtr _attention_bank;  // For ECAN integration
#endif

#ifdef HAVE_ATOMSPACE_ROCKS
    Handle _persistence_node;      // RocksDB storage node for persistence
#endif
    
    // Thread safety
    mutable std::recursive_mutex _memory_mutex;
    std::atomic<bool> _cleanup_running;
    
    // Performance tracking
    std::atomic<size_t> _access_count;
    std::atomic<size_t> _hit_count;
    std::atomic<size_t> _miss_count;
    std::chrono::steady_clock::time_point _last_cleanup;
    std::chrono::steady_clock::time_point _creation_time;

public:
    /**
     * Constructor with default configuration
     * @param atomspace Shared pointer to AtomSpace
     */
    explicit WorkingMemory(AtomSpacePtr atomspace);

    /**
     * Constructor with custom configuration
     * @param atomspace Shared pointer to AtomSpace
     * @param max_capacity Maximum number of items to retain in working memory
     * @param importance_threshold Minimum importance for retention
     * @param max_retention_time Maximum time to retain items
     */
    WorkingMemory(AtomSpacePtr atomspace, 
                  size_t max_capacity = 1000,
                  double importance_threshold = 0.1,
                  std::chrono::seconds max_retention_time = std::chrono::seconds(3600));
    
    /**
     * Destructor
     */
    ~WorkingMemory();

    // Core memory operations
    
    /**
     * Add an item to working memory with optional context
     * @param atom The atom to add to working memory
     * @param importance Initial importance value
     * @param context Optional context tag for organization
     * @return true if successfully added
     */
    bool addItem(Handle atom, double importance = 1.0, const std::string& context = "");
    
    /**
     * Retrieve an item from working memory
     * @param atom The atom to retrieve
     * @return shared pointer to MemoryItem if found, nullptr otherwise
     */
    std::shared_ptr<MemoryItem> getItem(Handle atom);
    
    /**
     * Check if an item exists in working memory
     * @param atom The atom to check for
     * @return true if the atom is in working memory
     */
    bool hasItem(Handle atom) const;
    
    /**
     * Remove an item from working memory
     * @param atom The atom to remove
     * @return true if successfully removed
     */
    bool removeItem(Handle atom);
    
    /**
     * Update the importance of an item in working memory
     * @param atom The atom to update
     * @param importance New importance value
     * @return true if successfully updated
     */
    bool updateImportance(Handle atom, double importance);
    
    /**
     * Access an item (updates access count and timestamp)
     * @param atom The atom being accessed
     * @return shared pointer to MemoryItem if found, nullptr otherwise
     */
    std::shared_ptr<MemoryItem> accessItem(Handle atom);

    // Context-based operations
    
    /**
     * Get all items in a specific context
     * @param context The context to retrieve items for
     * @return vector of memory items in the context
     */
    std::vector<std::shared_ptr<MemoryItem>> getItemsByContext(const std::string& context) const;
    
    /**
     * Set the active context for new items
     * @param context The context to set as active
     */
    void setActiveContext(const std::string& context);
    
    /**
     * Get the current active context
     * @return the current active context string
     */
    std::string getActiveContext() const;
    
    /**
     * Clear all items in a specific context
     * @param context The context to clear
     * @return number of items removed
     */
    size_t clearContext(const std::string& context);

    // Importance and attention-based operations
    
    /**
     * Get items above a certain importance threshold
     * @param min_importance Minimum importance threshold
     * @return vector of memory items above threshold
     */
    std::vector<std::shared_ptr<MemoryItem>> getImportantItems(double min_importance) const;
    
    /**
     * Get the most important items up to a limit
     * @param max_items Maximum number of items to return
     * @return vector of most important memory items
     */
    std::vector<std::shared_ptr<MemoryItem>> getMostImportantItems(size_t max_items) const;
    
    /**
     * Get the least important items (candidates for removal)
     * @param max_items Maximum number of items to return
     * @return vector of least important memory items
     */
    std::vector<std::shared_ptr<MemoryItem>> getLeastImportantItems(size_t max_items) const;
    
#ifdef HAVE_ATTENTION_BANK
    /**
     * Synchronize importance values with attention bank
     * Updates memory item importance based on ECAN attention values
     */
    void synchronizeWithAttentionBank();
    
    /**
     * Set the attention bank for ECAN integration
     * @param attention_bank Shared pointer to AttentionBank
     */
    void setAttentionBank(AttentionBankPtr attention_bank);
#endif

    // Memory management and cleanup
    
    /**
     * Run memory cleanup and garbage collection
     * Removes items that have decayed below threshold or exceeded retention time
     * @param force_cleanup If true, ignores normal cleanup intervals
     * @return number of items removed
     */
    size_t runCleanup(bool force_cleanup = false);
    
    /**
     * Apply temporal decay to all memory items
     * Reduces importance of items based on time since last access
     */
    void applyTemporalDecay();
    
    /**
     * Clear all items from working memory
     */
    void clear();
    
    /**
     * Compact memory by removing gaps and optimizing indices
     */
    void compactMemory();

    // Configuration and capacity management
    
    /**
     * Set the maximum capacity of working memory
     * @param capacity New maximum capacity
     */
    void setMaxCapacity(size_t capacity);
    
    /**
     * Get the current maximum capacity
     * @return current maximum capacity
     */
    size_t getMaxCapacity() const { return _max_capacity; }
    
    /**
     * Get the current number of items in memory
     * @return current size of working memory
     */
    size_t getCurrentSize() const;
    
    /**
     * Check if working memory is at capacity
     * @return true if at or over capacity
     */
    bool isAtCapacity() const;
    
    /**
     * Set the importance threshold for retention
     * @param threshold New importance threshold
     */
    void setImportanceThreshold(double threshold);
    
    /**
     * Get the current importance threshold
     * @return current importance threshold
     */
    double getImportanceThreshold() const { return _importance_threshold; }

    // AtomSpace integration
    
    /**
     * Create AtomSpace representations of working memory contents
     * Creates structured representation in the AtomSpace for reasoning
     */
    void createAtomSpaceRepresentation();
    
    /**
     * Update AtomSpace representation with current memory state
     */
    void updateAtomSpaceRepresentation();
    
    /**
     * Get the root handle for working memory in AtomSpace
     * @return Handle to the working memory root node
     */
    Handle getWorkingMemoryRoot() const { return _working_memory_root; }

#ifdef HAVE_ATOMSPACE_ROCKS
    // Persistence operations
    
    /**
     * Save working memory state to persistent storage
     * @param storage_path Optional path for storage
     * @return true if successfully saved
     */
    bool saveToStorage(const std::string& storage_path = "");
    
    /**
     * Load working memory state from persistent storage
     * @param storage_path Optional path for storage
     * @return true if successfully loaded
     */
    bool loadFromStorage(const std::string& storage_path = "");
    
    /**
     * Set up persistence with RocksDB storage node
     * @param storage_path Path for RocksDB storage
     * @return true if successfully configured
     */
    bool configurePersistence(const std::string& storage_path);
#endif

    // Performance monitoring and statistics
    
    /**
     * Get performance statistics for working memory
     * @return map of statistic names to values
     */
    std::map<std::string, double> getPerformanceStats() const;
    
    /**
     * Get cache hit rate (successful retrievals / total accesses)
     * @return hit rate as a percentage (0.0 to 1.0)
     */
    double getHitRate() const;
    
    /**
     * Reset performance counters
     */
    void resetPerformanceCounters();
    
    /**
     * Get memory usage information
     * @return map of memory usage metrics
     */
    std::map<std::string, size_t> getMemoryUsage() const;

    // Debug and diagnostic operations
    
    /**
     * Print working memory contents to log
     * @param log_level Log level to use (debug, info, etc.)
     */
    void printMemoryContents(const std::string& log_level = "debug") const;
    
    /**
     * Validate internal consistency of memory structures
     * @return true if all internal structures are consistent
     */
    bool validateMemoryConsistency() const;
    
    /**
     * Get detailed information about a memory item
     * @param atom The atom to get information for
     * @return formatted string with detailed item information
     */
    std::string getItemInfo(Handle atom) const;

private:
    // Internal helper methods
    
    /**
     * Initialize working memory structures in AtomSpace
     */
    void initializeAtomSpaceStructures();
    
    /**
     * Remove item from all indices
     * @param item The memory item to remove from indices
     */
    void removeFromIndices(std::shared_ptr<MemoryItem> item);
    
    /**
     * Add item to all indices
     * @param item The memory item to add to indices
     */
    void addToIndices(std::shared_ptr<MemoryItem> item);
    
    /**
     * Enforce capacity limits by removing least important items
     * @return number of items removed
     */
    size_t enforceCapacityLimits();
    
    /**
     * Calculate decay factor based on time since last access
     * @param item The memory item to calculate decay for
     * @return decay factor (0.0 to 1.0)
     */
    double calculateDecayFactor(const std::shared_ptr<MemoryItem>& item) const;
    
    /**
     * Update indices after importance change
     * @param item The memory item that was updated
     * @param old_importance Previous importance value
     */
    void updateIndicesForImportanceChange(std::shared_ptr<MemoryItem> item, double old_importance);

    // Current active context
    std::string _active_context;
    
    // Performance and monitoring
    mutable std::atomic<size_t> _total_operations;
    mutable std::chrono::steady_clock::time_point _last_performance_reset;
};

} // namespace agentzero
} // namespace opencog

#endif // _OPENCOG_AGENTZERO_WORKING_MEMORY_H