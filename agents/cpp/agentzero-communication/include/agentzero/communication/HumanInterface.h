/**
 * HumanInterface.h - Human-Agent Interaction Layer for Agent-Zero Communication
 * 
 * Part of AZ-HUMAN-001: Create HumanInterface layer
 * Provides comprehensive human-agent interaction capabilities
 * 
 * Copyright (C) 2024 OpenCog Foundation
 */

#ifndef AGENTZERO_HUMAN_INTERFACE_H
#define AGENTZERO_HUMAN_INTERFACE_H

#include <memory>
#include <string>
#include <vector>
#include <map>
#include <deque>
#include <functional>
#include <mutex>
#include <atomic>
#include <chrono>

#include "CommunicationTypes.h"

namespace opencog {
/*
 * HumanInterface.h
 *
 * Copyright (C) 2024 Agent-Zero-Genesis Project
 * 
 * Human-agent interaction layer for Agent-Zero
 */

#ifndef _AGENTZERO_HUMAN_INTERFACE_H
#define _AGENTZERO_HUMAN_INTERFACE_H

#include <string>
#include <opencog/atomspace/AtomSpace.h>

namespace agentzero {
namespace communication {

/**
 * HumanInterface - Core human-agent interaction management
 * 
 * This class provides comprehensive human-agent interaction capabilities:
 * - Multi-modal input processing (text, voice, gesture)
 * - Contextual conversation state management
 * - AtomSpace integration for interaction knowledge representation
 * - Session management for multiple concurrent conversations
 * - Response generation and formatting
 * - Interaction history and analytics
 */
class HumanInterface {
public:
    /**
     * Constructor
     * @param atomspace Shared pointer to the AtomSpace for knowledge representation
     * @param config Communication configuration parameters
     */
    explicit HumanInterface(AtomSpacePtr atomspace, 
                           const CommunicationConfig& config = CommunicationConfig{});
    
    /**
     * Destructor - Clean up resources and finalize sessions
     */
    virtual ~HumanInterface();

    /**
     * Initialize the human interface system
     * @return true if initialization successful, false otherwise
     */
    bool initialize();

    /**
     * Shutdown the human interface system gracefully
     */
    void shutdown();

    // === Core Interaction Methods ===

    /**
     * Process human input and generate appropriate response
     * @param input The human input data
     * @param session_id Unique session identifier
     * @return Interaction response containing agent's reply and metadata
     */
    InteractionResponse processInput(const HumanInput& input, const std::string& session_id);

    /**
     * Start a new interaction session
     * @param user_id Unique user identifier
     * @param session_config Optional session-specific configuration
     * @return Unique session identifier
     */
    std::string startSession(const std::string& user_id,
                           const SessionConfig& session_config = SessionConfig{});

    /**
     * End an interaction session and clean up resources
     * @param session_id Session identifier to terminate
     * @return true if session ended successfully, false if session not found
     */
    bool endSession(const std::string& session_id);

    /**
     * Get current session state and context information
     * @param session_id Session identifier
     * @return Session state or nullptr if session not found
     */
    std::shared_ptr<SessionState> getSessionState(const std::string& session_id) const;

    // === Context Management ===

    /**
     * Update interaction context for a session
     * @param session_id Session identifier
     * @param context_update Context information to add/update
     */
    void updateContext(const std::string& session_id, const ContextUpdate& context_update);

    /**
     * Get current interaction context for a session
     * @param session_id Session identifier
     * @return Current context or empty context if session not found
     */
    InteractionContext getContext(const std::string& session_id) const;

    /**
     * Clear interaction context for a session
     * @param session_id Session identifier
     */
    void clearContext(const std::string& session_id);

    // === History and Analytics ===

    /**
     * Get interaction history for a session
     * @param session_id Session identifier
     * @param max_interactions Maximum number of recent interactions to retrieve
     * @return Vector of interaction records
     */
    std::vector<InteractionRecord> getHistory(const std::string& session_id, 
                                            size_t max_interactions = 50) const;

    /**
     * Get interaction analytics and statistics
     * @param session_id Session identifier (empty for global analytics)
     * @return Analytics data including response times, success rates, etc.
     */
    InteractionAnalytics getAnalytics(const std::string& session_id = "") const;

    // === Configuration and Control ===

    /**
     * Update configuration parameters
     * @param new_config New configuration to apply
     */
    void updateConfig(const CommunicationConfig& new_config);

    /**
     * Get current configuration
     * @return Current configuration
     */
    const CommunicationConfig& getConfig() const { return config_; }

    /**
     * Register a custom input processor
     * @param input_type Type of input to process
     * @param processor Function to process this input type
     */
    void registerInputProcessor(InputType input_type, 
                              std::function<ProcessedInput(const RawInput&)> processor);

    /**
     * Register a custom response formatter
     * @param output_format Format type for responses
     * @param formatter Function to format responses
     */
    void registerResponseFormatter(OutputFormat output_format,
                                 std::function<FormattedResponse(const AgentResponse&)> formatter);

    // === AtomSpace Integration ===

    /**
     * Store interaction in AtomSpace for persistent knowledge
     * @param interaction_record Complete interaction record to store
     * @return Handle to the stored interaction atom
     */
    Handle storeInteractionInAtomSpace(const InteractionRecord& interaction_record);

    /**
     * Retrieve interaction patterns from AtomSpace
     * @param query_params Parameters for pattern matching
     * @return Vector of matching interaction patterns
     */
    std::vector<Handle> retrieveInteractionPatterns(const PatternQuery& query_params) const;

    /**
     * Update interaction knowledge in AtomSpace
     * @param knowledge_update Knowledge to add or update
     */
    void updateInteractionKnowledge(const KnowledgeUpdate& knowledge_update);

    // === Status and Monitoring ===

    /**
     * Get current system status
     * @return Status information including active sessions, health metrics
     */
    SystemStatus getStatus() const;

    /**
     * Check if the interface is ready for interactions
     * @return true if ready, false otherwise
     */
    bool isReady() const { return initialized_ && !shutdown_requested_; }

    /**
     * Get number of active sessions
     * @return Count of active sessions
     */
    size_t getActiveSessionCount() const;

private:
    // === Core Components ===
    std::unique_ptr<InputProcessor> input_processor_;
    std::unique_ptr<ResponseGenerator> response_generator_;
    std::unique_ptr<SessionManager> session_manager_;
    std::unique_ptr<ContextManager> context_manager_;

    // === AtomSpace Integration ===
    AtomSpacePtr atomspace_;
    std::unique_ptr<InteractionKnowledgeStore> knowledge_store_;

    // === Configuration ===
    CommunicationConfig config_;
    
    // === State Management ===
    mutable std::mutex interface_mutex_;
    std::atomic<bool> initialized_{false};
    std::atomic<bool> shutdown_requested_{false};
    
    // === Session Storage ===
    std::map<std::string, std::shared_ptr<SessionState>> active_sessions_;
    mutable std::mutex sessions_mutex_;
    
    // === Analytics ===
    std::unique_ptr<InteractionAnalyzer> analyzer_;
    mutable std::mutex analytics_mutex_;
    
    // === Custom Processors and Formatters ===
    std::map<InputType, std::function<ProcessedInput(const RawInput&)>> input_processors_;
    std::map<OutputFormat, std::function<FormattedResponse(const AgentResponse&)>> response_formatters_;
    mutable std::mutex processors_mutex_;

    // === Private Methods ===
    
    /**
     * Internal method to process input with proper error handling
     */
    InteractionResponse processInputInternal(const HumanInput& input, 
                                           const std::string& session_id);

    /**
     * Generate unique session identifier
     */
    std::string generateSessionId(const std::string& user_id) const;

    /**
     * Validate session exists and is active
     */
    bool validateSession(const std::string& session_id) const;

    /**
     * Clean up expired sessions
     */
    void cleanupExpiredSessions();

    /**
     * Initialize default input processors and response formatters
     */
    void initializeDefaultProcessors();

    /**
     * Log interaction for debugging and analytics
     */
    void logInteraction(const std::string& session_id, 
                       const HumanInput& input,
                       const InteractionResponse& response) const;
 * HumanInterface manages human-agent interactions
 * 
 * TODO: Implementation planned for AZ-HUMAN-001
 */
class HumanInterface
{
private:
    opencog::AtomSpacePtr _atomspace;

public:
    explicit HumanInterface(opencog::AtomSpacePtr atomspace);
    ~HumanInterface() = default;
    
    // Placeholder methods - to be implemented in AZ-HUMAN-001
    std::string process_human_input(const std::string& input);
    std::string generate_response(const std::string& context);
};

} // namespace communication
} // namespace agentzero
} // namespace opencog

#endif // AGENTZERO_HUMAN_INTERFACE_H

#endif // _AGENTZERO_HUMAN_INTERFACE_H
