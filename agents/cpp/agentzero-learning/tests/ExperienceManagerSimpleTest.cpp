/*
 * ExperienceManagerSimpleTest.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Simple test for ExperienceManager implementation
 * Part of the AGENT-ZERO-GENESIS project - Phase 5: Learning & Adaptation
 */

#include <iostream>
#include <memory>
#include <chrono>
#include <cassert>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/util/Logger.h>

#include "opencog/agentzero/learning/ExperienceManager.h"

using namespace opencog;
using namespace opencog::agentzero::learning;

class MockAgentZeroCore {
public:
    MockAgentZeroCore() = default;
    ~MockAgentZeroCore() = default;
};

void testExperienceManagerConstruction() {
    std::cout << "\n=== Testing ExperienceManager Construction ===" << std::endl;
    
    // Create AtomSpace
    AtomSpacePtr atomspace = std::make_shared<AtomSpace>();
    
    // Create mock agent core
    MockAgentZeroCore* mock_core = new MockAgentZeroCore();
    
    // Create ExperienceManager
    ExperienceManager* exp_manager = nullptr;
    
    try {
        exp_manager = new ExperienceManager(reinterpret_cast<AgentZeroCore*>(mock_core), atomspace);
        std::cout << "✅ ExperienceManager constructed successfully" << std::endl;
        
        // Test configuration status
        std::string config_status = exp_manager->getConfigurationStatus();
        std::cout << "Configuration Status:\n" << config_status << std::endl;
        
        assert(!config_status.empty());
        std::cout << "✅ Configuration status retrieved successfully" << std::endl;
        
    } catch (const std::exception& e) {
        std::cout << "❌ ExperienceManager construction failed: " << e.what() << std::endl;
        assert(false);
    }
    
    // Cleanup
    delete exp_manager;
    delete mock_core;
    
    std::cout << "✅ ExperienceManager construction test passed" << std::endl;
}

void testBasicExperienceRecording() {
    std::cout << "\n=== Testing Basic Experience Recording ===" << std::endl;
    
    // Create AtomSpace
    AtomSpacePtr atomspace = std::make_shared<AtomSpace>();
    
    // Create mock agent core
    MockAgentZeroCore* mock_core = new MockAgentZeroCore();
    
    // Create ExperienceManager
    ExperienceManager exp_manager(reinterpret_cast<AgentZeroCore*>(mock_core), atomspace);
    
    try {
        // Record a simple experience
        Handle exp_atom = exp_manager.recordExperience(
            "Test successful action",
            ExperienceManager::ExperienceType::ACTION_OUTCOME,
            ExperienceManager::ExperienceOutcome::SUCCESS
        );
        
        assert(exp_atom != Handle::UNDEFINED);
        std::cout << "✅ Basic experience recorded successfully: " << exp_atom->to_string() << std::endl;
        
        // Record a failure experience
        Handle fail_exp_atom = exp_manager.recordExperience(
            "Test failed action",
            ExperienceManager::ExperienceType::ACTION_OUTCOME,
            ExperienceManager::ExperienceOutcome::FAILURE
        );
        
        assert(fail_exp_atom != Handle::UNDEFINED);
        std::cout << "✅ Failure experience recorded successfully: " << fail_exp_atom->to_string() << std::endl;
        
        // Get experience statistics
        auto stats = exp_manager.getExperienceStatistics();
        std::cout << "Experience Statistics:" << std::endl;
        for (const auto& stat : stats) {
            std::cout << "  Type " << static_cast<int>(stat.first) << ": " << stat.second << " experiences" << std::endl;
        }
        
        assert(stats.size() > 0);
        std::cout << "✅ Experience statistics retrieved successfully" << std::endl;
        
    } catch (const std::exception& e) {
        std::cout << "❌ Experience recording failed: " << e.what() << std::endl;
        assert(false);
    }
    
    // Cleanup
    delete mock_core;
    
    std::cout << "✅ Basic experience recording test passed" << std::endl;
}

void testExperienceRetrieval() {
    std::cout << "\n=== Testing Experience Retrieval ===" << std::endl;
    
    // Create AtomSpace
    AtomSpacePtr atomspace = std::make_shared<AtomSpace>();
    
    // Create mock agent core
    MockAgentZeroCore* mock_core = new MockAgentZeroCore();
    
    // Create ExperienceManager
    ExperienceManager exp_manager(reinterpret_cast<AgentZeroCore*>(mock_core), atomspace);
    
    try {
        // Record multiple experiences
        std::vector<Handle> recorded_experiences;
        
        for (int i = 0; i < 5; ++i) {
            Handle exp_atom = exp_manager.recordExperience(
                "Test experience " + std::to_string(i),
                ExperienceManager::ExperienceType::ACTION_OUTCOME,
                (i % 2 == 0) ? ExperienceManager::ExperienceOutcome::SUCCESS : 
                              ExperienceManager::ExperienceOutcome::FAILURE
            );
            recorded_experiences.push_back(exp_atom);
        }
        
        std::cout << "✅ Recorded " << recorded_experiences.size() << " experiences" << std::endl;
        
        // Test retrieval by outcome
        std::vector<Handle> successful_experiences = exp_manager.getExperiencesByOutcome(
            ExperienceManager::ExperienceOutcome::SUCCESS, 10);
        
        std::cout << "✅ Retrieved " << successful_experiences.size() << " successful experiences" << std::endl;
        assert(successful_experiences.size() >= 2); // We recorded at least 3 successes (0, 2, 4)
        
        std::vector<Handle> failed_experiences = exp_manager.getExperiencesByOutcome(
            ExperienceManager::ExperienceOutcome::FAILURE, 10);
        
        std::cout << "✅ Retrieved " << failed_experiences.size() << " failed experiences" << std::endl;
        assert(failed_experiences.size() >= 2); // We recorded 2 failures (1, 3)
        
        // Test similar experience retrieval (with empty context)
        std::vector<Handle> similar_experiences = exp_manager.getSimilarExperiences(
            {}, ExperienceManager::ExperienceType::ACTION_OUTCOME, 3);
        
        std::cout << "✅ Retrieved " << similar_experiences.size() << " similar experiences" << std::endl;
        
    } catch (const std::exception& e) {
        std::cout << "❌ Experience retrieval failed: " << e.what() << std::endl;
        assert(false);
    }
    
    // Cleanup
    delete mock_core;
    
    std::cout << "✅ Experience retrieval test passed" << std::endl;
}

void testPatternDiscovery() {
    std::cout << "\n=== Testing Pattern Discovery ===" << std::endl;
    
    // Create AtomSpace
    AtomSpacePtr atomspace = std::make_shared<AtomSpace>();
    
    // Create mock agent core
    MockAgentZeroCore* mock_core = new MockAgentZeroCore();
    
    // Create ExperienceManager
    ExperienceManager exp_manager(reinterpret_cast<AgentZeroCore*>(mock_core), atomspace);
    
    try {
        // Create context with some actions and consequences
        ExperienceManager::ExperienceContext context;
        context.timestamp = std::chrono::system_clock::now();
        context.confidence_level = 0.8;
        
        // Create some action atoms
        Handle action1 = atomspace->add_node(CONCEPT_NODE, "MoveForward");
        Handle action2 = atomspace->add_node(CONCEPT_NODE, "TurnLeft");
        Handle consequence1 = atomspace->add_node(CONCEPT_NODE, "ReachedGoal");
        Handle consequence2 = atomspace->add_node(CONCEPT_NODE, "AvoidedObstacle");
        
        std::vector<Handle> actions = {action1, action2};
        std::vector<Handle> consequences = {consequence1, consequence2};
        
        // Record detailed experience
        Handle detailed_exp = exp_manager.recordExperience(
            "Detailed navigation experience",
            ExperienceManager::ExperienceType::PROBLEM_SOLVING,
            ExperienceManager::ExperienceOutcome::SUCCESS,
            context,
            actions,
            consequences
        );
        
        assert(detailed_exp != Handle::UNDEFINED);
        std::cout << "✅ Detailed experience recorded: " << detailed_exp->to_string() << std::endl;
        
        // Test pattern discovery
        size_t patterns_discovered = exp_manager.discoverExperiencePatterns(
            ExperienceManager::ExperienceType::ACTION_OUTCOME);
        
        std::cout << "✅ Pattern discovery completed, found " << patterns_discovered << " patterns" << std::endl;
        
        // Test experience management processing
        bool processing_success = exp_manager.processExperienceManagement();
        assert(processing_success);
        std::cout << "✅ Experience management processing successful" << std::endl;
        
    } catch (const std::exception& e) {
        std::cout << "❌ Pattern discovery failed: " << e.what() << std::endl;
        assert(false);
    }
    
    // Cleanup
    delete mock_core;
    
    std::cout << "✅ Pattern discovery test passed" << std::endl;
}

void testExperienceExport() {
    std::cout << "\n=== Testing Experience Export ===" << std::endl;
    
    // Create AtomSpace
    AtomSpacePtr atomspace = std::make_shared<AtomSpace>();
    
    // Create mock agent core
    MockAgentZeroCore* mock_core = new MockAgentZeroCore();
    
    // Create ExperienceManager
    ExperienceManager exp_manager(reinterpret_cast<AgentZeroCore*>(mock_core), atomspace);
    
    try {
        // Record some experiences
        for (int i = 0; i < 3; ++i) {
            exp_manager.recordExperience(
                "Export test experience " + std::to_string(i),
                ExperienceManager::ExperienceType::ACTION_OUTCOME,
                ExperienceManager::ExperienceOutcome::SUCCESS
            );
        }
        
        // Test export
        std::string exported_json = exp_manager.exportExperiences(
            ExperienceManager::ExperienceType::ACTION_OUTCOME);
        
        assert(!exported_json.empty());
        std::cout << "✅ Experience export successful" << std::endl;
        std::cout << "Exported JSON sample (first 200 chars):\n" 
                  << exported_json.substr(0, 200) << "..." << std::endl;
        
        // Verify JSON contains expected elements
        assert(exported_json.find("experiences") != std::string::npos);
        assert(exported_json.find("total_count") != std::string::npos);
        std::cout << "✅ Export format validation passed" << std::endl;
        
    } catch (const std::exception& e) {
        std::cout << "❌ Experience export failed: " << e.what() << std::endl;
        assert(false);
    }
    
    // Cleanup
    delete mock_core;
    
    std::cout << "✅ Experience export test passed" << std::endl;
}

int main() {
    std::cout << "Starting ExperienceManager Simple Tests..." << std::endl;
    
    // Set up logging
    logger().set_level(Logger::INFO);
    logger().set_print_to_stdout_flag(true);
    
    try {
        // Run all tests
        testExperienceManagerConstruction();
        testBasicExperienceRecording();
        testExperienceRetrieval();
        testPatternDiscovery();
        testExperienceExport();
        
        std::cout << "\n🎉 All ExperienceManager tests passed successfully!" << std::endl;
        return 0;
        
    } catch (const std::exception& e) {
        std::cout << "\n❌ Test suite failed with exception: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cout << "\n❌ Test suite failed with unknown exception" << std::endl;
        return 1;
    }
}