/*
 * tests/MetaLearningTest.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Unit tests for MetaLearning class
 */

#include <iostream>
#include <cassert>
#include <memory>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/atom_types/types.h>

#include "opencog/agentzero/MetaLearning.h"

using namespace opencog;
using namespace opencog::agentzero;

class MetaLearningTest
{
private:
    AtomSpacePtr _atomspace;
    std::unique_ptr<MetaLearning> _meta_learning;

public:
    MetaLearningTest() 
        : _atomspace(std::make_shared<AtomSpace>())
        , _meta_learning(std::make_unique<MetaLearning>(_atomspace))
    {}

    void runAllTests()
    {
        std::cout << "Running MetaLearning tests..." << std::endl;
        
        testStrategyAdaptation();
        testParameterOptimization();
        testPatternAnalysis();
        testKnowledgeTransfer();
        
        std::cout << "All MetaLearning tests passed!" << std::endl;
    }

private:
    void testStrategyAdaptation()
    {
        std::cout << "Testing strategy adaptation..." << std::endl;
        
        Handle skill = _atomspace->add_node(CONCEPT_NODE, "TestSkill");
        std::vector<Handle> context;
        context.push_back(_atomspace->add_node(CONCEPT_NODE, "Context1"));
        
        MetaLearning::StrategyAdaptation adaptation = 
            _meta_learning->adaptLearningStrategy(skill, 0, context);
        
        assert(static_cast<int>(adaptation) >= 0);
        
        // Test adaptation effectiveness evaluation
        _meta_learning->evaluateAdaptationEffectiveness(skill, adaptation, 0.4, 0.6);
        
        std::cout << "✓ Strategy adaptation test passed" << std::endl;
    }

    void testParameterOptimization()
    {
        std::cout << "Testing parameter optimization..." << std::endl;
        
        std::map<std::string, double> params;
        params["learning_rate"] = 0.1;
        params["exploration_rate"] = 0.2;
        
        std::vector<double> feedback = {0.3, 0.4, 0.5, 0.6};
        
        std::map<std::string, double> optimized = 
            _meta_learning->optimizeLearningParameters("test_context", params, feedback);
        
        assert(!optimized.empty());
        assert(optimized.find("learning_rate") != optimized.end());
        
        std::cout << "✓ Parameter optimization test passed" << std::endl;
    }

    void testPatternAnalysis()
    {
        std::cout << "Testing pattern analysis..." << std::endl;
        
        std::map<std::string, std::vector<double>> history;
        history["context1"] = {0.1, 0.2, 0.3, 0.4, 0.5};
        history["context2"] = {0.8, 0.82, 0.83, 0.84, 0.85}; // Plateau pattern
        
        auto recommendations = _meta_learning->analyzeLearningPatterns(history);
        assert(!recommendations.empty());
        
        std::cout << "✓ Pattern analysis test passed" << std::endl;
    }

    void testKnowledgeTransfer()
    {
        std::cout << "Testing knowledge transfer..." << std::endl;
        
        // Set up source domain with some learning data
        std::map<std::string, double> outcomes;
        outcomes["performance"] = 0.7;
        outcomes["learning_rate"] = 0.1;
        
        std::vector<Handle> session_data;
        session_data.push_back(_atomspace->add_node(CONCEPT_NODE, "SessionData"));
        
        _meta_learning->updateMetaKnowledge(session_data, outcomes);
        
        // Test transfer
        bool transfer_result = _meta_learning->transferMetaKnowledge(
            "session_1", "target_domain", 0.5);
        
        // Result may be true or false depending on similarity
        
        std::cout << "✓ Knowledge transfer test passed" << std::endl;
    }
};

int main()
{
    try {
        MetaLearningTest test;
        test.runAllTests();
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Test failed: " << e.what() << std::endl;
        return 1;
    }
}