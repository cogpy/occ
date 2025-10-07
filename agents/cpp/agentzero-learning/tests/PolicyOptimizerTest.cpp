/*
 * tests/PolicyOptimizerTest.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Unit tests for PolicyOptimizer class
 */

#include <iostream>
#include <cassert>
#include <memory>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/atom_types/types.h>

#include "opencog/agentzero/PolicyOptimizer.h"

using namespace opencog;
using namespace opencog::agentzero;

class PolicyOptimizerTest
{
private:
    AtomSpacePtr _atomspace;
    std::unique_ptr<PolicyOptimizer> _policy_optimizer;

public:
    PolicyOptimizerTest() 
        : _atomspace(std::make_shared<AtomSpace>())
        , _policy_optimizer(std::make_unique<PolicyOptimizer>(_atomspace))
    {}

    void runAllTests()
    {
        std::cout << "Running PolicyOptimizer tests..." << std::endl;
        
        testPolicyOptimization();
        testPolicyRefinement();
        testPolicyEvaluation();
        testMOSESParameters();
        
        std::cout << "All PolicyOptimizer tests passed!" << std::endl;
    }

private:
    void testPolicyOptimization()
    {
        std::cout << "Testing policy optimization..." << std::endl;
        
        HandleSeq initial_structure;
        initial_structure.push_back(_atomspace->add_node(CONCEPT_NODE, "Component1"));
        initial_structure.push_back(_atomspace->add_node(CONCEPT_NODE, "Component2"));
        
        std::vector<Handle> training_data;
        training_data.push_back(_atomspace->add_node(CONCEPT_NODE, "Training1"));
        
        Handle policy = _policy_optimizer->optimizePolicy(
            "TestPolicy",
            PolicyOptimizer::PolicyType::ACTION_SELECTION,
            PolicyOptimizer::OptimizationObjective::MAXIMIZE_EFFICIENCY,
            initial_structure,
            training_data
        );
        
        assert(policy != Handle::UNDEFINED);
        
        std::cout << "✓ Policy optimization test passed" << std::endl;
    }

    void testPolicyRefinement()
    {
        std::cout << "Testing policy refinement..." << std::endl;
        
        // Use the policy created in previous test
        std::vector<Handle> best_policies = _policy_optimizer->getBestPolicies(
            PolicyOptimizer::PolicyType::ACTION_SELECTION, 1);
        
        if (!best_policies.empty()) {
            Handle policy = best_policies[0];
            
            std::vector<Handle> new_data;
            new_data.push_back(_atomspace->add_node(CONCEPT_NODE, "RefinementData"));
            
            double fitness = _policy_optimizer->refinePolicy(policy, new_data, 3);
            assert(fitness >= 0.0 && fitness <= 1.0);
        }
        
        std::cout << "✓ Policy refinement test passed" << std::endl;
    }

    void testPolicyEvaluation()
    {
        std::cout << "Testing policy evaluation..." << std::endl;
        
        std::vector<Handle> policies = _policy_optimizer->getBestPolicies(
            PolicyOptimizer::PolicyType::ACTION_SELECTION, 5);
        
        if (!policies.empty()) {
            Handle policy = policies[0];
            
            std::vector<Handle> test_data;
            test_data.push_back(_atomspace->add_node(CONCEPT_NODE, "TestData"));
            
            double fitness = _policy_optimizer->evaluatePolicyFitness(policy, test_data);
            assert(fitness >= 0.0 && fitness <= 1.0);
            
            std::vector<double> history = _policy_optimizer->getOptimizationHistory(policy);
            assert(!history.empty());
        }
        
        std::cout << "✓ Policy evaluation test passed" << std::endl;
    }

    void testMOSESParameters()
    {
        std::cout << "Testing MOSES parameters..." << std::endl;
        
        _policy_optimizer->setMOSESParameters(50, 25, 0.15, 0.8);
        
        std::map<std::string, double> stats = _policy_optimizer->getOptimizationStatistics();
        assert(stats["population_size"] == 50.0);
        assert(stats["max_generations"] == 25.0);
        
        std::cout << "✓ MOSES parameters test passed" << std::endl;
    }
};

int main()
{
    try {
        PolicyOptimizerTest test;
        test.runAllTests();
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Test failed: " << e.what() << std::endl;
        return 1;
    }
}