/*
 * tests/SkillAcquisitionTest.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Unit tests for SkillAcquisition class
 * Part of the Agent-Zero Learning & Adaptation module
 */

#include <iostream>
#include <cassert>
#include <memory>
#include <vector>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/util/Logger.h>

#include "opencog/agentzero/SkillAcquisition.h"

using namespace opencog;
using namespace opencog::agentzero;

/**
 * Test class for SkillAcquisition functionality
 */
class SkillAcquisitionTest
{
private:
    AtomSpacePtr _atomspace;
    std::unique_ptr<SkillAcquisition> _skill_acquisition;

public:
    SkillAcquisitionTest() 
        : _atomspace(std::make_shared<AtomSpace>())
        , _skill_acquisition(std::make_unique<SkillAcquisition>(_atomspace))
    {
        logger().set_level(Logger::DEBUG);
        logger().set_print_to_stdout_flag(true);
    }

    void runAllTests()
    {
        std::cout << "Running SkillAcquisition tests..." << std::endl;
        
        testBasicSkillLearning();
        testSkillPractice();
        testSkillApplication();
        testSkillTransfer();
        testSkillManagement();
        testLearningConfiguration();
        testPerformanceTracking();
        
        std::cout << "All SkillAcquisition tests passed!" << std::endl;
    }

private:
    void testBasicSkillLearning()
    {
        std::cout << "Testing basic skill learning..." << std::endl;
        
        // Create experience data
        std::vector<Handle> experience_data;
        experience_data.push_back(_atomspace->add_node(CONCEPT_NODE, "Experience1"));
        experience_data.push_back(_atomspace->add_node(CONCEPT_NODE, "Experience2"));
        experience_data.push_back(_atomspace->add_node(CONCEPT_NODE, "Experience3"));
        
        // Learn a new skill
        Handle skill = _skill_acquisition->learnSkill(
            "TestSkill",
            SkillAcquisition::SkillType::PROCEDURAL,
            SkillAcquisition::LearningStrategy::IMITATION,
            experience_data
        );
        
        assert(skill != Handle::UNDEFINED);
        assert(_skill_acquisition->hasSkill("TestSkill"));
        assert(_skill_acquisition->getSkill("TestSkill") == skill);
        
        std::cout << "✓ Basic skill learning test passed" << std::endl;
    }

    void testSkillPractice()
    {
        std::cout << "Testing skill practice..." << std::endl;
        
        // Get existing skill
        Handle skill = _skill_acquisition->getSkill("TestSkill");
        assert(skill != Handle::UNDEFINED);
        
        SkillAcquisition::ProficiencyLevel initial_proficiency = 
            _skill_acquisition->getSkillProficiency(skill);
        
        // Practice the skill
        std::vector<Handle> practice_data;
        practice_data.push_back(_atomspace->add_node(CONCEPT_NODE, "Practice1"));
        practice_data.push_back(_atomspace->add_node(CONCEPT_NODE, "Practice2"));
        
        SkillAcquisition::ProficiencyLevel new_proficiency = 
            _skill_acquisition->practiceSkill(skill, practice_data);
        
        // Check that proficiency was updated
        assert(static_cast<int>(new_proficiency) >= static_cast<int>(initial_proficiency));
        
        std::cout << "✓ Skill practice test passed" << std::endl;
    }

    void testSkillApplication()
    {
        std::cout << "Testing skill application..." << std::endl;
        
        Handle skill = _skill_acquisition->getSkill("TestSkill");
        assert(skill != Handle::UNDEFINED);
        
        // Apply skill to a task
        std::vector<Handle> task_context;
        task_context.push_back(_atomspace->add_node(CONCEPT_NODE, "TaskContext1"));
        task_context.push_back(_atomspace->add_node(CONCEPT_NODE, "TaskContext2"));
        
        auto result = _skill_acquisition->applySkill(skill, task_context);
        
        assert(result.first == true); // Should succeed
        assert(!result.second.empty()); // Should have results
        
        std::cout << "✓ Skill application test passed" << std::endl;
    }

    void testSkillTransfer()
    {
        std::cout << "Testing skill transfer..." << std::endl;
        
        Handle source_skill = _skill_acquisition->getSkill("TestSkill");
        assert(source_skill != Handle::UNDEFINED);
        
        // Transfer skill
        std::vector<Handle> adaptation_rules;
        adaptation_rules.push_back(_atomspace->add_node(CONCEPT_NODE, "AdaptationRule1"));
        
        Handle transferred_skill = _skill_acquisition->transferSkill(
            source_skill,
            "TransferredSkill",
            adaptation_rules
        );
        
        assert(transferred_skill != Handle::UNDEFINED);
        assert(_skill_acquisition->hasSkill("TransferredSkill"));
        
        // Check that transferred skill has beginner proficiency
        SkillAcquisition::ProficiencyLevel transfer_proficiency = 
            _skill_acquisition->getSkillProficiency(transferred_skill);
        assert(transfer_proficiency == SkillAcquisition::ProficiencyLevel::BEGINNER);
        
        std::cout << "✓ Skill transfer test passed" << std::endl;
    }

    void testSkillManagement()
    {
        std::cout << "Testing skill management..." << std::endl;
        
        // Get all learned skills
        std::vector<Handle> all_skills = _skill_acquisition->getLearnedSkills();
        assert(all_skills.size() >= 2); // Should have at least TestSkill and TransferredSkill
        
        // Get skills by type
        std::vector<Handle> procedural_skills = 
            _skill_acquisition->getSkillsByType(SkillAcquisition::SkillType::PROCEDURAL);
        assert(!procedural_skills.empty());
        
        // Check performance history
        Handle test_skill = _skill_acquisition->getSkill("TestSkill");
        std::vector<double> performance_history = 
            _skill_acquisition->getSkillPerformanceHistory(test_skill);
        assert(!performance_history.empty());
        
        std::cout << "✓ Skill management test passed" << std::endl;
    }

    void testLearningConfiguration()
    {
        std::cout << "Testing learning configuration..." << std::endl;
        
        // Test learning rate setting
        _skill_acquisition->setLearningRate(0.5);
        
        // Test meta-learning toggle
        _skill_acquisition->setMetaLearningEnabled(false);
        _skill_acquisition->setMetaLearningEnabled(true);
        
        // Test skill transfer toggle
        _skill_acquisition->setSkillTransferEnabled(false);
        _skill_acquisition->setSkillTransferEnabled(true);
        
        // Test complexity limit
        _skill_acquisition->setMaxSkillComplexity(5);
        
        std::cout << "✓ Learning configuration test passed" << std::endl;
    }

    void testPerformanceTracking()
    {
        std::cout << "Testing performance tracking..." << std::endl;
        
        // Get learning statistics
        std::map<std::string, double> stats = _skill_acquisition->getLearningStatistics();
        assert(stats.find("total_skills") != stats.end());
        assert(stats["total_skills"] >= 2.0); // Should have at least 2 skills
        
        // Get status information
        std::string status = _skill_acquisition->getStatusInfo();
        assert(!status.empty());
        assert(status.find("SkillAcquisition Status") != std::string::npos);
        
        // Test parameter optimization
        bool optimization_result = _skill_acquisition->optimizeLearningParameters();
        // Result may be true or false depending on available data
        
        std::cout << "✓ Performance tracking test passed" << std::endl;
    }
};

int main()
{
    try {
        SkillAcquisitionTest test;
        test.runAllTests();
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Test failed with exception: " << e.what() << std::endl;
        return 1;
    }
}