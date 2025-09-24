#
# AgentZero Learning Tests
#
# Copyright (C) 2024 OpenCog Foundation
# SPDX-License-Identifier: AGPL-3.0-or-later
#

FIND_PACKAGE(CxxTest)
IF (CXXTEST_FOUND)

    # Include directories
    INCLUDE_DIRECTORIES(
        ${CMAKE_CURRENT_SOURCE_DIR}/../include
        ${COGUTIL_INCLUDE_DIR}
        ${ATOMSPACE_INCLUDE_DIR}
        ${CXXTEST_INCLUDE_DIR}
    )

    # Test sources
    SET(TEST_SOURCES
        MetaLearningUTest.cxxtest
        ExperienceManagerUTest.cxxtest
        SkillAcquisitionUTest.cxxtest
        PolicyOptimizerUTest.cxxtest
    )

    # Generate and build tests
    FOREACH(TEST_SOURCE ${TEST_SOURCES})
        GET_FILENAME_COMPONENT(TEST_NAME ${TEST_SOURCE} NAME_WE)
        CXXTEST_ADD_TEST(${TEST_NAME} ${TEST_NAME}.cpp ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SOURCE})
        
        # Link test with libraries
        TARGET_LINK_LIBRARIES(${TEST_NAME}
            agentzero-learning
            ${COGUTIL_LIBRARY}
            ${ATOMSPACE_LIBRARY}
        )
        
        # Set test properties
        SET_TARGET_PROPERTIES(${TEST_NAME} PROPERTIES
            FOLDER "tests/agents/agentzero-learning"
        )
    ENDFOREACH()

ENDIF (CXXTEST_FOUND)