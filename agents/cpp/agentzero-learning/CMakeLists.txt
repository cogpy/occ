#
# OpenCog AgentZero Learning Components
#
# Copyright (C) 2024 OpenCog Foundation
# SPDX-License-Identifier: AGPL-3.0-or-later
#

CMAKE_MINIMUM_REQUIRED(VERSION 3.16)
PROJECT(agentzero-learning)

# OpenCog components required for learning
FIND_PACKAGE(CogUtil 2.0.3 REQUIRED)
FIND_PACKAGE(AtomSpace 5.0.4 REQUIRED)

# Learning-specific dependencies
FIND_PACKAGE(MOSES QUIET)
FIND_PACKAGE(ASMoses QUIET)

# Optional learning dependencies
FIND_PACKAGE(Learn QUIET)

# Include directories
INCLUDE_DIRECTORIES(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
)

# Library sources
SET(LEARNING_SOURCES
    src/MetaLearning.cpp
    src/ExperienceManager.cpp
    src/SkillAcquisition.cpp
    src/PolicyOptimizer.cpp
)

# Create the agentzero-learning library
ADD_LIBRARY(agentzero-learning SHARED ${LEARNING_SOURCES})

# Link dependencies
TARGET_LINK_LIBRARIES(agentzero-learning
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
)

# Link optional MOSES if available
IF(MOSES_FOUND)
    TARGET_LINK_LIBRARIES(agentzero-learning ${MOSES_LIBRARY})
    TARGET_COMPILE_DEFINITIONS(agentzero-learning PRIVATE HAVE_MOSES=1)
ENDIF()

# Link optional ASMoses if available
IF(ASMoses_FOUND)
    TARGET_LINK_LIBRARIES(agentzero-learning ${ASMOSES_LIBRARY})
    TARGET_COMPILE_DEFINITIONS(agentzero-learning PRIVATE HAVE_ASMOSES=1)
ENDIF()

# Link optional Learn if available
IF(Learn_FOUND)
    TARGET_LINK_LIBRARIES(agentzero-learning ${LEARN_LIBRARY})
    TARGET_COMPILE_DEFINITIONS(agentzero-learning PRIVATE HAVE_LEARN=1)
ENDIF()

# Set library properties
SET_TARGET_PROPERTIES(agentzero-learning PROPERTIES
    VERSION "1.0.0"
    SOVERSION "1"
)

# Install targets
INSTALL(TARGETS agentzero-learning
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

# Install headers
INSTALL(DIRECTORY include/opencog
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# Build tests if enabled
IF(CXXTEST_FOUND)
    ADD_SUBDIRECTORY(tests)
ENDIF()

# Build examples if enabled
OPTION(BUILD_AGENTZERO_LEARNING_EXAMPLES "Build AgentZero Learning examples" ON)
IF(BUILD_AGENTZERO_LEARNING_EXAMPLES)
    ADD_SUBDIRECTORY(examples)
ENDIF()