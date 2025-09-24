# Agent-Zero Learning & Adaptation Module
# Implements AZ-LEARN-003: MOSES Policy Optimization Integration
# Part of Phase 5: Learning & Adaptation

cmake_minimum_required(VERSION 3.16)

project(AgentZeroLearning
    VERSION 0.1.0
    DESCRIPTION "Agent-Zero Learning & Adaptation with MOSES Policy Optimization"
    LANGUAGES CXX
)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

message(STATUS "Configuring Agent-Zero Learning Module")

# Find required dependencies
find_package(PkgConfig REQUIRED)

# Required OpenCog dependencies for learning module
pkg_check_modules(COGUTIL REQUIRED cogutil)
pkg_check_modules(ATOMSPACE REQUIRED atomspace)
pkg_check_modules(MOSES REQUIRED moses)
pkg_check_modules(ASMOSES REQUIRED asmoses)

# Optional learning-related dependencies
pkg_check_modules(LEARN learn)
pkg_check_modules(MINER miner)
pkg_check_modules(URE ure)

# Report found dependencies
message(STATUS "Agent-Zero Learning Dependencies:")
message(STATUS "  cogutil: ${COGUTIL_VERSION}")
message(STATUS "  atomspace: ${ATOMSPACE_VERSION}")
message(STATUS "  moses: ${MOSES_VERSION}")
message(STATUS "  asmoses: ${ASMOSES_VERSION}")

if(LEARN_FOUND)
    message(STATUS "  learn: ${LEARN_VERSION}")
endif()

if(MINER_FOUND)
    message(STATUS "  miner: ${MINER_VERSION}")
endif()

if(URE_FOUND)
    message(STATUS "  ure: ${URE_VERSION}")
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
    ${MOSES_INCLUDE_DIRS}
    ${ASMOSES_INCLUDE_DIRS}
)

if(LEARN_FOUND)
    include_directories(${LEARN_INCLUDE_DIRS})
endif()

if(MINER_FOUND)
    include_directories(${MINER_INCLUDE_DIRS})
endif()

if(URE_FOUND)
    include_directories(${URE_INCLUDE_DIRS})
endif()

# Link directories
link_directories(
    ${COGUTIL_LIBRARY_DIRS}
    ${ATOMSPACE_LIBRARY_DIRS}
    ${MOSES_LIBRARY_DIRS}
    ${ASMOSES_LIBRARY_DIRS}
)

if(LEARN_FOUND)
    link_directories(${LEARN_LIBRARY_DIRS})
endif()

if(MINER_FOUND)
    link_directories(${MINER_LIBRARY_DIRS})
endif()

if(URE_FOUND)
    link_directories(${URE_LIBRARY_DIRS})
endif()

# Source files
set(AGENTZERO_LEARNING_SOURCES
    src/ExperienceManager.cpp
    src/PolicyOptimizer.cpp
    src/SkillAcquisition.cpp
    src/MetaLearning.cpp
    src/LearningUtils.cpp
)

# Header files (for installation)
set(AGENTZERO_LEARNING_HEADERS
    include/agentzero/learning/ExperienceManager.h
    include/agentzero/learning/PolicyOptimizer.h
    include/agentzero/learning/SkillAcquisition.h
    include/agentzero/learning/MetaLearning.h
    include/agentzero/learning/LearningUtils.h
    include/agentzero/learning/LearningTypes.h
)

# Create the library
add_library(agentzero-learning SHARED ${AGENTZERO_LEARNING_SOURCES})

# Set library properties
set_target_properties(agentzero-learning PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 0
    PUBLIC_HEADER "${AGENTZERO_LEARNING_HEADERS}"
)

# Link libraries
target_link_libraries(agentzero-learning
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${MOSES_LIBRARIES}
    ${ASMOSES_LIBRARIES}
)

if(LEARN_FOUND)
    target_link_libraries(agentzero-learning ${LEARN_LIBRARIES})
endif()

if(MINER_FOUND)
    target_link_libraries(agentzero-learning ${MINER_LIBRARIES})
endif()

if(URE_FOUND)
    target_link_libraries(agentzero-learning ${URE_LIBRARIES})
endif()

# Compiler flags
target_compile_definitions(agentzero-learning PRIVATE
    AGENTZERO_LEARNING_VERSION="${PROJECT_VERSION}"
    MOSES_INTEGRATION_ENABLED=1
    ASMOSES_INTEGRATION_ENABLED=1
)

if(LEARN_FOUND)
    target_compile_definitions(agentzero-learning PRIVATE LEARN_INTEGRATION_ENABLED=1)
endif()

if(MINER_FOUND)
    target_compile_definitions(agentzero-learning PRIVATE MINER_INTEGRATION_ENABLED=1)
endif()

if(URE_FOUND)
    target_compile_definitions(agentzero-learning PRIVATE URE_INTEGRATION_ENABLED=1)
endif()

# Installation
include(GNUInstallDirs)

install(TARGETS agentzero-learning
    EXPORT AgentZeroLearningTargets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/agentzero/learning
)

# Install export targets
install(EXPORT AgentZeroLearningTargets
    FILE AgentZeroLearningTargets.cmake
    NAMESPACE AgentZero::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/AgentZeroLearning
)

# Configure package config file
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/AgentZeroLearningConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/AgentZeroLearningConfig.cmake"
    @ONLY
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/AgentZeroLearningConfig.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/AgentZeroLearning
)

# Add subdirectories
if(BUILD_TESTING)
    add_subdirectory(tests)
endif()

if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Status summary
message(STATUS "Agent-Zero Learning Configuration:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  MOSES integration: ENABLED")
message(STATUS "  ASMOSES integration: ENABLED")
message(STATUS "  Learn integration: ${LEARN_FOUND}")
message(STATUS "  Miner integration: ${MINER_FOUND}")
message(STATUS "  URE integration: ${URE_FOUND}")
message(STATUS "")