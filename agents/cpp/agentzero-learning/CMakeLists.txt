# Agent-Zero Learning & Adaptation Module
# Comprehensive implementation combining AZ-LEARN-003 MOSES Policy Optimization 
# with advanced ExperienceManager and sophisticated experience classification
# Part of Phase 5: Learning & Adaptation

cmake_minimum_required(VERSION 3.16)

project(AgentZeroLearning
    VERSION 0.1.0
    DESCRIPTION "Comprehensive Agent-Zero Learning with MOSES Policy Optimization and Advanced Experience Management"
    LANGUAGES CXX
)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

message(STATUS "Configuring Comprehensive Agent-Zero Learning Module")

# Find required dependencies
find_package(PkgConfig REQUIRED)

# Required OpenCog dependencies for learning module
pkg_check_modules(COGUTIL REQUIRED cogutil)
pkg_check_modules(ATOMSPACE REQUIRED atomspace)

# MOSES dependencies - handle both required and optional scenarios
pkg_check_modules(MOSES moses)
if (MOSES_FOUND)
    message(STATUS "Found MOSES version ${MOSES_VERSION}")
else()
    message(STATUS "MOSES not found via pkg-config, looking for system includes")
    find_path(MOSES_INCLUDE_PATH
        NAMES opencog/moses/main/moses.h
        PATHS /usr/local/include /usr/include
        PATH_SUFFIXES opencog
    )
    if(MOSES_INCLUDE_PATH)
        message(STATUS "Found MOSES headers at ${MOSES_INCLUDE_PATH}")
        set(MOSES_FOUND TRUE)
    else()
        message(WARNING "MOSES not found - MOSES integration will be disabled")
        set(MOSES_FOUND FALSE)
    endif()
endif()

pkg_check_modules(ASMOSES asmoses)
if (ASMOSES_FOUND)
    message(STATUS "Found ASMOSES version ${ASMOSES_VERSION}")
else()
    message(STATUS "ASMOSES not found via pkg-config, looking for system includes")
    find_path(ASMOSES_INCLUDE_PATH
        NAMES opencog/asmoses/combo/combo.h
        PATHS /usr/local/include /usr/include
        PATH_SUFFIXES opencog
    )
    if(ASMOSES_INCLUDE_PATH)
        message(STATUS "Found ASMOSES headers at ${ASMOSES_INCLUDE_PATH}")
        set(ASMOSES_FOUND TRUE)
    else()
        message(STATUS "ASMOSES not found - AtomSpace evolution features may be limited")
        set(ASMOSES_FOUND FALSE)
    endif()
endif()

# Optional learning-related dependencies
pkg_check_modules(LEARN learn)
pkg_check_modules(MINER miner)
pkg_check_modules(URE ure)
pkg_check_modules(COGSERVER cogserver)

# Report found dependencies
message(STATUS "Agent-Zero Learning Dependencies:")
message(STATUS "  cogutil: ${COGUTIL_VERSION}")
message(STATUS "  atomspace: ${ATOMSPACE_VERSION}")
message(STATUS "  moses: ${MOSES_FOUND}")
message(STATUS "  asmoses: ${ASMOSES_FOUND}")

if(LEARN_FOUND)
    message(STATUS "  learn: ${LEARN_VERSION}")
endif()

if(MINER_FOUND)
    message(STATUS "  miner: ${MINER_VERSION}")
endif()

if(URE_FOUND)
    message(STATUS "  ure: ${URE_VERSION}")
endif()

if(COGSERVER_FOUND)
    message(STATUS "  cogserver: ${COGSERVER_VERSION}")
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${COGUTIL_INCLUDE_DIRS}
    ${ATOMSPACE_INCLUDE_DIRS}
)

if(MOSES_FOUND)
    include_directories(${MOSES_INCLUDE_DIRS})
endif()

if(ASMOSES_FOUND)
    include_directories(${ASMOSES_INCLUDE_DIRS})
endif()

if(LEARN_FOUND)
    include_directories(${LEARN_INCLUDE_DIRS})
endif()

if(MINER_FOUND)
    include_directories(${MINER_INCLUDE_DIRS})
endif()

if(URE_FOUND)
    include_directories(${URE_INCLUDE_DIRS})
endif()

if(COGSERVER_FOUND)
    include_directories(${COGSERVER_INCLUDE_DIRS})
endif()

# Link directories
link_directories(
    ${COGUTIL_LIBRARY_DIRS}
    ${ATOMSPACE_LIBRARY_DIRS}
)

if(MOSES_FOUND)
    link_directories(${MOSES_LIBRARY_DIRS})
endif()

if(ASMOSES_FOUND)
    link_directories(${ASMOSES_LIBRARY_DIRS})
endif()

if(LEARN_FOUND)
    link_directories(${LEARN_LIBRARY_DIRS})
endif()

if(MINER_FOUND)
    link_directories(${MINER_LIBRARY_DIRS})
endif()

if(URE_FOUND)
    link_directories(${URE_LIBRARY_DIRS})
endif()

if(COGSERVER_FOUND)
    link_directories(${COGSERVER_LIBRARY_DIRS})
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

# Link required libraries
target_link_libraries(agentzero-learning
    ${COGUTIL_LIBRARIES}
    ${ATOMSPACE_LIBRARIES}
    ${Boost_LIBRARIES}
)

# Link optional libraries with proper feature flags
if(MOSES_FOUND AND MOSES_LIBRARIES)
    target_link_libraries(agentzero-learning ${MOSES_LIBRARIES})
    target_compile_definitions(agentzero-learning PRIVATE HAVE_MOSES=1 MOSES_INTEGRATION_ENABLED=1)
    message(STATUS "Linking MOSES libraries: ${MOSES_LIBRARIES}")
elseif(MOSES_FOUND)
    target_compile_definitions(agentzero-learning PRIVATE HAVE_MOSES=1 MOSES_INTEGRATION_ENABLED=1)
    message(STATUS "MOSES headers found, compiling with MOSES support")
endif()

if(ASMOSES_FOUND AND ASMOSES_LIBRARIES)
    target_link_libraries(agentzero-learning ${ASMOSES_LIBRARIES})
    target_compile_definitions(agentzero-learning PRIVATE HAVE_ASMOSES=1 ASMOSES_INTEGRATION_ENABLED=1)
    message(STATUS "Linking ASMOSES libraries: ${ASMOSES_LIBRARIES}")
elseif(ASMOSES_FOUND)
    target_compile_definitions(agentzero-learning PRIVATE HAVE_ASMOSES=1 ASMOSES_INTEGRATION_ENABLED=1)
    message(STATUS "ASMOSES headers found, compiling with ASMOSES support")
endif()

if(LEARN_FOUND)
    target_link_libraries(agentzero-learning ${LEARN_LIBRARIES})
    target_compile_definitions(agentzero-learning PRIVATE LEARN_INTEGRATION_ENABLED=1)
endif()

if(MINER_FOUND)
    target_link_libraries(agentzero-learning ${MINER_LIBRARIES})
    target_compile_definitions(agentzero-learning PRIVATE MINER_INTEGRATION_ENABLED=1)
endif()

if(URE_FOUND)
    target_link_libraries(agentzero-learning ${URE_LIBRARIES})
    target_compile_definitions(agentzero-learning PRIVATE URE_INTEGRATION_ENABLED=1)
endif()

if(COGSERVER_FOUND)
    target_link_libraries(agentzero-learning ${COGSERVER_LIBRARIES})
    target_compile_definitions(agentzero-learning PRIVATE HAVE_COGSERVER=1)
endif()

# Compiler flags
target_compile_definitions(agentzero-learning PRIVATE
    AGENTZERO_LEARNING_VERSION="${PROJECT_VERSION}"
)

target_include_directories(agentzero-learning PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

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
message(STATUS "Comprehensive Agent-Zero Learning Configuration:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  MOSES integration: ${MOSES_FOUND}")
message(STATUS "  ASMOSES integration: ${ASMOSES_FOUND}")
message(STATUS "  Learn integration: ${LEARN_FOUND}")
message(STATUS "  Miner integration: ${MINER_FOUND}")
message(STATUS "  URE integration: ${URE_FOUND}")
message(STATUS "  CogServer integration: ${COGSERVER_FOUND}")
message(STATUS "")
