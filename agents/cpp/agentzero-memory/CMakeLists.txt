# Agent-Zero Memory Module CMakeLists.txt
# AZ-MEM-002: WorkingMemory management implementation

cmake_minimum_required(VERSION 3.16)

project(AgentZeroMemory
    VERSION 0.1.0
    DESCRIPTION "Agent-Zero Memory & Context Management Module"
    LANGUAGES CXX
)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

message(STATUS "Configuring Agent-Zero Memory & Context Management module...")

# Find required OpenCog packages
find_package(PkgConfig REQUIRED)

# Find cogutil (required)
pkg_check_modules(COGUTIL REQUIRED cogutil)
if (COGUTIL_FOUND)
    message(STATUS "Found cogutil version ${COGUTIL_VERSION}")
    include_directories(${COGUTIL_INCLUDE_DIRS})
    link_directories(${COGUTIL_LIBRARY_DIRS})
endif()

# Find atomspace (required)
pkg_check_modules(ATOMSPACE REQUIRED atomspace)
if (ATOMSPACE_FOUND)
    message(STATUS "Found atomspace version ${ATOMSPACE_VERSION}")
    include_directories(${ATOMSPACE_INCLUDE_DIRS})
    link_directories(${ATOMSPACE_LIBRARY_DIRS})
endif()

# Find atomspace-rocks (required for persistence)
pkg_check_modules(ATOMSPACE_ROCKS atomspace-rocks)
if (ATOMSPACE_ROCKS_FOUND)
    message(STATUS "Found atomspace-rocks version ${ATOMSPACE_ROCKS_VERSION}")
    include_directories(${ATOMSPACE_ROCKS_INCLUDE_DIRS})
    link_directories(${ATOMSPACE_ROCKS_LIBRARY_DIRS})
else()
    message(STATUS "atomspace-rocks not found via pkg-config, looking for system includes")
    find_path(ATOMSPACE_ROCKS_INCLUDE_PATH
        NAMES opencog/persist/rocks/RocksStorageNode.h
        PATHS /usr/local/include /usr/include
        PATH_SUFFIXES opencog
    )
    if(ATOMSPACE_ROCKS_INCLUDE_PATH)
        message(STATUS "Found atomspace-rocks headers at ${ATOMSPACE_ROCKS_INCLUDE_PATH}")
        include_directories(${ATOMSPACE_ROCKS_INCLUDE_PATH})
        set(ATOMSPACE_ROCKS_FOUND TRUE)
    else()
        message(STATUS "atomspace-rocks not found - persistence features may be limited")
        set(ATOMSPACE_ROCKS_FOUND FALSE)
    endif()
endif()

# Find attention module (required for ECAN integration)
pkg_check_modules(ATTENTION attention)
if (ATTENTION_FOUND)
    message(STATUS "Found attention version ${ATTENTION_VERSION}")
    include_directories(${ATTENTION_INCLUDE_DIRS})
    link_directories(${ATTENTION_LIBRARY_DIRS})
else()
    message(STATUS "Attention module not found via pkg-config, looking for system includes")
    find_path(ATTENTION_INCLUDE_PATH
        NAMES opencog/attentionbank/bank/AttentionBank.h
        PATHS /usr/local/include /usr/include
        PATH_SUFFIXES opencog
    )
    if(ATTENTION_INCLUDE_PATH)
        message(STATUS "Found attention headers at ${ATTENTION_INCLUDE_PATH}")
        include_directories(${ATTENTION_INCLUDE_PATH})
        set(ATTENTION_FOUND TRUE)
    else()
        message(STATUS "Attention module not found - attention-based memory retention disabled")
        set(ATTENTION_FOUND FALSE)
    endif()
endif()

# Find cogserver (optional)
pkg_check_modules(COGSERVER cogserver)
if (COGSERVER_FOUND)
    message(STATUS "Found cogserver version ${COGSERVER_VERSION}")
    include_directories(${COGSERVER_INCLUDE_DIRS})
    link_directories(${COGSERVER_LIBRARY_DIRS})
else()
    message(STATUS "CogServer not found - some features will be disabled")
    set(COGSERVER_FOUND FALSE)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# Source files
set(AGENTZERO_MEMORY_SOURCES
    src/WorkingMemory.cpp
    src/EpisodicMemory.cpp
    src/LongTermMemory.cpp
    src/ContextManager.cpp
)

# Header files
set(AGENTZERO_MEMORY_HEADERS
    include/opencog/agentzero/WorkingMemory.h
    include/opencog/agentzero/EpisodicMemory.h
    include/opencog/agentzero/LongTermMemory.h
    include/opencog/agentzero/ContextManager.h
)

# Create library (when source files exist)
if(AGENTZERO_MEMORY_SOURCES)
    add_library(agentzero-memory SHARED
        ${AGENTZERO_MEMORY_SOURCES}
        ${AGENTZERO_MEMORY_HEADERS}
    )

    target_link_libraries(agentzero-memory
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
        ${Boost_LIBRARIES}
    )
    
    # Link atomspace-rocks libraries if available
    if(ATOMSPACE_ROCKS_FOUND AND ATOMSPACE_ROCKS_LIBRARIES)
        target_link_libraries(agentzero-memory ${ATOMSPACE_ROCKS_LIBRARIES})
        target_compile_definitions(agentzero-memory PRIVATE HAVE_ATOMSPACE_ROCKS=1)
        message(STATUS "Linking atomspace-rocks libraries: ${ATOMSPACE_ROCKS_LIBRARIES}")
    elseif(ATOMSPACE_ROCKS_FOUND)
        target_compile_definitions(agentzero-memory PRIVATE HAVE_ATOMSPACE_ROCKS=1)
        message(STATUS "atomspace-rocks headers found, compiling with rocks support")
    endif()
    
    # Link attention libraries if available
    if(ATTENTION_FOUND AND ATTENTION_LIBRARIES)
        target_link_libraries(agentzero-memory ${ATTENTION_LIBRARIES})
        target_compile_definitions(agentzero-memory PRIVATE HAVE_ATTENTION_BANK=1)
        message(STATUS "Linking attention libraries: ${ATTENTION_LIBRARIES}")
    elseif(ATTENTION_FOUND)
        target_compile_definitions(agentzero-memory PRIVATE HAVE_ATTENTION_BANK=1)
        message(STATUS "Attention headers found, compiling with attention support")
    endif()
    
    # Link cogserver libraries if available
    if(COGSERVER_FOUND AND COGSERVER_LIBRARIES)
        target_link_libraries(agentzero-memory ${COGSERVER_LIBRARIES})
        target_compile_definitions(agentzero-memory PRIVATE HAVE_COGSERVER=1)
        message(STATUS "Linking cogserver libraries: ${COGSERVER_LIBRARIES}")
    endif()

    target_include_directories(agentzero-memory PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
    )

    # Installation
    install(TARGETS agentzero-memory
        EXPORT AgentZeroCppTargets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        INCLUDES DESTINATION include
    )

    install(DIRECTORY include/
        DESTINATION include
        FILES_MATCHING PATTERN "*.h"
    )
endif()

# Tests (when implemented)
if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()

# Examples (when implemented)
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Status message
message(STATUS "Agent-Zero Memory module configuration complete")
message(STATUS "Dependencies:")
message(STATUS "  cogutil: ${COGUTIL_VERSION}")
message(STATUS "  atomspace: ${ATOMSPACE_VERSION}")
message(STATUS "  atomspace-rocks: ${ATOMSPACE_ROCKS_FOUND}")
message(STATUS "  attention: ${ATTENTION_FOUND}")
message(STATUS "  cogserver: ${COGSERVER_FOUND}")