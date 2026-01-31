# CMakeLists.txt for Inferno AGI OS Kernel Modules
# Cross-platform build system for cognitive kernel services

cmake_minimum_required(VERSION 3.16)
project(InfernoAGIOS VERSION 0.1.0 LANGUAGES C CXX)

# C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Options
option(BUILD_KERNEL_MODULES "Build kernel-level modules" ON)
option(BUILD_USERSPACE "Build userspace libraries" ON)
option(BUILD_TESTS "Build test suite" ON)
option(ENABLE_DISTRIBUTED "Enable distributed cognitive features" ON)

# Platform detection
if(WIN32)
    set(PLATFORM_WINDOWS TRUE)
    add_definitions(-DPLATFORM_WINDOWS -D_WIN32_WINNT=0x0A00)
elseif(UNIX AND NOT APPLE)
    set(PLATFORM_LINUX TRUE)
    add_definitions(-DPLATFORM_LINUX)
elseif(APPLE)
    set(PLATFORM_MACOS TRUE)
    add_definitions(-DPLATFORM_MACOS)
endif()

# Find dependencies
find_package(Threads REQUIRED)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel/atomspace
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel/reasoning
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel/attention
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel/cognitive9p
    ${CMAKE_CURRENT_SOURCE_DIR}/../cogutil/opencog/util
)

# Kernel-compatible source files (portable C implementation)
set(KERNEL_SOURCES
    kernel/atomspace/atomspace_portable.c
    kernel/reasoning/reasoning_portable.c
    kernel/attention/attention_portable.c
    kernel/cognitive9p/cognitive9p_portable.c
)

# Userspace library sources
set(USERSPACE_SOURCES
    userspace/cogfs_client.c
    userspace/atomspace_client.c
    userspace/reasoning_client.c
)

# Create portable kernel library
if(BUILD_KERNEL_MODULES)
    add_library(inferno_agi_kernel STATIC ${KERNEL_SOURCES})
    target_link_libraries(inferno_agi_kernel Threads::Threads)
    
    if(WIN32)
        target_link_libraries(inferno_agi_kernel ws2_32)
    endif()
endif()

# Create userspace library
if(BUILD_USERSPACE)
    add_library(inferno_agi_user SHARED ${USERSPACE_SOURCES})
    target_link_libraries(inferno_agi_user inferno_agi_kernel Threads::Threads)
    
    if(WIN32)
        target_link_libraries(inferno_agi_user ws2_32)
    endif()
endif()

# Build tests
if(BUILD_TESTS)
    enable_testing()
    
    add_executable(test_atomspace kernel/test/test_atomspace.c)
    target_link_libraries(test_atomspace inferno_agi_kernel)
    add_test(NAME AtomSpaceTest COMMAND test_atomspace)
    
    add_executable(test_reasoning kernel/test/test_reasoning.c)
    target_link_libraries(test_reasoning inferno_agi_kernel)
    add_test(NAME ReasoningTest COMMAND test_reasoning)
    
    add_executable(test_cognitive9p kernel/test/test_cognitive9p.c)
    target_link_libraries(test_cognitive9p inferno_agi_kernel)
    add_test(NAME Cognitive9PTest COMMAND test_cognitive9p)
endif()

# Installation
install(TARGETS inferno_agi_kernel
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
)

if(BUILD_USERSPACE)
    install(TARGETS inferno_agi_user
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin
    )
endif()

install(FILES
    kernel/atomspace/atomspace.h
    kernel/reasoning/reasoning.h
    kernel/attention/attention.h
    kernel/cognitive9p/cognitive9p.h
    DESTINATION include/inferno-agi
)

# Print configuration summary
message(STATUS "")
message(STATUS "Inferno AGI OS Configuration:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  Platform: ${CMAKE_SYSTEM_NAME}")
message(STATUS "  Build kernel modules: ${BUILD_KERNEL_MODULES}")
message(STATUS "  Build userspace: ${BUILD_USERSPACE}")
message(STATUS "  Build tests: ${BUILD_TESTS}")
message(STATUS "  Distributed features: ${ENABLE_DISTRIBUTED}")
message(STATUS "")
