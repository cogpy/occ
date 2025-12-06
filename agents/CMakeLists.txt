cmake_minimum_required(VERSION 3.16)
project(agents VERSION 0.1.0 DESCRIPTION "OpenCog Agents Project")

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

message(STATUS "Configuring OpenCog Agents Project")

# Add C++ Agent-Zero implementation
add_subdirectory(cpp)

# Add specific Agent-Zero components
add_subdirectory(cpp/agentzero-perception)
add_subdirectory(cpp/agentzero-planning)  # AZ-SPATIAL-001: Added planning module
add_subdirectory(cpp/agentzero-memory)    # AZ-MEM-003: Added memory module
add_subdirectory(cpp/agentzero-memory)    # AZ-MEM-002: Added memory management module
add_subdirectory(cpp/agentzero-tools)     # AZ-TOOL-001: Added tools integration module

# Add demonstration scenarios (AZ-DEMO-001)
add_subdirectory(demonstrations)

# Future: Add other agent implementations here
# add_subdirectory(python)  # For Python-based agents
# add_subdirectory(scheme)  # For pure Scheme agents

message(STATUS "OpenCog Agents configuration complete")