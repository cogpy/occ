cmake_minimum_required(VERSION 3.16)
project(agent-zero-demonstrations VERSION 1.0.0)

# Set Guile as the primary language for these demos
message(STATUS "Configuring Agent-Zero Demonstration Scenarios")

# Find Guile (optional - demos can run without CMake build)
find_program(GUILE_EXECUTABLE guile)

if(GUILE_EXECUTABLE)
    message(STATUS "Found Guile: ${GUILE_EXECUTABLE}")
    
    # Get Guile version
    execute_process(
        COMMAND ${GUILE_EXECUTABLE} --version
        OUTPUT_VARIABLE GUILE_VERSION_OUTPUT
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    
    string(REGEX MATCH "([0-9]+\\.[0-9]+)" GUILE_VERSION "${GUILE_VERSION_OUTPUT}")
    message(STATUS "Guile version: ${GUILE_VERSION}")
else()
    message(WARNING "Guile not found. Demonstrations require Guile 3.0+")
endif()

# Install demonstration files
set(DEMO_FILES
    demo1_cognitive_loop.scm
    demo2_knowledge_integration.scm
    demo3_perception_action.scm
    demo4_goal_management.scm
    demo5_full_integration.scm
    run_all_demos.sh
    README.md
)

# Installation directory
set(DEMO_INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/share/opencog/agent-zero/demonstrations)

# Install demo files
install(
    FILES ${DEMO_FILES}
    DESTINATION ${DEMO_INSTALL_DIR}
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                GROUP_READ GROUP_EXECUTE
                WORLD_READ WORLD_EXECUTE
)

# Create a custom target to run demos (for testing)
if(GUILE_EXECUTABLE)
    add_custom_target(run-demonstrations
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/run_all_demos.sh
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Running Agent-Zero demonstration scenarios"
        VERBATIM
    )
    
    # Individual demo targets
    add_custom_target(run-demo1
        COMMAND ${GUILE_EXECUTABLE} -l demo1_cognitive_loop.scm
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Running Demo 1: Basic Cognitive Loop"
        VERBATIM
    )
    
    add_custom_target(run-demo2
        COMMAND ${GUILE_EXECUTABLE} -l demo2_knowledge_integration.scm
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Running Demo 2: Knowledge Integration"
        VERBATIM
    )
    
    add_custom_target(run-demo3
        COMMAND ${GUILE_EXECUTABLE} -l demo3_perception_action.scm
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Running Demo 3: Perception-Action Cycle"
        VERBATIM
    )
    
    add_custom_target(run-demo4
        COMMAND ${GUILE_EXECUTABLE} -l demo4_goal_management.scm
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Running Demo 4: Goal Management"
        VERBATIM
    )
    
    add_custom_target(run-demo5
        COMMAND ${GUILE_EXECUTABLE} -l demo5_full_integration.scm
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Running Demo 5: Full System Integration"
        VERBATIM
    )
endif()

# Display usage information
message(STATUS "")
message(STATUS "Agent-Zero Demonstrations configured")
message(STATUS "====================================")
message(STATUS "")
message(STATUS "To run demonstrations:")
message(STATUS "  All demos:    make run-demonstrations")
message(STATUS "  Individual:   make run-demo1, run-demo2, etc.")
message(STATUS "  Or directly:  ./run_all_demos.sh")
message(STATUS "")
message(STATUS "To install:     make install")
message(STATUS "Install dir:    ${DEMO_INSTALL_DIR}")
message(STATUS "")
