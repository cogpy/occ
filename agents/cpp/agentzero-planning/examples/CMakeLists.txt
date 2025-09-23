cmake_minimum_required(VERSION 3.16)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${COGUTIL_INCLUDE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
    ${OCTOMAP_INCLUDE_DIRS}
)

if(SPACETIME_FOUND)
    include_directories(${SPACETIME_INCLUDE_DIR})
endif()

# Example executable
add_executable(spatial_temporal_example spatial_temporal_example.cpp)

# Link libraries
target_link_libraries(spatial_temporal_example
    agentzero-planning
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_LIBRARY}
    ${OCTOMAP_LIBRARIES}
)

if(SPACETIME_FOUND)
    target_link_libraries(spatial_temporal_example ${SPACETIME_LIBRARY})
endif()

# Install example
install(TARGETS spatial_temporal_example
    RUNTIME DESTINATION bin/examples
)