cmake_minimum_required(VERSION 3.16)

# === Examples ===

message(STATUS "Configuring Agent-Zero Perception Examples")

# Example demonstrating MultiModalSensor usage
set(BASIC_EXAMPLE_SOURCES
    basic_sensor_example.cpp
)

add_executable(basic_sensor_example ${BASIC_EXAMPLE_SOURCES})

target_link_libraries(basic_sensor_example
    agentzero-perception
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_atombase_LIBRARY}
    ${ATOMSPACE_atomspace_LIBRARY}
    ${ATOMSPACE_value_LIBRARY}
    /usr/local/lib/opencog/libsensory.so
    /usr/local/lib/opencog/libsensory-types.so
)

# Example demonstrating TextualSensor usage
set(TEXT_EXAMPLE_SOURCES
    textual_sensor_example.cpp
)

add_executable(textual_sensor_example ${TEXT_EXAMPLE_SOURCES})

target_link_libraries(textual_sensor_example
    agentzero-perception
    ${COGUTIL_LIBRARY}
    ${ATOMSPACE_atombase_LIBRARY}
    ${ATOMSPACE_atomspace_LIBRARY}
    ${ATOMSPACE_value_LIBRARY}
    /usr/local/lib/opencog/libsensory.so
    /usr/local/lib/opencog/libsensory-types.so
)

# Install examples (optional)
install(TARGETS basic_sensor_example textual_sensor_example
    RUNTIME DESTINATION share/opencog/examples/agentzero-perception
)

message(STATUS "Agent-Zero Perception Examples configured")