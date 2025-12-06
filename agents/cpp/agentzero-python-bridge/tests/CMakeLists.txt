# Test CMakeLists.txt for Agent-Zero Python Bridge

cmake_minimum_required(VERSION 3.16)

# Find Python for running tests
find_package(Python3 COMPONENTS Interpreter REQUIRED)

# Add test for Python module import
add_test(
    NAME test_python_import
    COMMAND ${Python3_EXECUTABLE} -c "import opencog.agentzero; print('Import successful')"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# Add unit tests
add_test(
    NAME test_agentzero_unit
    COMMAND ${Python3_EXECUTABLE} -m pytest test_agentzero.py -v
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# Set test properties
set_tests_properties(test_python_import PROPERTIES
    LABELS "python;import"
)

set_tests_properties(test_agentzero_unit PROPERTIES
    LABELS "python;unit"
    DEPENDS test_python_import
)

message(STATUS "Agent-Zero Python Bridge tests configured")
