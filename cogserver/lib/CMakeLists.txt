# Configure and install CogServer package config files

set(ConfigPackageLocation lib/cmake/CogServer)

# Export the targets (assuming CogServerTargets is defined in main CMakeLists.txt)
install(EXPORT CogServerTargets
	FILE CogServerTargets.cmake
	DESTINATION ${ConfigPackageLocation}
)

include(CMakePackageConfigHelpers)

# Configure the package config file
configure_package_config_file(CogServerConfig.cmake.in
	${CMAKE_CURRENT_BINARY_DIR}/CogServerConfig.cmake
	INSTALL_DESTINATION ${ConfigPackageLocation}
	PATH_VARS CMAKE_INSTALL_PREFIX
)

# Create package version file
write_basic_package_version_file(
	"${CMAKE_CURRENT_BINARY_DIR}/CogServerConfigVersion.cmake"
	VERSION ${SEMANTIC_VERSION}
	COMPATIBILITY SameMajorVersion
)

# Install the config files
INSTALL(FILES
	${CMAKE_CURRENT_BINARY_DIR}/CogServerConfigVersion.cmake
	${CMAKE_CURRENT_BINARY_DIR}/CogServerConfig.cmake
	DESTINATION ${ConfigPackageLocation}
)
