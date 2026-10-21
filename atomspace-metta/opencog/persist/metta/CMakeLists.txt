
# Generic MeTTa encoding/decoding.
ADD_LIBRARY(metta-io
	DecodeMeTTa.cc
	EncodeMeTTa.cc
)

# EncodeMeTTa.cc includes the generated opencog/atoms/metta-types/atom_types.h,
# so wait for it and look for it in the build directory.
ADD_DEPENDENCIES(metta-io metta_atom_types)
TARGET_INCLUDE_DIRECTORIES(metta-io PRIVATE ${CMAKE_BINARY_DIR})

TARGET_LINK_LIBRARIES(metta-io
	metta-types
	atomspace
)

INSTALL (TARGETS metta-io EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	MeTTa.h
	DESTINATION "include/opencog/atoms/metta"
)

# -------------------------------
