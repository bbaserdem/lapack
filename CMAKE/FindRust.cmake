# FindRust.cmake
# 
# Find the Rust compiler and cargo build tool
#
# This module defines:
#   RUST_FOUND         - True if Rust compiler is found
#   RUST_EXECUTABLE    - Path to rustc executable
#   CARGO_EXECUTABLE   - Path to cargo executable
#   RUST_VERSION       - Version of the Rust compiler
#   RUST_VERSION_MAJOR - Major version component
#   RUST_VERSION_MINOR - Minor version component
#   RUST_VERSION_PATCH - Patch version component
#
# This module also provides the following functions:
#   rust_add_library(TARGET_NAME MANIFEST_PATH [ARGS])
#     Creates a custom target to build a Rust library using cargo
#

# Find rustc executable
find_program(RUST_EXECUTABLE
    NAMES rustc
    DOC "Path to the Rust compiler"
)

# Find cargo executable
find_program(CARGO_EXECUTABLE
    NAMES cargo
    DOC "Path to the Cargo build tool"
)

# Get Rust version if rustc is found
if(RUST_EXECUTABLE)
    execute_process(
        COMMAND ${RUST_EXECUTABLE} --version
        OUTPUT_VARIABLE RUST_VERSION_STRING
        RESULT_VARIABLE RUST_VERSION_RESULT
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    
    if(RUST_VERSION_RESULT EQUAL 0)
        # Parse version string: "rustc 1.72.0 (5680fa18f 2023-08-23)"
        string(REGEX MATCH "rustc ([0-9]+)\\.([0-9]+)\\.([0-9]+)" 
               RUST_VERSION_MATCH "${RUST_VERSION_STRING}")
        
        if(RUST_VERSION_MATCH)
            set(RUST_VERSION "${CMAKE_MATCH_1}.${CMAKE_MATCH_2}.${CMAKE_MATCH_3}")
            set(RUST_VERSION_MAJOR ${CMAKE_MATCH_1})
            set(RUST_VERSION_MINOR ${CMAKE_MATCH_2})
            set(RUST_VERSION_PATCH ${CMAKE_MATCH_3})
        endif()
    endif()
endif()

# Check minimum version requirement
if(Rust_FIND_VERSION)
    if(RUST_VERSION VERSION_LESS Rust_FIND_VERSION)
        set(RUST_FOUND FALSE)
        if(Rust_FIND_REQUIRED)
            message(FATAL_ERROR "Rust version ${RUST_VERSION} is less than required ${Rust_FIND_VERSION}")
        endif()
    endif()
endif()

# Standard find package handling
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Rust
    REQUIRED_VARS RUST_EXECUTABLE CARGO_EXECUTABLE
    VERSION_VAR RUST_VERSION
    HANDLE_COMPONENTS
)

# Function to add a Rust library target
function(rust_add_library TARGET_NAME MANIFEST_PATH)
    set(options "")
    set(oneValueArgs CRATE_TYPE LIB_TYPE)
    set(multiValueArgs ARGS ENV_VARS)
    cmake_parse_arguments(RUST_LIB "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
    
    if(NOT EXISTS "${MANIFEST_PATH}")
        message(FATAL_ERROR "Cargo.toml not found at ${MANIFEST_PATH}")
    endif()
    
    # Get the directory containing Cargo.toml
    get_filename_component(RUST_SOURCE_DIR "${MANIFEST_PATH}" DIRECTORY)
    
    # Set build profile based on CMAKE_BUILD_TYPE
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(CARGO_BUILD_TYPE "debug")
        set(CARGO_BUILD_FLAG "")
    else()
        set(CARGO_BUILD_TYPE "release")
        set(CARGO_BUILD_FLAG "--release")
    endif()
    
    # Set target directory based on library type and host OS
    if(RUST_LIB_CRATE_TYPE STREQUAL "cdylib" OR RUST_LIB_LIB_TYPE STREQUAL "SHARED")
        if(WIN32)
            set(RUST_LIB_EXT "dll")
            set(RUST_LIB_PREFIX "")
        elseif(APPLE)
            set(RUST_LIB_EXT "dylib")
            set(RUST_LIB_PREFIX "lib")
        else()
            set(RUST_LIB_EXT "so")
            set(RUST_LIB_PREFIX "lib")
        endif()
        set(ACTUAL_LIB_TYPE "SHARED")
    else()
        # Static library
        if(WIN32)
            set(RUST_LIB_EXT "lib")
            set(RUST_LIB_PREFIX "")
        else()
            set(RUST_LIB_EXT "a")
            set(RUST_LIB_PREFIX "lib")
        endif()
        set(ACTUAL_LIB_TYPE "STATIC")
    endif()
    
    # Get the actual crate name from Cargo.toml
    if(EXISTS "${RUST_SOURCE_DIR}/Cargo.toml")
        file(READ "${RUST_SOURCE_DIR}/Cargo.toml" CARGO_TOML_CONTENT)
        string(REGEX MATCH "name = \"([^\"]+)\"" CRATE_NAME_MATCH "${CARGO_TOML_CONTENT}")
        if(CRATE_NAME_MATCH)
            set(ACTUAL_CRATE_NAME ${CMAKE_MATCH_1})
        else()
            # Fallback to target name
            set(ACTUAL_CRATE_NAME ${TARGET_NAME})
        endif()
    else()
        set(ACTUAL_CRATE_NAME ${TARGET_NAME})
    endif()
    
    # Set output library path using actual crate name
    set(RUST_LIB_OUTPUT_DIR "${RUST_SOURCE_DIR}/target/${CARGO_BUILD_TYPE}")
    set(RUST_LIB_OUTPUT "${RUST_LIB_OUTPUT_DIR}/${RUST_LIB_PREFIX}${ACTUAL_CRATE_NAME}.${RUST_LIB_EXT}")
    
    # Prepare environment variables for cargo build
    set(CARGO_ENV_COMMAND ${CMAKE_COMMAND} -E env)
    foreach(ENV_VAR ${RUST_LIB_ENV_VARS})
        list(APPEND CARGO_ENV_COMMAND "${ENV_VAR}")
    endforeach()
    
    # Create custom command to build the Rust library
    add_custom_command(
        OUTPUT ${RUST_LIB_OUTPUT}
        COMMAND ${CARGO_ENV_COMMAND} ${CARGO_EXECUTABLE} build ${CARGO_BUILD_FLAG} ${RUST_LIB_ARGS}
        WORKING_DIRECTORY ${RUST_SOURCE_DIR}
        COMMENT "Building Rust library ${TARGET_NAME}"
        VERBATIM
    )
    
    # Create custom target
    add_custom_target(${TARGET_NAME}_rust_build
        DEPENDS ${RUST_LIB_OUTPUT}
    )
    
    # Create imported target for the Rust library
    add_library(${TARGET_NAME} ${ACTUAL_LIB_TYPE} IMPORTED)
    set_target_properties(${TARGET_NAME} PROPERTIES
        IMPORTED_LOCATION ${RUST_LIB_OUTPUT}
    )
    
    # Make the imported target depend on the build target
    add_dependencies(${TARGET_NAME} ${TARGET_NAME}_rust_build)
    
    # Set additional properties
    if(ACTUAL_LIB_TYPE STREQUAL "SHARED")
        set_target_properties(${TARGET_NAME} PROPERTIES
            IMPORTED_IMPLIB ${RUST_LIB_OUTPUT}
        )
    endif()
endfunction()

mark_as_advanced(RUST_EXECUTABLE CARGO_EXECUTABLE)