# CMakeLists.txt for LAPACK Sparse Matrix implementation

# Sparse matrix modules
set(SPARSE_MODULES
    MODULES/sparse_types.f90
    MODULES/sparse_constants.f90
)

# COO format routines
set(SPARSE_COO
    COO/DCOOINIT.f90
    COO/DCOOCONV.f90
    COO/DCOOCHECK.f90
    COO/DCOOFREE.f90
)

# CSR format routines
set(SPARSE_CSR
    CSR/DCSRINIT.f90
    CSR/DCSRFREE.f90
)

# CSC format routines
set(SPARSE_CSC
    CSC/DCSCINIT.f90
    CSC/DCSCFREE.f90
)

# Create the sparse matrix library
add_library(lapack_sparse STATIC
    ${SPARSE_MODULES}
    ${SPARSE_COO}
    ${SPARSE_CSR}
    ${SPARSE_CSC}
)

# Set module directory for Fortran modules
set_target_properties(lapack_sparse PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules
)

# Include the module directory for consumers
target_include_directories(lapack_sparse PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>
    $<INSTALL_INTERFACE:include/lapack/sparse>
)

# Export the library
install(TARGETS lapack_sparse
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
)

# Install module files
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules/
    DESTINATION include/lapack/sparse
    FILES_MATCHING PATTERN "*.mod"
)