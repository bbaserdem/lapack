# CMakeLists.txt for LAPACK Sparse Matrix implementation
cmake_minimum_required(VERSION 3.10)
project(lapack_sparse Fortran)

# Enable Fortran language
enable_language(Fortran)

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
    COO/DCOO2CSC.f90
    COO/DDEN2COO.f90
    COO/DCOO2DEN.f90
    COO/DCOOMV.f90
    COO/DCOOTRANS.f90
)

# CSR format routines
set(SPARSE_CSR
    CSR/DCSRINIT.f90
    CSR/DCSRFREE.f90
    CSR/DCSR2COO.f90
    CSR/DCSRMV.f90
    CSR/DCSRCSC.f90
    CSR/DGECSR.f90
)

# CSC format routines
set(SPARSE_CSC
    CSC/DCSCINIT.f90
    CSC/DCSCFREE.f90
    CSC/DCSC2COO.f90
    CSC/DCSCMV.f90
)

# I/O routines
set(SPARSE_IO
    IO/DSPREAD.f90
    IO/DSPWRITE.f90
)

# Utility routines
set(SPARSE_UTILS
    UTILS/LSAME.f90
    UTILS/DSPGET.f90
    UTILS/DSPSET.f90
    UTILS/DSPADD2.f90
    UTILS/DSPSORT.f90
    UTILS/DSPCOMP.f90
    UTILS/DSPSIZE.f90
)

# Create the sparse matrix library
add_library(lapack_sparse STATIC
    ${SPARSE_MODULES}
    ${SPARSE_COO}
    ${SPARSE_CSR}
    ${SPARSE_CSC}
    ${SPARSE_IO}
    ${SPARSE_UTILS}
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

# Add testing subdirectory if BUILD_TESTING is enabled
if(BUILD_TESTING)
    add_subdirectory(TESTING)
endif()