# CMakeLists.txt for LAPACK Sparse Matrix implementation - Double precision only
cmake_minimum_required(VERSION 3.10)
project(lapack_sparse Fortran)

# Enable Fortran language
enable_language(Fortran)

# Sparse matrix modules
set(SPARSE_MODULES
    MODULES/sparse_types.f90
    MODULES/sparse_types_extended.f90
    MODULES/sparse_constants.f90
)

# COO format routines - Double precision
set(SPARSE_COO_D
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

# CSR format routines - Double precision
set(SPARSE_CSR_D
    CSR/DCSRINIT.f90
    CSR/DCSRFREE.f90
    CSR/DCSR2COO.f90
    CSR/DCSRMV.f90
    CSR/DCSRCSC.f90
    CSR/DGECSR.f90
)

# CSC format routines - Double precision
set(SPARSE_CSC_D
    CSC/DCSCINIT.f90
    CSC/DCSCFREE.f90
    CSC/DCSC2COO.f90
    CSC/DCSCMV.f90
)

# I/O routines - Double precision
set(SPARSE_IO_D
    IO/DSPREAD.f90
    IO/DSPWRITE.f90
)

# Utility routines - Common (precision-independent)
set(SPARSE_UTILS_COMMON
    UTILS/LSAME.f90
)

# Utility routines - Double precision
set(SPARSE_UTILS_D
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
    ${SPARSE_UTILS_COMMON}
    ${SPARSE_COO_D}
    ${SPARSE_CSR_D}
    ${SPARSE_CSC_D}
    ${SPARSE_IO_D}
    ${SPARSE_UTILS_D}
)

# Test programs - Double precision
add_executable(dchkaa_sparse
    TESTING/dchkaa_sparse.f90
    TESTING/dchksp.f90
    TESTING/derrsp.f90
    TESTING/dlatsp.f90
    TESTING/dsptests.f90
    TESTING/dtimsp.f90
    TESTING/dspedge.f90
    TESTING/dspnum.f90
    TESTING/dspmem.f90
)

target_link_libraries(dchkaa_sparse lapack_sparse)

# Additional test programs
add_executable(dchksp TESTING/dchksp.f90 TESTING/dsptests.f90)
target_link_libraries(dchksp lapack_sparse)

add_executable(dtimsp TESTING/dtimsp.f90)
target_link_libraries(dtimsp lapack_sparse)

add_executable(dspedge TESTING/dspedge.f90)
target_link_libraries(dspedge lapack_sparse)

add_executable(dspnum TESTING/dspnum.f90)
target_link_libraries(dspnum lapack_sparse)

add_executable(dspmem TESTING/dspmem.f90)
target_link_libraries(dspmem lapack_sparse)

# Set compiler flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -std=f2008 -fall-intrinsics")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -fcheck=all -fbacktrace")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")

# Install targets
install(TARGETS lapack_sparse DESTINATION lib)
install(FILES ${CMAKE_BINARY_DIR}/*.mod DESTINATION include OPTIONAL)