# CMakeLists.txt for LAPACK Sparse Matrix implementation
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

# COO format routines - Single precision
set(SPARSE_COO_S
    COO/SCOOINIT.f90
    COO/SCOOCONV.f90
    COO/SCOOCHECK.f90
    COO/SCOOFREE.f90
    COO/SCOO2CSC.f90
    COO/SDEN2COO.f90
    COO/SCOO2DEN.f90
    COO/SCOOMV.f90
    COO/SCOOTRANS.f90
)

# COO format routines - Complex precision
set(SPARSE_COO_C
    COO/CCOOINIT.f90
    COO/CCOOCONV.f90
    COO/CCOOCHECK.f90
    COO/CCOOFREE.f90
    COO/CCOO2CSC.f90
    COO/CDEN2COO.f90
    COO/CCOO2DEN.f90
    COO/CCOOMV.f90
    COO/CCOOTRANS.f90
)

# COO format routines - Double complex precision
set(SPARSE_COO_Z
    COO/ZCOOINIT.f90
    COO/ZCOOCONV.f90
    COO/ZCOOCHECK.f90
    COO/ZCOOFREE.f90
    COO/ZCOO2CSC.f90
    COO/ZDEN2COO.f90
    COO/ZCOO2DEN.f90
    COO/ZCOOMV.f90
    COO/ZCOOTRANS.f90
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

# CSR format routines - Single precision
set(SPARSE_CSR_S
    CSR/SCSRINIT.f90
    CSR/SCSRFREE.f90
    CSR/SCSR2COO.f90
    CSR/SCSRMV.f90
    CSR/SCSRCSC.f90
    CSR/SGECSR.f90
)

# CSR format routines - Complex precision
set(SPARSE_CSR_C
    CSR/CCSRINIT.f90
    CSR/CCSRFREE.f90
    CSR/CCSR2COO.f90
    CSR/CCSRMV.f90
    CSR/CCSRCSC.f90
    CSR/CGECSR.f90
)

# CSR format routines - Double complex precision
set(SPARSE_CSR_Z
    CSR/ZCSRINIT.f90
    CSR/ZCSRFREE.f90
    CSR/ZCSR2COO.f90
    CSR/ZCSRMV.f90
    CSR/ZCSRCSC.f90
    CSR/ZGECSR.f90
)

# CSC format routines - Double precision
set(SPARSE_CSC_D
    CSC/DCSCINIT.f90
    CSC/DCSCFREE.f90
    CSC/DCSC2COO.f90
    CSC/DCSCMV.f90
)

# CSC format routines - Single precision
set(SPARSE_CSC_S
    CSC/SCSCINIT.f90
    CSC/SCSCFREE.f90
    CSC/SCSC2COO.f90
    CSC/SCSCMV.f90
)

# CSC format routines - Complex precision
set(SPARSE_CSC_C
    CSC/CCSCINIT.f90
    CSC/CCSCFREE.f90
    CSC/CCSC2COO.f90
    CSC/CCSCMV.f90
)

# CSC format routines - Double complex precision
set(SPARSE_CSC_Z
    CSC/ZCSCINIT.f90
    CSC/ZCSCFREE.f90
    CSC/ZCSC2COO.f90
    CSC/ZCSCMV.f90
)

# I/O routines - Double precision
set(SPARSE_IO_D
    IO/DSPREAD.f90
    IO/DSPWRITE.f90
)

# I/O routines - Single precision
set(SPARSE_IO_S
    IO/SSPREAD.f90
    IO/SSPWRITE.f90
)

# I/O routines - Complex precision
set(SPARSE_IO_C
    IO/CSPREAD.f90
    IO/CSPWRITE.f90
)

# I/O routines - Double complex precision
set(SPARSE_IO_Z
    IO/ZSPREAD.f90
    IO/ZSPWRITE.f90
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

# Utility routines - Single precision
set(SPARSE_UTILS_S
    UTILS/SSPGET.f90
    UTILS/SSPSET.f90
    UTILS/SSPADD2.f90
    UTILS/SSPSORT.f90
    UTILS/SSPCOMP.f90
    UTILS/SSPSIZE.f90
)

# Utility routines - Complex precision
set(SPARSE_UTILS_C
    UTILS/CSPGET.f90
    UTILS/CSPSET.f90
    UTILS/CSPADD2.f90
    UTILS/CSPSORT.f90
    UTILS/CSPCOMP.f90
    UTILS/CSPSIZE.f90
)

# Utility routines - Double complex precision
set(SPARSE_UTILS_Z
    UTILS/ZSPGET.f90
    UTILS/ZSPSET.f90
    UTILS/ZSPADD2.f90
    UTILS/ZSPSORT.f90
    UTILS/ZSPCOMP.f90
    UTILS/ZSPSIZE.f90
)

# Create the sparse matrix library
add_library(lapack_sparse STATIC
    ${SPARSE_MODULES}
    ${SPARSE_UTILS_COMMON}
    # Double precision
    ${SPARSE_COO_D}
    ${SPARSE_CSR_D}
    ${SPARSE_CSC_D}
    ${SPARSE_IO_D}
    ${SPARSE_UTILS_D}
    # Single precision
    ${SPARSE_COO_S}
    ${SPARSE_CSR_S}
    ${SPARSE_CSC_S}
    ${SPARSE_IO_S}
    ${SPARSE_UTILS_S}
    # Complex precision
    ${SPARSE_COO_C}
    ${SPARSE_CSR_C}
    ${SPARSE_CSC_C}
    ${SPARSE_IO_C}
    ${SPARSE_UTILS_C}
    # Double complex precision
    ${SPARSE_COO_Z}
    ${SPARSE_CSR_Z}
    ${SPARSE_CSC_Z}
    ${SPARSE_IO_Z}
    ${SPARSE_UTILS_Z}
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