# CMakeLists.txt for LAPACK Sparse Matrix testing

# Test utility modules
add_library(sparse_test_utils STATIC
    dsptests.f90
    dlatsp.f90
    slatsp.f90
    chkxer.f90
)
target_link_libraries(sparse_test_utils lapack_sparse)

# Test programs
add_executable(test_conversions test_conversions.f90)
add_executable(test_spmv test_spmv.f90)
add_executable(test_matrices test_matrices.f90)
# Temporarily disable BLAS-dependent tests
# add_executable(test_spmv_accuracy test_spmv_accuracy.f90)
add_executable(test_io test_io.f90)
# add_executable(benchmark_sparse benchmark_sparse.f90)
add_executable(debug_spmv debug_spmv.f90)
add_executable(debug_csr debug_csr.f90)
add_executable(test_advanced_operations test_advanced_operations.f90)
add_executable(test_precision_variants test_precision_variants.f90)

# LAPACK-style test programs
add_executable(dchkaa_sparse dchkaa_sparse.f90)
add_executable(dchksp dchksp.f90)
add_executable(derrsp derrsp.f90)
add_executable(schksp schksp.f90)
add_executable(dtimsp dtimsp.f90)
add_executable(dspedge dspedge.f90)
add_executable(dspnum dspnum.f90)
add_executable(dspmem dspmem.f90)

# Link with sparse matrix library and BLAS
target_link_libraries(test_conversions lapack_sparse)
target_link_libraries(test_spmv lapack_sparse)
target_link_libraries(test_matrices lapack_sparse)
# target_link_libraries(test_spmv_accuracy lapack_sparse blas)
target_link_libraries(test_io lapack_sparse)
# target_link_libraries(benchmark_sparse lapack_sparse blas)
target_link_libraries(debug_spmv lapack_sparse)
target_link_libraries(debug_csr lapack_sparse)
target_link_libraries(test_advanced_operations lapack_sparse)
target_link_libraries(test_precision_variants lapack_sparse)

# Link LAPACK-style tests
target_link_libraries(dchkaa_sparse lapack_sparse sparse_test_utils)
target_link_libraries(dchksp lapack_sparse sparse_test_utils)
target_link_libraries(derrsp lapack_sparse)
target_link_libraries(schksp lapack_sparse sparse_test_utils)
target_link_libraries(dtimsp lapack_sparse)
target_link_libraries(dspedge lapack_sparse)
target_link_libraries(dspnum lapack_sparse)
target_link_libraries(dspmem lapack_sparse)

# Enable testing
enable_testing()

# Add tests
add_test(NAME sparse_conversions 
         COMMAND test_conversions)

add_test(NAME sparse_spmv
         COMMAND test_spmv)

add_test(NAME sparse_matrices
         COMMAND test_matrices)

# add_test(NAME sparse_spmv_accuracy
#          COMMAND test_spmv_accuracy)

add_test(NAME sparse_io
         COMMAND test_io)

add_test(NAME sparse_advanced_operations
         COMMAND test_advanced_operations)

add_test(NAME sparse_precision_variants
         COMMAND test_precision_variants)

# Add LAPACK-style tests
add_test(NAME sparse_lapack_driver
         COMMAND dchkaa_sparse)

add_test(NAME sparse_error_exits
         COMMAND derrsp)

add_test(NAME sparse_timing
         COMMAND dtimsp)

add_test(NAME sparse_edge_cases
         COMMAND dspedge)

add_test(NAME sparse_numerical_accuracy
         COMMAND dspnum)

add_test(NAME sparse_memory_management
         COMMAND dspmem)

# Set test properties
set_tests_properties(sparse_conversions PROPERTIES
    PASS_REGULAR_EXPRESSION "ALL TESTS PASSED!"
    FAIL_REGULAR_EXPRESSION "FAILED|Error"
)

set_tests_properties(sparse_spmv PROPERTIES
    PASS_REGULAR_EXPRESSION "Tests passed: [0-9]+ out of [0-9]+"
    FAIL_REGULAR_EXPRESSION "FAILED|ERROR"
)

set_tests_properties(sparse_matrices PROPERTIES
    PASS_REGULAR_EXPRESSION "Tests passed: [0-9]+ out of [0-9]+"
    FAIL_REGULAR_EXPRESSION "FAILED|Error"
)

# set_tests_properties(sparse_spmv_accuracy PROPERTIES
#     PASS_REGULAR_EXPRESSION "Tests passed: [0-9]+ out of [0-9]+"
#     FAIL_REGULAR_EXPRESSION "FAILED|Error"
# )

set_tests_properties(sparse_io PROPERTIES
    PASS_REGULAR_EXPRESSION "Tests passed: [0-9]+ out of [0-9]+"
    FAIL_REGULAR_EXPRESSION "FAILED|ERROR"
)

set_tests_properties(sparse_advanced_operations PROPERTIES
    PASS_REGULAR_EXPRESSION "Tests: [0-9]+ passed"
    FAIL_REGULAR_EXPRESSION "failed|FAILED|Error"
)

set_tests_properties(sparse_precision_variants PROPERTIES
    PASS_REGULAR_EXPRESSION "Total Tests: [0-9]+ passed"
    FAIL_REGULAR_EXPRESSION "failed|FAILED|Error"
)

# Properties for LAPACK-style tests
set_tests_properties(sparse_lapack_driver PROPERTIES
    PASS_REGULAR_EXPRESSION "All tests passed"
    FAIL_REGULAR_EXPRESSION "FAILED|tests failed"
)

set_tests_properties(sparse_error_exits PROPERTIES
    PASS_REGULAR_EXPRESSION "routines passed the tests of the error exits"
    FAIL_REGULAR_EXPRESSION "failed the tests"
)

set_tests_properties(sparse_timing PROPERTIES
    PASS_REGULAR_EXPRESSION "Sparse Matrix Performance Tests"
    FAIL_REGULAR_EXPRESSION "ERROR|FAILED"
)

set_tests_properties(sparse_edge_cases PROPERTIES
    PASS_REGULAR_EXPRESSION "All edge case tests PASSED"
    FAIL_REGULAR_EXPRESSION "FAILED"
)

set_tests_properties(sparse_numerical_accuracy PROPERTIES
    PASS_REGULAR_EXPRESSION "All numerical accuracy tests PASSED"
    FAIL_REGULAR_EXPRESSION "FAILED"
)

set_tests_properties(sparse_memory_management PROPERTIES
    PASS_REGULAR_EXPRESSION "All memory management tests PASSED"
    FAIL_REGULAR_EXPRESSION "FAILED"
)