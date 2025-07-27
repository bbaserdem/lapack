# CMakeLists.txt for LAPACK Sparse Matrix testing

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