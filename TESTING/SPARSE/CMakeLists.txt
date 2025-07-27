# CMakeLists.txt for LAPACK Sparse Matrix testing

# Test for sparse matrix initialization and basic operations
add_executable(test_sparse_init test_sparse_init.f90)

# Link with sparse matrix library
target_link_libraries(test_sparse_init PRIVATE lapack_sparse)

# Add test
add_test(NAME sparse_init_test COMMAND test_sparse_init)