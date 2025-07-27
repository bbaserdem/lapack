/*
 * LAPACKE Sparse Matrix Example - COO Format
 * 
 * This example demonstrates the use of LAPACKE sparse matrix
 * functions with COO (Coordinate List) format.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lapacke.h"

#define M 4
#define N 4
#define NNZ 6

int main() {
    /* Sparse matrix in COO format:
     * [1.0  0.0  2.0  0.0]
     * [0.0  3.0  0.0  4.0]
     * [5.0  0.0  6.0  0.0]
     * [0.0  7.0  0.0  8.0]
     */
    
    /* Input COO arrays (1-based indexing) */
    lapack_int row_ind[NNZ] = {1, 1, 2, 2, 3, 3, 4, 4};
    lapack_int col_ind[NNZ] = {1, 3, 2, 4, 1, 3, 2, 4};
    double values[NNZ] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0};
    
    /* Vectors for matrix-vector multiplication */
    double x[N] = {1.0, 2.0, 3.0, 4.0};
    double y[M] = {0.0, 0.0, 0.0, 0.0};
    
    /* Dense matrix for conversion */
    double dense[M*N];
    
    /* COO matrix pointers */
    double *coo_values = NULL;
    lapack_int *coo_row_ind = NULL;
    lapack_int *coo_col_ind = NULL;
    
    lapack_int info;
    
    printf("LAPACKE Sparse Matrix Example - COO Format\n");
    printf("==========================================\n\n");
    
    /* Initialize COO matrix */
    printf("1. Initializing COO matrix...\n");
    info = LAPACKE_dcooinit(M, N, NNZ, row_ind, col_ind, values, 
                            &coo_values, &coo_row_ind, &coo_col_ind);
    
    if (info != 0) {
        printf("Error: COO initialization failed with info = %d\n", info);
        return 1;
    }
    printf("   COO matrix initialized successfully\n\n");
    
    /* Convert COO to dense format */
    printf("2. Converting COO to dense format...\n");
    info = LAPACKE_dcoo2den(LAPACK_ROW_MAJOR, M, N, NNZ, coo_values,
                            coo_row_ind, coo_col_ind, dense, N);
    
    if (info != 0) {
        printf("Error: COO to dense conversion failed with info = %d\n", info);
        LAPACKE_dcoofree(coo_values, coo_row_ind, coo_col_ind);
        return 1;
    }
    
    printf("   Dense matrix:\n");
    for (int i = 0; i < M; i++) {
        printf("   ");
        for (int j = 0; j < N; j++) {
            printf("%6.1f ", dense[i*N + j]);
        }
        printf("\n");
    }
    printf("\n");
    
    /* Perform matrix-vector multiplication: y = A * x */
    printf("3. Performing matrix-vector multiplication y = A * x...\n");
    printf("   Input vector x: [");
    for (int i = 0; i < N; i++) {
        printf("%.1f", x[i]);
        if (i < N-1) printf(", ");
    }
    printf("]\n");
    
    info = LAPACKE_dcoomv('N', M, N, 1.0, NNZ, coo_values, coo_row_ind, coo_col_ind,
                          x, 1, 0.0, y, 1);
    
    if (info != 0) {
        printf("Error: Matrix-vector multiplication failed with info = %d\n", info);
        LAPACKE_dcoofree(coo_values, coo_row_ind, coo_col_ind);
        return 1;
    }
    
    printf("   Result vector y: [");
    for (int i = 0; i < M; i++) {
        printf("%.1f", y[i]);
        if (i < M-1) printf(", ");
    }
    printf("]\n\n");
    
    /* Perform transpose matrix-vector multiplication: y = A^T * x */
    printf("4. Performing transpose matrix-vector multiplication y = A^T * x...\n");
    double y_trans[N] = {0.0, 0.0, 0.0, 0.0};
    
    info = LAPACKE_dcoomv('T', M, N, 1.0, NNZ, coo_values, coo_row_ind, coo_col_ind,
                          x, 1, 0.0, y_trans, 1);
    
    if (info != 0) {
        printf("Error: Transpose matrix-vector multiplication failed with info = %d\n", info);
        LAPACKE_dcoofree(coo_values, coo_row_ind, coo_col_ind);
        return 1;
    }
    
    printf("   Result vector y: [");
    for (int i = 0; i < N; i++) {
        printf("%.1f", y_trans[i]);
        if (i < N-1) printf(", ");
    }
    printf("]\n\n");
    
    /* Clean up */
    printf("5. Cleaning up...\n");
    info = LAPACKE_dcoofree(coo_values, coo_row_ind, coo_col_ind);
    printf("   Memory freed successfully\n\n");
    
    printf("Example completed successfully!\n");
    
    return 0;
}