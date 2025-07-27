/*****************************************************************************
  Copyright (c) 2014, Intel Corp.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Intel Corporation nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
  THE POSSIBILITY OF SUCH DAMAGE.
*****************************************************************************
* Contents: Native C interface to LAPACK sparse matrix functions
* Author: Intel Corporation
*****************************************************************************/

#ifndef _LAPACKE_SPARSE_H_
#define _LAPACKE_SPARSE_H_

#include "lapacke.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Sparse matrix constants */
#define LAPACKE_SPARSE_SUCCESS       0
#define LAPACKE_SPARSE_ERR_ALLOC    -1001
#define LAPACKE_SPARSE_ERR_DIM      -1002
#define LAPACKE_SPARSE_ERR_INDEX    -1003

/* Sparse matrix formats */
#define LAPACKE_SPARSE_COO          1
#define LAPACKE_SPARSE_CSR          2  
#define LAPACKE_SPARSE_CSC          3

/* COO (Coordinate List) format functions */

/* Initialize COO matrix from arrays */
lapack_int LAPACKE_dcooinit( lapack_int nrows, lapack_int ncols, lapack_int nnz,
                             const lapack_int* row_ind, const lapack_int* col_ind,
                             const double* values, double** coo_values, 
                             lapack_int** coo_row_ind, lapack_int** coo_col_ind );

/* COO matrix-vector multiplication */
lapack_int LAPACKE_dcoomv( char trans, lapack_int m, lapack_int n, double alpha,
                           lapack_int nnz, const double* values,
                           const lapack_int* row_ind, const lapack_int* col_ind,
                           const double* x, lapack_int incx, double beta,
                           double* y, lapack_int incy );

/* Free COO matrix memory */
lapack_int LAPACKE_dcoofree( double* values, lapack_int* row_ind, lapack_int* col_ind );

/* COO matrix validation */
lapack_int LAPACKE_dcoocheck( lapack_int nrows, lapack_int ncols, lapack_int nnz,
                              const lapack_int* row_ind, const lapack_int* col_ind );

/* COO to dense conversion */
lapack_int LAPACKE_dcoo2den( int matrix_layout, lapack_int m, lapack_int n,
                             lapack_int nnz, const double* values,
                             const lapack_int* row_ind, const lapack_int* col_ind,
                             double* a, lapack_int lda );

/* Dense to COO conversion */
lapack_int LAPACKE_dden2coo( int matrix_layout, lapack_int m, lapack_int n,
                             const double* a, lapack_int lda, double tol,
                             lapack_int* nnz, double** values,
                             lapack_int** row_ind, lapack_int** col_ind );

/* COO matrix transpose */
lapack_int LAPACKE_dcootrans( lapack_int m, lapack_int n, lapack_int nnz,
                              const double* values, const lapack_int* row_ind,
                              const lapack_int* col_ind, double** trans_values,
                              lapack_int** trans_row_ind, lapack_int** trans_col_ind );

/* COO to CSC conversion */
lapack_int LAPACKE_dcoo2csc( lapack_int m, lapack_int n, lapack_int nnz,
                             const double* coo_values, const lapack_int* coo_row_ind,
                             const lapack_int* coo_col_ind, double** csc_values,
                             lapack_int** csc_row_ind, lapack_int** csc_col_ptr );

/* CSR (Compressed Sparse Row) format functions */

/* Initialize CSR matrix */
lapack_int LAPACKE_dcsrinit( lapack_int nrows, lapack_int ncols, lapack_int nnz,
                             const lapack_int* row_ptr, const lapack_int* col_ind,
                             const double* values, double** csr_values,
                             lapack_int** csr_col_ind, lapack_int** csr_row_ptr );

/* CSR matrix-vector multiplication */  
lapack_int LAPACKE_dcsrmv( char trans, lapack_int m, lapack_int n, double alpha,
                           const lapack_int* row_ptr, const lapack_int* col_ind,
                           const double* values, const double* x, lapack_int incx,
                           double beta, double* y, lapack_int incy );

/* Free CSR matrix memory */
lapack_int LAPACKE_dcsrfree( double* values, lapack_int* col_ind, lapack_int* row_ptr );

/* CSR to COO conversion */
lapack_int LAPACKE_dcsr2coo( lapack_int m, lapack_int n, const lapack_int* row_ptr,
                             const lapack_int* col_ind, const double* csr_values,
                             double** coo_values, lapack_int** coo_row_ind,
                             lapack_int** coo_col_ind );

/* CSR to CSC conversion */
lapack_int LAPACKE_dcsrcsc( lapack_int m, lapack_int n, const lapack_int* row_ptr,
                            const lapack_int* col_ind, const double* csr_values,
                            double** csc_values, lapack_int** csc_row_ind,
                            lapack_int** csc_col_ptr );

/* Dense to CSR conversion */
lapack_int LAPACKE_dgecsr( int matrix_layout, lapack_int m, lapack_int n,
                           const double* a, lapack_int lda, double tol,
                           lapack_int* nnz, double** values, lapack_int** col_ind,
                           lapack_int** row_ptr );

/* CSC (Compressed Sparse Column) format functions */

/* Initialize CSC matrix */
lapack_int LAPACKE_dcscinit( lapack_int nrows, lapack_int ncols, lapack_int nnz,
                             const lapack_int* col_ptr, const lapack_int* row_ind,
                             const double* values, double** csc_values,
                             lapack_int** csc_row_ind, lapack_int** csc_col_ptr );

/* CSC matrix-vector multiplication */
lapack_int LAPACKE_dcscmv( char trans, lapack_int m, lapack_int n, double alpha,
                           const lapack_int* col_ptr, const lapack_int* row_ind,
                           const double* values, const double* x, lapack_int incx,
                           double beta, double* y, lapack_int incy );

/* Free CSC matrix memory */
lapack_int LAPACKE_dcscfree( double* values, lapack_int* row_ind, lapack_int* col_ptr );

/* CSC to COO conversion */
lapack_int LAPACKE_dcsc2coo( lapack_int m, lapack_int n, const lapack_int* col_ptr,
                             const lapack_int* row_ind, const double* csc_values,
                             double** coo_values, lapack_int** coo_row_ind,
                             lapack_int** coo_col_ind );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _LAPACKE_SPARSE_H_ */