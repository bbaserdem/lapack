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
* Contents: Native high-level C interface to LAPACK function dcooinit
* Author: Intel Corporation
*****************************************************************************/

#include "lapacke_utils.h"
#include "lapacke_sparse.h"

lapack_int API_SUFFIX(LAPACKE_dcooinit)( lapack_int nrows, lapack_int ncols, lapack_int nnz,
                             const lapack_int* row_ind, const lapack_int* col_ind,
                             const double* values, double** coo_values, 
                             lapack_int** coo_row_ind, lapack_int** coo_col_ind )
{
    lapack_int info = 0;
    
    /* Check input parameters */
    if( nrows < 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcooinit", -1 );
        return -1;
    }
    if( ncols < 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcooinit", -2 );
        return -2;
    }
    if( nnz < 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcooinit", -3 );
        return -3;
    }
    
    /* Quick return if possible */
    if( nnz == 0 ) {
        *coo_values = NULL;
        *coo_row_ind = NULL;
        *coo_col_ind = NULL;
        return 0;
    }
    
    /* Allocate memory for COO arrays */
    *coo_values = (double*)LAPACKE_malloc( sizeof(double) * nnz );
    *coo_row_ind = (lapack_int*)LAPACKE_malloc( sizeof(lapack_int) * nnz );
    *coo_col_ind = (lapack_int*)LAPACKE_malloc( sizeof(lapack_int) * nnz );
    
    if( !*coo_values || !*coo_row_ind || !*coo_col_ind ) {
        if( *coo_values ) LAPACKE_free( *coo_values );
        if( *coo_row_ind ) LAPACKE_free( *coo_row_ind );
        if( *coo_col_ind ) LAPACKE_free( *coo_col_ind );
        *coo_values = NULL;
        *coo_row_ind = NULL;
        *coo_col_ind = NULL;
        return LAPACKE_SPARSE_ERR_ALLOC;
    }
    
    /* Copy and validate data */
    for( lapack_int i = 0; i < nnz; i++ ) {
        /* Validate indices (1-based) */
        if( row_ind[i] < 1 || row_ind[i] > nrows ) {
            LAPACKE_free( *coo_values );
            LAPACKE_free( *coo_row_ind );
            LAPACKE_free( *coo_col_ind );
            *coo_values = NULL;
            *coo_row_ind = NULL;
            *coo_col_ind = NULL;
            return LAPACKE_SPARSE_ERR_INDEX;
        }
        if( col_ind[i] < 1 || col_ind[i] > ncols ) {
            LAPACKE_free( *coo_values );
            LAPACKE_free( *coo_row_ind );
            LAPACKE_free( *coo_col_ind );
            *coo_values = NULL;
            *coo_row_ind = NULL;
            *coo_col_ind = NULL;
            return LAPACKE_SPARSE_ERR_INDEX;
        }
        
        /* Copy data */
        (*coo_values)[i] = values[i];
        (*coo_row_ind)[i] = row_ind[i];
        (*coo_col_ind)[i] = col_ind[i];
    }
    
    return info;
}