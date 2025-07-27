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
* Contents: Native high-level C interface to LAPACK function dcoo2den
* Author: Intel Corporation
*****************************************************************************/

#include "lapacke_utils.h"
#include "lapacke_sparse.h"

lapack_int API_SUFFIX(LAPACKE_dcoo2den)( int matrix_layout, lapack_int m, lapack_int n,
                             lapack_int nnz, const double* values,
                             const lapack_int* row_ind, const lapack_int* col_ind,
                             double* a, lapack_int lda )
{
    lapack_int info = 0;
    lapack_int i, j, k;
    
    /* Check input parameters */
    if( matrix_layout != LAPACK_COL_MAJOR && matrix_layout != LAPACK_ROW_MAJOR ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoo2den", -1 );
        return -1;
    }
    if( m < 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoo2den", -2 );
        return -2;
    }
    if( n < 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoo2den", -3 );
        return -3;
    }
    if( nnz < 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoo2den", -4 );
        return -4;
    }
    if( lda < ((matrix_layout == LAPACK_COL_MAJOR) ? MAX(1,m) : MAX(1,n)) ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoo2den", -8 );
        return -8;
    }
    
    /* Quick return if possible */
    if( m == 0 || n == 0 ) {
        return 0;
    }
    
    /* Initialize dense matrix to zero */
    if( matrix_layout == LAPACK_COL_MAJOR ) {
        for( j = 0; j < n; j++ ) {
            for( i = 0; i < m; i++ ) {
                a[i + j * lda] = 0.0;
            }
        }
        
        /* Fill in non-zero values */
        for( k = 0; k < nnz; k++ ) {
            i = row_ind[k] - 1; /* Convert to 0-based */
            j = col_ind[k] - 1; /* Convert to 0-based */
            
            /* Validate indices */
            if( i < 0 || i >= m || j < 0 || j >= n ) {
                return LAPACKE_SPARSE_ERR_INDEX;
            }
            
            a[i + j * lda] = values[k];
        }
    } else { /* LAPACK_ROW_MAJOR */
        for( i = 0; i < m; i++ ) {
            for( j = 0; j < n; j++ ) {
                a[i * lda + j] = 0.0;
            }
        }
        
        /* Fill in non-zero values */
        for( k = 0; k < nnz; k++ ) {
            i = row_ind[k] - 1; /* Convert to 0-based */
            j = col_ind[k] - 1; /* Convert to 0-based */
            
            /* Validate indices */
            if( i < 0 || i >= m || j < 0 || j >= n ) {
                return LAPACKE_SPARSE_ERR_INDEX;
            }
            
            a[i * lda + j] = values[k];
        }
    }
    
    return info;
}