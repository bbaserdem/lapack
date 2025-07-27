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
* Contents: Native high-level C interface to LAPACK function dcoomv
* Author: Intel Corporation
*****************************************************************************/

#include "lapacke_utils.h"
#include "lapacke_sparse.h"

lapack_int API_SUFFIX(LAPACKE_dcoomv)( char trans, lapack_int m, lapack_int n, double alpha,
                           lapack_int nnz, const double* values,
                           const lapack_int* row_ind, const lapack_int* col_ind,
                           const double* x, lapack_int incx, double beta,
                           double* y, lapack_int incy )
{
    lapack_int info = 0;
    lapack_int i, j, k, ix, iy, kx, ky;
    double temp;
    int notr;
    
    /* Check input parameters */
    if( trans != 'N' && trans != 'n' && trans != 'T' && trans != 't' &&
        trans != 'C' && trans != 'c' ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoomv", -1 );
        return -1;
    }
    if( m < 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoomv", -2 );
        return -2;
    }
    if( n < 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoomv", -3 );
        return -3;
    }
    if( nnz < 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoomv", -5 );
        return -5;
    }
    if( incx == 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoomv", -10 );
        return -10;
    }
    if( incy == 0 ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dcoomv", -12 );
        return -12;
    }
    
    /* Quick return if possible */
    if( m == 0 || n == 0 || (alpha == 0.0 && beta == 1.0) || nnz == 0 ) {
        return 0;
    }
    
    notr = (trans == 'N' || trans == 'n');
    
    /* Set up the start points in X and Y */
    if( notr ) {
        /* No transpose: x has length n, y has length m */
        kx = (incx > 0) ? 1 : 1 - (n - 1) * incx;
        ky = (incy > 0) ? 1 : 1 - (m - 1) * incy;
    } else {
        /* Transpose: x has length m, y has length n */
        kx = (incx > 0) ? 1 : 1 - (m - 1) * incx;
        ky = (incy > 0) ? 1 : 1 - (n - 1) * incy;
    }
    
    /* First form y := beta*y */
    if( beta != 1.0 ) {
        if( notr ) {
            /* No transpose: y has length m */
            if( incy == 1 ) {
                if( beta == 0.0 ) {
                    for( i = 0; i < m; i++ ) {
                        y[i] = 0.0;
                    }
                } else {
                    for( i = 0; i < m; i++ ) {
                        y[i] = beta * y[i];
                    }
                }
            } else {
                iy = ky - 1; /* Convert to 0-based */
                if( beta == 0.0 ) {
                    for( i = 0; i < m; i++ ) {
                        y[iy] = 0.0;
                        iy += incy;
                    }
                } else {
                    for( i = 0; i < m; i++ ) {
                        y[iy] = beta * y[iy];
                        iy += incy;
                    }
                }
            }
        } else {
            /* Transpose: y has length n */
            if( incy == 1 ) {
                if( beta == 0.0 ) {
                    for( i = 0; i < n; i++ ) {
                        y[i] = 0.0;
                    }
                } else {
                    for( i = 0; i < n; i++ ) {
                        y[i] = beta * y[i];
                    }
                }
            } else {
                iy = ky - 1; /* Convert to 0-based */
                if( beta == 0.0 ) {
                    for( i = 0; i < n; i++ ) {
                        y[iy] = 0.0;
                        iy += incy;
                    }
                } else {
                    for( i = 0; i < n; i++ ) {
                        y[iy] = beta * y[iy];
                        iy += incy;
                    }
                }
            }
        }
    }
    
    if( alpha == 0.0 ) return 0;
    
    /* Now perform the sparse matrix-vector multiplication */
    if( notr ) {
        /* Form y := alpha*A*x + y */
        if( incx == 1 && incy == 1 ) {
            /* Both increments equal to 1 */
            for( k = 0; k < nnz; k++ ) {
                i = row_ind[k] - 1; /* Convert to 0-based */
                j = col_ind[k] - 1; /* Convert to 0-based */
                y[i] = y[i] + alpha * values[k] * x[j];
            }
        } else {
            /* General increments */
            for( k = 0; k < nnz; k++ ) {
                i = row_ind[k]; /* 1-based */
                j = col_ind[k]; /* 1-based */
                ix = kx + (j - 1) * incx - 1; /* Convert to 0-based */
                iy = ky + (i - 1) * incy - 1; /* Convert to 0-based */
                y[iy] = y[iy] + alpha * values[k] * x[ix];
            }
        }
    } else {
        /* Form y := alpha*A**T*x + y */
        if( incx == 1 && incy == 1 ) {
            /* Both increments equal to 1 */
            for( k = 0; k < nnz; k++ ) {
                i = row_ind[k] - 1; /* Convert to 0-based */
                j = col_ind[k] - 1; /* Convert to 0-based */
                y[j] = y[j] + alpha * values[k] * x[i];
            }
        } else {
            /* General increments */
            for( k = 0; k < nnz; k++ ) {
                i = row_ind[k]; /* 1-based */
                j = col_ind[k]; /* 1-based */
                ix = kx + (i - 1) * incx - 1; /* Convert to 0-based */
                iy = ky + (j - 1) * incy - 1; /* Convert to 0-based */
                y[iy] = y[iy] + alpha * values[k] * x[ix];
            }
        }
    }
    
    return info;
}