#include <pal.h>

/**
 * Computes the raw full-length linear cross correlation of vectors x and y and
 * stores the result in vector r. Normalization is done outside this function.
 *
 * NOTE: This implementation requires nx >= ny -- need to be checked by the caller !!!
 *
 * @param x   Pointer an input vector of floats of 'nx' elements
 *
 * @param y     Pointer an input vector of floats of 'ny' elements
 *
 * @param r     Pointer an output vector of floats of 'ny+ny-1' elements
 *
 * @param nx    Number of elements in vector 'x'
 *
 * @param ny    Number of elements in vector 'y'
 *
 * @return      None
 *
 */

void p_xcorr_f32(const float *x, const float *y, float *r, int nx, int ny)
{
  const float* data1_p;
  const float* data2_p;
  const float* start_data1_p;
  const float* start_data2_p;
  int i1, i2, n1, n2, i;
  const float* vec1_save_p;
  const float* vec2_save_p;
  float tmp;
  // Default assumption ny <= nx
  n1 = nx;
  n2 = ny;
  start_data1_p = x;
  start_data2_p = y;

  /*  This part can be uncommented if a more general function is needed, see NOTE above
  float* r_orig_p;
  r_orig_p = r;
  // Check if we shall swith pointers
  if ( nx < ny ) {
    n1 = ny;
    n2 = nx;
    start_data1_p = y;
    start_data2_p = x;
  }   
  */

  // Divide in three fases
  // First calculate last part of vector 2 first part of vector 1
  vec2_save_p = &start_data2_p[n2-1];
  for ( i1 = 0; i1 < n2-1; i1++ ) {
    data1_p = start_data1_p;
    data2_p = vec2_save_p--;
    *r = (*data1_p++) * (*data2_p++);
    for ( i2 = 1; i2 < i1+1; i2++ ) {
      *r += (*data1_p++) * (*data2_p++);
    }
    r++;
  }
  // Now the middle part, with full correlation betweeen vector 1 and 2
  vec1_save_p = start_data1_p;
  for ( i1 = 0; i1 < n1-n2+1; i1++ ) {
    data1_p = vec1_save_p++;
    data2_p = start_data2_p;
    *r = (*data1_p++) * (*data2_p++);
    for ( i2 = 1; i2 < n2; i2++ ) {
      *r += (*data1_p++) * (*data2_p++);
    }
    r++;
  }

  // And finaly the last part of vector 1 with the first part of vector 2
  vec1_save_p = &start_data1_p[n1-n2+1];
  for ( i1 = 0; i1 < n2 - 1; i1++ ) {
    data1_p = vec1_save_p++;
    data2_p = start_data2_p;
    *r = (*data1_p++) * (*data2_p++);
    for ( i2 = 1; i2 < n2-i1-1; i2++ ) {
      *r += (*data1_p++) * (*data2_p++);
    }
    r++;
  }

  /*  This part can be uncommented if a more general function is needed, see NOTE above  
  // Swap data in output vector if size of X is less then Y
  if ( nx < ny ) {
    for ( i = 0; i < (n1+n2-1)/2; i++ ) {
      tmp = r_orig_p[i];
      r_orig_p[i] = r_orig_p[n1+n2-2-i];
      r_orig_p[n1+n2-2-i] = tmp;
    }
  }
  */
}
