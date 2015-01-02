/*
 ****************************************************************
 * Basic Element Wise Vector Math Functions
 *
 * a,b : input vector pointer
 * c   : result vector pointer
 * n   : length of input/output vector
 *
 ****************************************************************
 */

/*integer to float conversion*/
void p_itof( const int* a, float* c, int n );

/*float to integer conversion*/
void p_ftoi( const float* a, int* c, int n  );

/*absolute value c = abs ( a ) */
void p_abs_32f( const float* a, float* c, int n );

/*arc cosine: c = acos ( a ) */
void p_acos_32f( const float* a, float* c, int n );

/*arc hyperbolic cosine, c = acosh ( a ) */
void p_acosh_32f( const float* a, float* c, int n );

/*arc sine: c = asin ( a ) */
void p_asin_32f( const float* a, float* c, int n );

/*arc hyperbolic sine: c = asinh ( a ) */
void p_asinh_32f( const float* a, float* c, int n );

/*arc tanget: c = atan ( a ) */
void p_atan_32f( const float* a, float* c, int n );

/*arc tangent of b/a: c = atan2 ( a , b) */
void p_atan2_32f( const float* a, const float* b, float* c, int n );

/*arc hyperbolic tanget: c = atanh ( a ) */
void p_atanh_32f( const float* a, float* c, int n );

/*cubic root of a:  c = cbrt ( a) */
void p_cbrt_32f( const float* a, float* c, int n );

/*cosine: c = cos ( a ) */
void p_cos_32f( const float* a, float* c, int n );

/*hyperpolic cosine:  c = cosh ( a ) */
void p_cosh_32f(const float* a, float* c, int n );

/*division: c =  a ./ b */
void p_div_32f( const float* a, const float* b, float* c, int n );

/*exponential: c = exp ( a ) */
void p_exp_32f( const float* a, float* c, int n );

/*inverse: c = 1 / ( a ) */
void p_inv_32f( const float* a, float* c, int n );

/*inverse cube root: c = 1 / cbrt ( a ) */
void p_invcbrt_32f( const float* a, float* c, int n );

/*inverse square root c = 1 / sqrt ( a ) */
void p_invsqrt_32f( const float* a, float* c, int n );

/*natural logarithm: c = ln ( a ) */
void p_ln_32f( const float* a, float* c, int n );

/*denary logarithm: c = log10 ( a ) */
void p_log10_32f( const float* a, float* c, int n );

/*element raised to a power: c = pow ( a , b ) */
void p_pow_32f( const float* a, const float* b, float* c, int n );

/*sine: c = sin ( a ) */
void p_sin_32f( const float* a, float* c, int n );

/*computes sin and cos of a: c = sin ( a ),  z = cos ( a ) */
void p_sincos_32f( const float* a, float* c, float* z );

/*hyperbolic Sine: c = sinh ( a ) */
void p_sinh_32f( const float* a, float* c, int n );

/*square root c = sqrt ( a ) */
void p_sqrt_32f(const float* a, float* c, int n );

/*tangent: c = tan ( a ) */
void p_tan_32f( const float* a, float* c, int n );

/*hyperbolic tangent, c = tanh ( a ) */
void p_tanh_32f( const float* a, float* c, int n );

/*dot product: c =  sum ( a[n-1:0] * b[n-1:0] ) */
void p_dot_32f( const float* a,  const float* b,  float* c, int n );

/*absolute difference: c = | a[n-1:0] - b[n-1:0] | */
void p_absdiff_32f( const float* a,  const float* b,  float* c, int n );

/*add vectors: c =  a[n-1:0] + b[n-1:0]  */
void p_add_32f( const float* a,  const float* b,  float* c, int n );

/*subtract vectors: c =  a[n-1:0] - b[n-1:0]  */
void p_sub_32f( const float* a,  const float* b,  float* c, int n );

/*multiply vectors: c =  a[n-1:0] - b[n-1:0]  */
void p_mul_32f( const float* a,  const float* b,  float* c, int n );

/*
 ****************************************************************
 * Vector Reduction Operations
 *
 * a, b : input vector pointer
 * c    : scalar result pointer
 * n    : length of input vector
 *
 ****************************************************************
 */

/*sum: c = sum ( a[n-1:0] ) */
void p_sum_32f( const float* a, float* c, int n  );

/*sum of squares: c =  sum( a[n-1:0]^2 ) */
void p_sumsq_32f( const float* a, float* c, int n  );

/*mean: c = sum ( a[n-1:0] ) / n  */
void p_mean_32f( const float* a, float* c, int n );

/*middle value: c = median ( a[n-1:0] ) */
void p_median_32f( const float* a, float* c, int n  );

/*most common number: c = mode ( a[n-1:0] ) */
void p_mode_32f( const float* a, float* c, int n  );

/*find max value and its index from input vector */
void p_maxl_32f( const float* a, float* c, int* index, int n );

/*find min value and its index from input vector */
void p_minl_32f( const float* a, float* c, int* index, int n  );


