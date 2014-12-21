/*
 *Floating point conversion functions
 */

/*integer to float conversion on a vector*/
void p_itof(int n, int* a, float* y);

/*float to integer conversion*/
void p_ftoi(int n, float* a, int* y );



/*
 * Trigonometric functions (element wise)
 */

/*inverse cosine*/
void p_acos_32f(int n, float* a, float* y );

/*inverse hyperbolic cosine*/
void p_acosh_32f(int n, float* a, float* y );

/*inverse sine*/
void p_asin_32f(int n, float* a, float* y );

/*inverse tanget*/
void p_atan_32f(int n, float* a, float* y );

/*four quadrant inverse tangent*/
void p_atan2_32f(int n, float* a, float* b, float* y );

/*cube root*/
void p_cbrt_32f(int n, float* a, float* y );

/*cosine*/
void p_cos_32f(int n, float* a, float* y );

/*hyperpolic cosine*/
void p_cosh_32f(int n, float* a, float* y );

/*exponential*/
void p_exp_32f(int n, float* a, float* y );

/*inverse cube root*/
void p_icbrt_32f(int n, float* a, float* y );

/*natural log*/
void p_ln_32f(int n, float* a, float* y );

/*denary logarithm*/
void p_log10_32f(int n, float* a, float* y );

/*element raised to a specific power*/
void p_pow_32f(int n, float* a, float* b, float* y );

/*sine*/
void p_sin_32f(int n, float* a, float* y );

/*sine & cosine*/
void p_sincos_32f(int n, float* a, float* y, float* z );

/*hyperbolic Sine*/
void p_sinh_32f(int n, float* a, float* y );

/*tangent*/
void p_tan_32f(int n, float* a, float* y );

/*hyperbolic Tangent*/
void p_tanh_32f(int n, float* a, float* y );

/*
 * Reduction functions
 */

/*sum*/
void p_sum_32f(int n, float* a, float* y  );

/*avarage of vector*/
void p_ave_32f(int n, float* a, float* y );

/*mean of vector*/
void p_mean_32f(int n, float* a, float* y );

/*find max value in a vector and index*/
void p_maxval_32f(int n, float* a, int* index, float* y );

/*find min value in a vector and index*/
void p_minval_32f(int n, float* a, int* index, float* y );

/*sum of product on vector*/
void p_sop_32f(int n, const float* a, const float* b, const float* c);

/*
 * Matrix math
 */

/*multiply two matrices*/
void p_matmul_32f(int m, int n, int k, const float* a, const float* b, const float* c);

/*add two matrices*/
void p_matadd_32f(int m, int n, const float* a, const float* b, const float* c);

/*subtract two matrices*/
void p_matsub_32f(int m, int n, const float* a, const float* b, const float* c);

/*transpose a matrix*/
void p_mattran_32f(int m, int k, const float* a, const float* c);

/*element wise division*/
void p_matdiv_32f(int m, int n, float* a, float* b, float* y );

/*element wise square root*/
void p_matsqrt_32f(int m, int n, float* a, float* y );

/*element wise inverse square root*/
void p_matisqrt_32f(int m, int n, int k, float* a, float* y );

/*invert a matrix, gauss jordan, not singular, square, no error checking*/
void p_matinv_32f(int m, const float* a, const float* y);
