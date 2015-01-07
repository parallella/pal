/*Standard math constants*/
#define	M_E		2.7182818284590452354	/* e */
#define	M_LOG2E		1.4426950408889634074	/* log 2e */
#define	M_LOG10E	0.43429448190325182765	/* log 10e */
#define	M_LN2		0.69314718055994530942	/* log e2 */
#define	M_LN10		2.30258509299404568402	/* log e10 */
#define	M_PI		3.14159265358979323846	/* pi */
#define	M_PI_2		1.57079632679489661923	/* pi/2 */
#define	M_PI_4		0.78539816339744830962	/* pi/4 */
#define	M_1_PI		0.31830988618379067154	/* 1/pi */
#define	M_2_PI		0.63661977236758134308	/* 2/pi */
#define	M_2_SQRTPI	1.12837916709551257390	/* 2/sqrt(pi) */
#define	M_SQRT2		1.41421356237309504880	/* sqrt(2) */
#define	M_SQRT1_2	0.70710678118654752440	/* 1/sqrt(2) */

/*ADDED MATH CONSTANTS*/
#define M_TC            0.63212055882855767840  /* 1 - 1/e */
#define	M_PI2	        6.283185              	/* pi*2 */
#define M_GOLDEN        1.618034                /* golden ratio */
#define M_SQRT3         1.732051                /* sqrt(3) */

/*ADDED RECIPROCAL CONSTANTS (AVOID DIVISION AT ALL COST)*/
/*IDEALLY THIS WOULD BE IN THE COMPILER? A BETTER WAY?*/
#define M_DIV3           0.3333333333333333333  /* 1/3 */
#define M_DIV4           0.25                   /* 1/4 */
#define M_DIV5           0.2                    /* 1/5 */
#define M_DIV6           0.1666666666666666666  /* 1/6 */
#define M_DIV7           0.142857143            /* 1/7 */
#define M_DIV8           0.125                  /* 1/8 */
#define M_DIV9           0.1111111111111111111  /* 1/9 */
#define M_DIV10          0.1                    /* 1/10 */
#define M_DIV11          0.090909091            /* 1/11 */
#define M_DIV12          0.0833333333333333333  /* 1/12 */
#define M_DIV13          0.076923077            /* 1/13 */
#define M_DIV15          0.0666666666666666666  /* 1/14 */
#define M_DIV15          0.0666666666666666666  /* 1/15 */
#define M_DIV16          0.0625                 /* 1/16 */

/*ADDED PHYSICAL CONSTANTS (ADD YOUR FAVORITE..)*/
#define M_C             299792458               /*speed of light (m/s)*/
#define M_M0            1.2566370614359172950   /*mag permeability (mH/m)*/
#define M_H             ((float)6.62606957e-34) /*planck constant (J/Hz)*/
#define M_HBAR          ((float)1.05457172e-34) /*diract constant* (J.s/rad)*/
#define M_K             ((float)1.3806488e-23)  /*boltzmann constant (J/K)*/
#define M_ME            ((float)9.10938291e-31) /*mass of electron (kg)*/

/*computing macros?
 *
 *abs,floor,ceil,round,sign,zsign,sqr,min,max,swap,clamp
 *
 *
 *
 */

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


