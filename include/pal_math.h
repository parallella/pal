#pragma once


#include "pal_base.h"  //need pal types

/*Standard math constants*/
#if !(defined __USE_BSD || __USE_XOPEN || __USE_GNU)
#define M_E 2.71828182845904523540        /* e */
#define M_LOG2E 1.44269504088896340740    /* log 2e */
#define M_LOG10E 0.43429448190325182765   /* log 10e */
#define M_LN2 0.69314718055994530942      /* log e2 */
#define M_LN10 2.30258509299404568402     /* log e10 */
#define M_PI 3.14159265358979323846       /* pi */
#define M_PI_2 1.57079632679489661923     /* pi/2 */
#define M_PI_4 0.78539816339744830962     /* pi/4 */
#define M_1_PI 0.31830988618379067154     /* 1/pi */
#define M_2_PI 0.63661977236758134308     /* 2/pi */
#define M_2_SQRTPI 1.12837916709551257390 /* 2/sqrt(pi) */
#define M_SQRT2 1.41421356237309504880    /* sqrt(2) */
#define M_SQRT1_2 0.70710678118654752440  /* 1/sqrt(2) */
#define HUGE ((float)3.40282347e+38))     /*maximum value of float*/           
#define MAXFLOAT ((float)3.40282347e+38)) /*maximum value of float*/       
#define MINFLOAT ((float)1.175494351e-38))/*minimum value of float*/

#endif

/*ADDED MATH CONSTANTS*/
#define M_TC 0.63212055882855767840 /* 1 - 1/e */
#define M_PI2 6.283185              /* pi*2 */
#define M_GOLDEN 1.618034           /* golden ratio */
#define M_SQRT3 1.732051            /* sqrt(3) */

/*ADDED RECIPROCAL CONSTANTS (AVOID DIVISION AT ALL COST)*/
/*IDEALLY THIS WOULD BE IN THE COMPILER? A BETTER WAY?*/
#define M_DIV3 0.3333333333333333333  /* 1/3 */
#define M_DIV4 0.25                   /* 1/4 */
#define M_DIV5 0.2                    /* 1/5 */
#define M_DIV6 0.1666666666666666666  /* 1/6 */
#define M_DIV7 0.142857143            /* 1/7 */
#define M_DIV8 0.125                  /* 1/8 */
#define M_DIV9 0.1111111111111111111  /* 1/9 */
#define M_DIV10 0.1                   /* 1/10 */
#define M_DIV11 0.090909091           /* 1/11 */
#define M_DIV12 0.0833333333333333333 /* 1/12 */
#define M_DIV13 0.076923077           /* 1/13 */
#define M_DIV14 0.071428571           /* 1/14 */
#define M_DIV15 0.0666666666666666666 /* 1/15 */
#define M_DIV16 0.0625                /* 1/16 */

/*ADDED PHYSICAL CONSTANTS (ADD YOUR FAVORITES:-)*/
#define PH_C 299792458                  /*speed of light (m/s)*/
#define PH_M0 1.2566370614359172950     /*mag permeability (mH/m)*/
#define PH_H ((float)6.62606957e-34)    /*planck constant (J/Hz)*/
#define PH_HBAR ((float)1.05457172e-34) /*diract constant* (J.s/rad)*/
#define PH_K ((float)1.3806488e-23)     /*boltzmann constant (J/K)*/
#define PH_ME ((float)9.10938291e-31)   /*mass of electron (kg)*/
#define PH_MP ((float)1.672614e-27)     /*mass of proton (kg)*/
#define PH_MN ((float)1.674920e-27)     /*mass of neutron (kg)*/
#define PH_EC ((float)1.6021917e-19)    /*charge of electron (C)*/
#define PH_F ((float)9.648670e4)        /*faraday constant (C/mol)*/
#define PH_G ((float)6.6732e-11)        /*gravitational constant (N*m^2/kg^2)*/
#define PH_AVO ((float)6.022169e23)     /*avogadro constant*/

/*computing macros?
 *
 *abs,floor,ceil,round,sign,zsign,sqr,min,max,swap,clamp
 *
 */

/*
 ****************************************************************
 * Basic Element Wise Vector Math Functions
 *
 * a,b : input vector pointer
 * c   : result vector pointer
 * n   : length of input/output vector
 * p    : number of processors in team (task parallelism)
 * team : team to work with
 *
 ****************************************************************
 */

/*integer to float conversion*/
void p_itof(int *a, float *c, int n, int p, p_team_t team);

/*float to integer conversion*/
void p_ftoi(float *a, int *c, int n, int p, p_team_t team);

/*absolute value c = abs ( a ) */
void p_abs_f32(float *a, float *c, int n, int p, p_team_t team);

/*arc cosine: c = acos ( a ) */
void p_acos_f32(float *a, float *c, int n, int p, p_team_t team);

/*arc hyperbolic cosine, c = acosh ( a ) */
void p_acosh_f32(float *a, float *c, int n, int p, p_team_t team);

/*arc sine: c = asin ( a ) */
void p_asin_f32(float *a, float *c, int n, int p, p_team_t team);

/*arc hyperbolic sine: c = asinh ( a ) */
void p_asinh_f32(float *a, float *c, int n, int p, p_team_t team);

/*arc tanget: c = atan ( a ) */
void p_atan_f32(float *a, float *c, int n, int p, p_team_t team);

/*arc tangent of b/a: c = atan2 ( a , b) */
void p_atan2_f32(float *a, float *b, float *c, int n, int p, p_team_t team);

/*arc hyperbolic tanget: c = atanh ( a ) */
void p_atanh_f32(float *a, float *c, int n, int p, p_team_t team);

/*cubic root of a:  c = cbrt ( a) */
void p_cbrt_f32(float *a, float *c, int n, int p, p_team_t team);

/*cosine: c = cos ( a ) */
void p_cos_f32(float *a, float *c, int n, int p, p_team_t team);

/*hyperpolic cosine:  c = cosh ( a ) */
void p_cosh_f32(float *a, float *c, int n, int p, p_team_t team);

/*division: c =  a ./ b */
void p_div_f32(float *a, float *b, float *c, int n, int p, p_team_t team);

/*exponential: c = exp ( a ) */
void p_exp_f32(float *a, float *c, int n, int p, p_team_t team);

/*inverse: c = 1 / ( a ) */
void p_inv_f32(float *a, float *c, int n, int p, p_team_t team);

/*inverse cube root: c = 1 / cbrt ( a ) */
void p_invcbrt_f32(float *a, float *c, int n, int p, p_team_t team);

/*inverse square root c = 1 / sqrt ( a ) */
void p_invsqrt_f32(float *a, float *c, int n, int p, p_team_t team);

/*natural logarithm: c = ln ( a ) */
void p_ln_f32(float *a, float *c, int n, int p, p_team_t team);

/*denary logarithm: c = log10 ( a ) */
void p_log10_f32(float *a, float *c, int n, int p, p_team_t team);

/*element raised to a power: c = pow ( a , b ) */
void p_pow_f32(float *a, float *b, float *c, int n, int p, p_team_t team);

/*sine: c = sin ( a ) */
void p_sin_f32(float *a, float *c, int n, int p, p_team_t team);

/*computes sin and cos of a: c = sin ( a ),  z = cos ( a ) */
void p_sincos_f32(float *a, float *c, float *z, int n, int p, p_team_t team);

/*hyperbolic Sine: c = sinh ( a ) */
void p_sinh_f32(float *a, float *c, int n, int p, p_team_t team);

/*square root c = sqrt ( a ) */
void p_sqrt_f32(float *a, float *c, int n, int p, p_team_t team);

/*tangent: c = tan ( a ) */
void p_tan_f32(float *a, float *c, int n, int p, p_team_t team);

/*hyperbolic tangent, c = tanh ( a ) */
void p_tanh_f32(float *a, float *c, int n, int p, p_team_t team);

/*dot product: c =  sum ( a[n-1:0] * b[n-1:0] ) */
void p_dot_f32(float *a, float *b, float *c, int n, int p, p_team_t team);

/*absolute difference: c = | a[n-1:0] - b[n-1:0] | */
void p_absdiff_f32(float *a, float *b, float *c, int n, int p, p_team_t team);

/*add vectors: c =  a[n-1:0] + b[n-1:0]  */
void p_add_f32(float *a, float *b, float *c, int n, int p, p_team_t team);

/*subtract vectors: c =  a[n-1:0] - b[n-1:0]  */
void p_sub_f32(float *a, float *b, float *c, int n, int p, p_team_t team);

/*multiply vectors: c =  a[n-1:0] - b[n-1:0]  */
void p_mul_f32(float *a, float *b, float *c, int n, int p, p_team_t team);

/* Element wise multiply accumulate: c += a * b' */
void p_mac_f32(float *a, float *b, float *c, int n, int p, p_team_t team);

/*
 ****************************************************************
 * Vector Reduction Operations
 *
 * a, b : input vector pointer
 * c    : scalar result pointer
 * n    : length of input vector (data parallelism)
 * p    : number of processors in team (task parallelism)
 * team : team to work with
 *
 ****************************************************************
 */

/*sum: c = sum ( a[n-1:0] ) */
void p_sum_f32(float *a, float *c, int n, int p, p_team_t team);

/*sum of squares: c =  sum( a[n-1:0]^2 ) */
void p_sumsq_f32(float *a, float *c, int n, int p, p_team_t team);

/*mean: c = sum ( a[n-1:0] ) / n  */
void p_mean_f32(float *a, float *c, int n, int p, p_team_t team);

/*middle value: c = median ( a[n-1:0] ) */
void p_median_f32(float *a, float *c, int n, int p, p_team_t team);

/*most common number: c = mode ( a[n-1:0] ) */
void p_mode_f32(float *a, float *c, int n, int p, p_team_t team);

/*find max value and its index from input vector */
void p_max_f32(float *a, float *c, int *index, int n, int p, p_team_t team);

/*find min value and its index from input vector */
void p_min_f32(float *a, float *c, int *index, int n, int p, p_team_t team);

/*
 ****************************************************************
 * Miscellaneous Operations
 *
 ****************************************************************
 */

/*sort an array*/
void p_sort_f32(float *a, float *c, int n, int p, p_team_t team);
void p_sort_u32(int *a, int *c, int n, int p, p_team_t team);

/* seed pseudo-random number generator */
void p_srand(unsigned int seed);

/* generate random number */
int p_rand(void);

/*population count*/
void p_popcount_u32(uint32_t *a, uint32_t *c, int n, int p, p_team_t team);
void p_popcount_u64(uint64_t *a, uint64_t *c, int n, int p, p_team_t team);
