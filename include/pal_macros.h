/*Macros are part of the API...*/

/*Include:
 *
 *types, min,max
 *the regular, abs
 *definitions of numbers (PI, etc)
 *tricks that can be done in preprocessor, like ilog2
 *..what else
 *
 */

/*MATH.H CONSTANTS*/
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
#define P_C             299792458               /*speed of light (m/s)*/
#define P_M0            1.2566370614359172950   /*mag permeability (mH/m)*/
#define P_H             ((float)6.62606957e-34) /*planck constant (J/Hz)*/
#define P_HBAR          ((float)1.05457172e-34) /*diract constant* (J.s/rad)*/
#define P_K             ((float)1.3806488e-23)  /*boltzmann constant (J/K)*/
#define P_ME            ((float)9.10938291e-31) /*mass of electron (kg)*/

/*computing macros?
 *
 *abs,floor,ceil,round,sign,zsign,sqr,min,max,swap,clamp
 *
 *
 *
 */
