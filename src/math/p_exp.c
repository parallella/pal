#include <pal.h>

/**
 *
 * Calculate exponent (e^a), where e is the base of the natural logarithm
 * (2.71828.)
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

/* float version of e_exp.c
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com. */

#include <math.h>

#define GET_FLOAT_WORD(i,d)  conv.value = (d); (i) = conv.word;
#define SET_FLOAT_WORD(d,i)  conv.word = (i); (d) = conv.value;

static const float huge = 1.0e+30;

static const float
one	= 1.0,
halF[2]	= {0.5,-0.5,},
twom100 = 7.8886090522e-31,      /* 2**-100=0x0d800000 */
o_threshold=  8.8721679688e+01,  /* 0x42b17180 */
u_threshold= -1.0397208405e+02,  /* 0xc2cff1b5 */
ln2HI[2]   ={ 6.9313812256e-01,		/* 0x3f317180 */
	     -6.9313812256e-01,},	/* 0xbf317180 */
ln2LO[2]   ={ 9.0580006145e-06,  	/* 0x3717f7d1 */
	     -9.0580006145e-06,},	/* 0xb717f7d1 */
invln2 =  1.4426950216e+00, 		/* 0x3fb8aa3b */
P1   =  1.6666667163e-01, /* 0x3e2aaaab */
P2   = -2.7777778450e-03, /* 0xbb360b61 */
P3   =  6.6137559770e-05, /* 0x388ab355 */
P4   = -1.6533901999e-06, /* 0xb5ddea0e */
P5   =  4.1381369442e-08; /* 0x3331bb4c */

void p_exp_f32(const float *a, float *c, int n, int p, p_team_t team)
{
    int i;
    for (i = 0; i < n; i++) {
        float x = a[i];
        union {float value; uint32_t word;} conv;
        float y,hi,lo,t;
        int32_t k,xsb;
        u_int32_t hx;

        hi = lo = 0;
        k = 0;
        GET_FLOAT_WORD(hx,x);
        xsb = (hx>>31)&1;		/* sign bit of x */
        hx &= 0x7fffffff;		/* high word of |x| */

        /* filter out non-finite argument */
        if(hx >= 0x42b17218) {			/* if |x|>=88.721... */
            if(hx>0x7f800000) {
                c[i] = x+x;	 		    /* NaN */ 
                continue;
            }
            if(hx==0x7f800000) {
                c[i] = (xsb==0)? x:0.0;		/* exp(+-inf)={inf,0} */
                continue;
            }            
            if(x > o_threshold) {
                c[i] = huge*huge;               /* overflow */
                continue;
            }            
            if(x < u_threshold) {
                c[i] = twom100*twom100;         /* underflow */
                continue;
            }            
        }

        /* argument reduction */
        if(hx > 0x3eb17218) {		/* if  |x| > 0.5 ln2 */
            if(hx < 0x3F851592) {	/* and |x| < 1.5 ln2 */
                hi = x-ln2HI[xsb]; lo=ln2LO[xsb]; k = 1-xsb-xsb;
            } else {
                k  = invln2*x+halF[xsb];
                t  = k;
                hi = x - t*ln2HI[0];	/* t*ln2HI is exact here */
                lo = t*ln2LO[0];
            }
            x  = hi - lo;
        }
        else if(hx < 0x31800000)  {	/* when |x|<2**-28 */
            if(huge+x>one) { 
                c[i] = one+x;       /* trigger inexact */
                continue;
            }
        }
        else k = 0;

        /* x is now in primary range */
        t  = x*x;
        float d = x - t*(P1+t*(P2+t*(P3+t*(P4+t*P5))));


        /* Inverting d-2, code taken from p_inv.c */
        float z = d-2.0f;
        conv.value = z;
        conv.word = 0x7EEEEBB3 - conv.word;
        conv.value *= (2 - conv.value * z);
        conv.value *= (2 - conv.value * z);
        conv.value *= (2 - conv.value * z);
        z = conv.value;

        if(k==0) {
            c[i] = one-(x*d*z-x);
            continue;
        }
        else
            y = one-((lo+(x*d*z))-hi);
        if(k >= -125) {
            u_int32_t hy;
            GET_FLOAT_WORD(hy,y);
            SET_FLOAT_WORD(y,hy+(k<<23));	/* add k to y's exponent */
            c[i] = y;
        } else {
            u_int32_t hy;
            GET_FLOAT_WORD(hy,y);
            SET_FLOAT_WORD(y,hy+((k+100)<<23));	/* add k to y's exponent */
            c[i] = y*twom100;
        }
    }
}
