#include <pal.h>

/**
 * Computes a FIR filter (direct-form) on input data in vector 'x' using the
 * coefficients stored in vector 'h'.
 *
 * @param x     Pointer to input vector of 'n' elements
 *
 * @param h     Pointer to filter coefficients.
 *
 * @param r     Pointer to result vector
 *
 * @param nx    The number of input samples
 *
 * @param nh    The number of coefficients of the filter.
 *
 * @return      None
 *
 */

void p_fir_f32(const float *x, const float *h, float *r, int nx, int nh)
{
    int wrp;    /* Delay line block pointer */
    int dlp;    /* Delay line individual tap position */
    int rdp;    /* I/O data pointer */
    int cp;     /* FIR coefficient index */
    int ndl;    /* Length of delay line */

    float *dl;
    float fir[8];

    /* The length of the delay line is (next multiple of 8 of nh) + 8. */
    if (nh & 7) {
        ndl = nh & ~0x7;
        ndl += 16;
    } else {
        ndl = nh + 8;
    }

    /* Delay line is twice the necessary size to avoid moving data around. */
    /* [TODO] Use p_malloc (not implemented yet) */
    dl = malloc(2*ndl*sizeof(float));

    /* Reset delay line */
    for (wrp = 0; wrp < 2*ndl; wrp++) {
        dl[wrp] = 0.0;
    }

    /* Start convolution */
    wrp = ndl;
    for (rdp = 0; rdp < nx-7; rdp += 8) {
        /* Update delay line */
        dl[wrp+0] = dl[wrp-ndl+0] = x[rdp+0];
        dl[wrp+1] = dl[wrp-ndl+1] = x[rdp+1];
        dl[wrp+2] = dl[wrp-ndl+2] = x[rdp+2];
        dl[wrp+3] = dl[wrp-ndl+3] = x[rdp+3];
        dl[wrp+4] = dl[wrp-ndl+4] = x[rdp+4];
        dl[wrp+5] = dl[wrp-ndl+5] = x[rdp+5];
        dl[wrp+6] = dl[wrp-ndl+6] = x[rdp+6];
        dl[wrp+7] = dl[wrp-ndl+7] = x[rdp+7];

        fir[0] = fir[1] = fir[2] = fir[3] = 0.0;
        fir[4] = fir[5] = fir[6] = fir[7] = 0.0;

        /* Filter samples using temporary local array */
        for (cp = nh-1, dlp = wrp-nh+1; cp >= 0; cp--, dlp++) {
            fir[0] += h[cp] * dl[dlp+0];
            fir[1] += h[cp] * dl[dlp+1];
            fir[2] += h[cp] * dl[dlp+2];
            fir[3] += h[cp] * dl[dlp+3];
            fir[4] += h[cp] * dl[dlp+4];
            fir[5] += h[cp] * dl[dlp+5];
            fir[6] += h[cp] * dl[dlp+6];
            fir[7] += h[cp] * dl[dlp+7];
        }

        /* Update output array */
        r[rdp+0] = fir[0];
        r[rdp+1] = fir[1];
        r[rdp+2] = fir[2];
        r[rdp+3] = fir[3];
        r[rdp+4] = fir[4];
        r[rdp+5] = fir[5];
        r[rdp+6] = fir[6];
        r[rdp+7] = fir[7];

        /* Increment delay line block and wrap around, if necessary. */
        wrp += 8;
        if (wrp == 2*ndl)
            wrp = ndl;
    }

    /* If the length of x is not a multiple of 8, there are still N < 8 samples
       to process. The following therefore remains within one delay line block
       and no wrapping is possible. */
    for ( ; rdp < nx; rdp++, wrp++) {
        /* Update delay line */
        dl[wrp] = x[rdp];

        fir[0] = 0.0;

        /* Filter samples using temporary local array */
        for (cp = nh-1, dlp = wrp-nh+1; cp >= 0; cp--, dlp++) {
            fir[0] += h[cp] * dl[dlp];
        }

        /* Update output array */
        r[rdp] = fir[0];
    }

    free(dl);
}
