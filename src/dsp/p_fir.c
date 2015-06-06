#include <pal.h>

/**
 * Computes a FIR filter (direct-form) on input data in vector 'x' using the
 * coefficients stored in vector 'h'. This function maintains the array
 * 'dbuf' containing the previous delayed input values to allow
 * consecutive processing of input data blocks.
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
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

void p_fir_f32(const float *x, const float *h, float *r, int nx, int nh,
               int p, p_team_t team)
{

    int wrp; // delay line's current position.
    int rdp; // I/O data's current position
    int cp;  // FIR coefficient index
    int dlp; // delay line position per-tap.
    
    float dl[64 * 2];   // TODO: Fix size, make dynamic
    float fir[8];       // temp fir accumulators
    
    for (cp=0; cp < nh; cp++){
	dl[cp] = dl[cp+nh] = 0.0;
    }   
    for (rdp=0; rdp<nx; rdp+=nh){
	for (wrp=0; wrp<nh; wrp+=8){
	    dl[wrp+0] = dl[wrp+nh+0] = x[rdp+wrp+0];
	    dl[wrp+1] = dl[wrp+nh+1] = x[rdp+wrp+1];
	    dl[wrp+2] = dl[wrp+nh+2] = x[rdp+wrp+2];
	    dl[wrp+3] = dl[wrp+nh+3] = x[rdp+wrp+3];
	    dl[wrp+4] = dl[wrp+nh+4] = x[rdp+wrp+4];
	    dl[wrp+5] = dl[wrp+nh+5] = x[rdp+wrp+5];
	    dl[wrp+6] = dl[wrp+nh+6] = x[rdp+wrp+6];
	    dl[wrp+7] = dl[wrp+nh+7] = x[rdp+wrp+7];
	    fir[0] = fir[1] = fir[2] = fir[3] = 0;
	    fir[4] = fir[5] = fir[6] = fir[7] = 0;
	    //Unrolled inner loop
	    for (cp=0, dlp=(wrp+1); cp<nh; cp++, dlp++){
		fir[0] += h[cp] * dl[dlp + 0];
		fir[1] += h[cp] * dl[dlp + 1];
		fir[2] += h[cp] * dl[dlp + 2];
		fir[3] += h[cp] * dl[dlp + 3];
		fir[4] += h[cp] * dl[dlp + 4];
		fir[5] += h[cp] * dl[dlp + 5];
		fir[6] += h[cp] * dl[dlp + 6];
		fir[7] += h[cp] * dl[dlp + 7];
	    }
	    r[rdp+wrp+0] = fir[0];
	    r[rdp+wrp+1] = fir[1];
	    r[rdp+wrp+2] = fir[2];
	    r[rdp+wrp+3] = fir[3];
	    r[rdp+wrp+4] = fir[4];
	    r[rdp+wrp+5] = fir[5];
	    r[rdp+wrp+6] = fir[6];
	    r[rdp+wrp+7] = fir[7];
	}
    }    
}

