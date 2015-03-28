#include <pal.h>

/**
 * Computes a cascaded IIR filter of 'nb' biquad sections using 32-bit
 * coefficients and 32-bit delay buffers. Each biquad section is implemented
 * using Direct-form II.
 *
 * @param x     Pointer an input vector with 'nx' elements
 *
 * @param h     Pointer to vector containing '5*nb' filter coefficient
 *
 * @param r     Pointer an output vector of floats of 'nr' elements
 *
 * @param nb    Number of biquads in filter
 *
 * @param nr    Size of input and output vectors
 * 
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      Returns 0 if successful
 *
 */

void p_iir_f32(float *x, float *h, float *r, int nb, int nr,
	       int p, p_team_t team)
{
	int register rdp; // pointer to the I/O data's current position.
	int register cp;  // pointer to the coefficients array.
	unsigned time, time_s, time_e;

	float ca[_Nstage][_Ntaps]; // bwd coefficients
	float cb[_Nstage][_Ntaps]; // fwd coefficients
	float dl[_Nstage][_Ntaps]; // delay line
	float iir[4];              // temp accumulator

	for (cp=0; cp<_Ntaps; cp++){
		dl[0][cp] = 0.0;
		ca[0][cp] = coeffs[0][IIR][cp];
		cb[0][cp] = coeffs[0][FIR][cp];

		dl[1][cp] = 0.0;
		ca[1][cp] = coeffs[1][IIR][cp];
		cb[1][cp] = coeffs[1][FIR][cp];
	}

	for (rdp=0; rdp<_Ndata; rdp+=8){
		iir[0]   = ca[0][0] * (inp_data[rdp+0] + ca[0][1] * dl[0][0] + ca[0][2] * dl[0][1]);
		iir[2]   = cb[0][0] * iir[0]           + cb[0][1] * dl[0][0] + cb[0][2] * dl[0][1];
		dl[0][1] = dl[0][0];
		dl[0][0] = iir[0];

		iir[1]   = ca[1][0] * (inp_data[rdp+0] + ca[1][1] * dl[1][0] + ca[1][2] * dl[1][1]);
		iir[3]   = cb[1][0] * iir[1]           + cb[1][1] * dl[1][0] + cb[1][2] * dl[1][1];
		dl[1][1] = dl[1][0];
		dl[1][0] = iir[1];

		out_data[rdp+0] = iir[2] + iir[3];


		iir[0]   = ca[0][0] * (inp_data[rdp+1] + ca[0][1] * dl[0][0] + ca[0][2] * dl[0][1]);
		iir[2]   = cb[0][0] * iir[0]           + cb[0][1] * dl[0][0] + cb[0][2] * dl[0][1];
		dl[0][1] = dl[0][0];
		dl[0][0] = iir[0];

		iir[1]   = ca[1][0] * (inp_data[rdp+1] + ca[1][1] * dl[1][0] + ca[1][2] * dl[1][1]);
		iir[3]   = cb[1][0] * iir[1]           + cb[1][1] * dl[1][0] + cb[1][2] * dl[1][1];
		dl[1][1] = dl[1][0];
		dl[1][0] = iir[1];

		out_data[rdp+1] = iir[2] + iir[3];


		iir[0]   = ca[0][0] * (inp_data[rdp+2] + ca[0][1] * dl[0][0] + ca[0][2] * dl[0][1]);
		iir[2]   = cb[0][0] * iir[0]           + cb[0][1] * dl[0][0] + cb[0][2] * dl[0][1];
		dl[0][1] = dl[0][0];
		dl[0][0] = iir[0];

		iir[1]   = ca[1][0] * (inp_data[rdp+2] + ca[1][1] * dl[1][0] + ca[1][2] * dl[1][1]);
		iir[3]   = cb[1][0] * iir[1]           + cb[1][1] * dl[1][0] + cb[1][2] * dl[1][1];
		dl[1][1] = dl[1][0];
		dl[1][0] = iir[1];

		out_data[rdp+2] = iir[2] + iir[3];


		iir[0]   = ca[0][0] * (inp_data[rdp+3] + ca[0][1] * dl[0][0] + ca[0][2] * dl[0][1]);
		iir[2]   = cb[0][0] * iir[0]           + cb[0][1] * dl[0][0] + cb[0][2] * dl[0][1];
		dl[0][1] = dl[0][0];
		dl[0][0] = iir[0];

		iir[1]   = ca[1][0] * (inp_data[rdp+3] + ca[1][1] * dl[1][0] + ca[1][2] * dl[1][1]);
		iir[3]   = cb[1][0] * iir[1]           + cb[1][1] * dl[1][0] + cb[1][2] * dl[1][1];
		dl[1][1] = dl[1][0];
		dl[1][0] = iir[1];

		out_data[rdp+3] = iir[2] + iir[3];


		iir[0]   = ca[0][0] * (inp_data[rdp+4] + ca[0][1] * dl[0][0] + ca[0][2] * dl[0][1]);
		iir[2]   = cb[0][0] * iir[0]           + cb[0][1] * dl[0][0] + cb[0][2] * dl[0][1];
		dl[0][1] = dl[0][0];
		dl[0][0] = iir[0];

		iir[1]   = ca[1][0] * (inp_data[rdp+4] + ca[1][1] * dl[1][0] + ca[1][2] * dl[1][1]);
		iir[3]   = cb[1][0] * iir[1]           + cb[1][1] * dl[1][0] + cb[1][2] * dl[1][1];
		dl[1][1] = dl[1][0];
		dl[1][0] = iir[1];

		out_data[rdp+4] = iir[2] + iir[3];


		iir[0]   = ca[0][0] * (inp_data[rdp+5] + ca[0][1] * dl[0][0] + ca[0][2] * dl[0][1]);
		iir[2]   = cb[0][0] * iir[0]           + cb[0][1] * dl[0][0] + cb[0][2] * dl[0][1];
		dl[0][1] = dl[0][0];
		dl[0][0] = iir[0];

		iir[1]   = ca[1][0] * (inp_data[rdp+5] + ca[1][1] * dl[1][0] + ca[1][2] * dl[1][1]);
		iir[3]   = cb[1][0] * iir[1]           + cb[1][1] * dl[1][0] + cb[1][2] * dl[1][1];
		dl[1][1] = dl[1][0];
		dl[1][0] = iir[1];

		out_data[rdp+5] = iir[2] + iir[3];


		iir[0]   = ca[0][0] * (inp_data[rdp+6] + ca[0][1] * dl[0][0] + ca[0][2] * dl[0][1]);
		iir[2]   = cb[0][0] * iir[0]           + cb[0][1] * dl[0][0] + cb[0][2] * dl[0][1];
		dl[0][1] = dl[0][0];
		dl[0][0] = iir[0];

		iir[1]   = ca[1][0] * (inp_data[rdp+6] + ca[1][1] * dl[1][0] + ca[1][2] * dl[1][1]);
		iir[3]   = cb[1][0] * iir[1]           + cb[1][1] * dl[1][0] + cb[1][2] * dl[1][1];
		dl[1][1] = dl[1][0];
		dl[1][0] = iir[1];

		out_data[rdp+6] = iir[2] + iir[3];


		iir[0]   = ca[0][0] * (inp_data[rdp+7] + ca[0][1] * dl[0][0] + ca[0][2] * dl[0][1]);
		iir[2]   = cb[0][0] * iir[0]           + cb[0][1] * dl[0][0] + cb[0][2] * dl[0][1];
		dl[0][1] = dl[0][0];
		dl[0][0] = iir[0];

		iir[1]   = ca[1][0] * (inp_data[rdp+7] + ca[1][1] * dl[1][0] + ca[1][2] * dl[1][1]);
		iir[3]   = cb[1][0] * iir[1]           + cb[1][1] * dl[1][0] + cb[1][2] * dl[1][1];
		dl[1][1] = dl[1][0];
		dl[1][0] = iir[1];

		out_data[rdp+7] = iir[2] + iir[3];
	}

	time_e = e_ctimer_get(E_CTIMER_0);
	e_ctimer_stop(E_CTIMER_0);

	time = time_s - time_e;

	return time;
}
