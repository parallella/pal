#pragma once

/*
 ****************************************************************
 * 1D: Filters, Convolution, Correlation Functions
 *
 * x,y          : input vector pointers
 * h            : input coefficient vector pointer
 * r            : output result vector pointer
 * nx,ny,nr,nh  : size of respective vectors
 * p            : number of processor to use (task parallelism)
 * team         : team to work with
 *
 ****************************************************************
 */

/*auto correlation: r[j] = sum ( x[j+k] * x[k] ), k=0..(n-j-1) */
void p_acorr_f32(float *x, float *r, int nx, int nr,
		 int p, p_team_t team);

/*convolution: r[j] = sum ( h[k] * x[j-k), k=0..(nh-1) */
void p_conv_f32(float *x, float *h, float *r, int nr, int nh,
		int p, p_team_t team);

/*cross correlation: r[j] = sum ( x[j+k] * y[k]), k=0..(nx+ny-1) */
/* NOTE: current implementation requires nx >= ny */
void p_xcorr_f32(float *x, float *y, float *r, int nx, int ny,
		 int p, p_team_t team);

/*FIR filter direct form: r[j] = sum ( h[k] * x [j-k]), k=0..(nh-1) */
void p_fir_f32(float *x, float *h, float *r, int nx, int nh,
	       int p, p_team_t team);

/*FIR filter with decimation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1) */
void p_firdec_f32(float *x, float *h, float *r, int nx, int nh, int df,
		  int p, p_team_t team);

/*FIR filter with inerpolation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1) */
void p_firint_f32(float *x, float *h, float *r, int nx, int nh, int ifactor,
		  int p, p_team_t team);

/*FIR symmetric form: */
void p_firsym_f32(float *x, float *h, float *r, int nx, int nh,
		  int p, p_team_t team);

/*IIR filter: */
void p_iir_f32(float *x, float *h, float *r, int nb, int nr,
	       int p, p_team_t team);
