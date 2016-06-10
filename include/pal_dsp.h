#pragma once

/*
 ****************************************************************
 * 1D: Filters, Convolution, Correlation Functions
 *
 * x,y          : input vector pointers
 * h            : input coefficient vector pointer
 * r            : output result vector pointer
 * nx,ny,nr,nh  : size of respective vectors
 *
 ****************************************************************
 */

/*auto correlation: r[j] = sum ( x[j+k] * x[k] ), k=0..(n-j-1) */
void p_acorr_f32(const float *x, float *r, int nx, int nr);
void p_acorr_f64(const double *x, double *r, int nx, int nr);

/*convolution: r[j] = sum ( h[k] * x[j-k), k=0..(nh-1) */
void p_conv_f32(const float *x, const float *h, float *r, int nr, int nh);
void p_conv_f64(const double *x, const double *h, double *r, int nr, int nh);

/*cross correlation: r[j] = sum ( x[j+k] * y[k]), k=0..(nx+ny-1) */
/* NOTE: current implementation requires nx >= ny */
void p_xcorr_f32(const float *x, const float *y, float *r, int nx, int ny);
void p_xcorr_f64(const double *x, const double *y, double *r, int nx, int ny);

/*FIR filter direct form: r[j] = sum ( h[k] * x [j-k]), k=0..(nh-1) */
void p_fir_f32(const float *x, const float *h, float *r, int nx, int nh);
void p_fir_f64(const double *x, const double *h, double *r, int nx, int nh);

/*FIR filter with decimation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1) */
void p_firdec_f32(const float *x, const float *h, float *r,
                  int nx, int nh, int df);
void p_firdec_f64(const double *x, const double *h, double *r,
                  int nx, int nh, int df);

/*FIR filter with inerpolation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1) */
void p_firint_f32(const float *x, const float *h, float *r,
                  int nx, int nh, int ifactor);
void p_firint_f64(const double *x, const double *h, double *r,
                  int nx, int nh, int ifactor);

/*FIR symmetric form: */
void p_firsym_f32(const float *x, const float *h, float *r, int nx, int nh);
void p_firsym_f64(const double *x, const double *h, double *r, int nx, int nh);

/*IIR filter: */
void p_iir_f32(const float *x, const float *h, float *r, int nb, int nr);
void p_iir_f64(const double *x, const double *h, double *r, int nb, int nr);
