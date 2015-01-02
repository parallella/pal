/*
 ****************************************************************
 * 1D: Filters, Convolution, Correlation Functions
 *
 * x,y          : input vector pointers
 * h            : input coefficient vector pointer
 * r            : output result vector pointer
 * dbuf         : delay buffer pointer
 * nx,ny,nr,nh  : size of respective vectors
 * 
 ****************************************************************
 */

/*auto correlation: r[j] = sum ( x[j+k] * x[k] ), k=0..(n-j-1) */
void p_acorr_32f( const float* x, float* r, int nx );

/*convolution: r[j] = sum ( h[k] * x[j-k), k=0..(nh-1) */
void p_conv_32f( const float* x, const float* h, float* r, int nx, int nh );

/*correlation: r[j] = sum ( x[j+k] * y[k]), k=0..(nx+ny-1) */
void p_corr_32f(const float* x, const float* y, float* r, int nx, int ny );

/*FIR filter direct form: r[j] = sum ( h[k] * x [j-k]), k=0..(nh-1) */
void p_fir_32f(const float* x, const float* h, float* r, float* dbuf, int nx, int nh );

/*FIR filter with decimation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1) */
void p_firdec_32f(const float* x, const float* h, float* r, float* dbuf, int nx, int nh, int dfactor );

/*FIR filter with inerpolation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1) */
void p_firint_32f(const float* x, const float* h, float* r, float* dbuf, int nx, int nh, int ifactor );

/*FIR filter lattice form*/
void p_firlat_32f(const float* x, const float* h, float* r, float* dbuf, int nx, int nh);

/*FIR symmetric form: */
void p_firsym_32f(const float* x, const float* h, float* r, float* dbuf, int nx, int nh);

/*IIR filter: */
void p_iir_32f(const float* x, const float* h, float* r, float* dbuf, int nx, int nh);



