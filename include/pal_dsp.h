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
void p_convol_32f( const float* x, const float* h, float* r, int nx, int nh );

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

/*
 ****************************************************************
 * Image Processing Functions
 *
 * x,y       : input matrix pointer
 * m         : filter mask pointer
 * r         : output matrix pointer
 * rows      : rows in input image
 * columns   : columns in input image (multiple of 4)
 * msize     : mask size (square)
 * opt       : options
 *
 ****************************************************************
 */

/*2D convolution */
void p_convol2D_32f(const float* x, const float* m, float* r, int rows, int cols, int msize);

/*2D box filter (3x3) */
void p_box3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D gauss filter (3x3) */
void p_gauss3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D median filter (3x3) */
void p_median3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D sobel filter (3x3) */
void p_sobel3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D laplace filter (3x3) */
void p_laplacel3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D scharr filter (3x3) */
void p_scharr3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D prewitt filter (3x3) */
void p_prewitt3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D moving average filter (3x3) */
void p_average3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D canny filter */
void p_canny_32f(const float* x, const float* m, float* r, int rows, int cols, int msize);

/*2D sum of absolute differences*/
void p_sad8x8_32f(const float* x, const float* m, float* r, int rows, int cols);
void p_sad16x16_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D sum of mean differences*/
void p_mad8x8_32f(const float* x, const float* m, float* r, int rows, int cols);
void p_mad16x16_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D integral if image */
void p_integral2D_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2D fast convolution (FFT based) */
void p_fastconv_32f(const float* x, const float* y, float* r, int rows, int cols );

/*2D resize image*/
void p_resize_32f(const float* x, float* r, int rows, int cols, int opt );

//what else??



