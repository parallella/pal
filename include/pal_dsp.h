/* 
 * 1D DSP Functions
 */

/*auto correlation*/
void p_acorrs_32f(int n, float* a, float* y );

/*convolution*/
void p_conv_32f(int n, float* a, float* h, float* y );

/*correlation*/
void p_corr_32f(int n, float* a, float* b, float* y );

/*real fir filter*/
void p_fir_32f(int nx, int nh, float* a, float* h, float* y );

/*decimating fir filter*/
void p_firdec_32f(int nx, int nh, float* a, float* h, float* y );

/*interpolating fir filter*/
void p_firint_32f(int nx, int nh, float* a, float* h, float* y );

/*lattice fir filter*/
void p_firlat_32f(int nx, int nh, float* a, float* h, float* y );

/*iir filter*/
void p_iir_32f(int nx, int nh, float* a, float* h, float* y );

/*histogram*/
void p_hist_32f(int n, float* a, float* y );

/*
 *2D DSP Functions (image processing mostly)
 */

void p_filter2d_32f(float * restrict A, int na, int ma, float * restrict M, 
		    int nm, int mm, float * restrict B);

/*moving average filter*/
void p_ave2d_32f(float * restrict A, int na, int ma, int nm, float scf, 
	       float * restrict B);

/*median filter*/
void p_median2d_32f();

/*sobel filter*/
void p_sobel2d_32f();

/*correlation function*/
void p_corr2d_32f();

/*auto-correlation function*/
void p_acorr2d_32f();

/*sum of absolute differences*/
void p_sad2d_32f();

/*mad*/
void p_mad2d_32f();

/*histogram function*/
void p_hist2d_32f();

/*histogram function*/
void p_threshold2d_32f();

/*sobel/canny/harris/threshold/color conversion/dog/ */









