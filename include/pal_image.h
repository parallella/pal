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

/*2d convolution */
void p_conv2d_32f(const float* x, const float* m, float* r, int rows, int cols, int msize);

/*2d box filter (3x3) */
void p_box3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d gauss filter (3x3) */
void p_gauss3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d median filter (3x3) */
void p_median3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d sobel filter (3x3) */
void p_sobel3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d laplace filter (3x3) */
void p_laplacel3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d scharr filter (3x3) */
void p_scharr3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d prewitt filter (3x3) */
void p_prewitt3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d moving average filter (3x3) */
void p_average3x3_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d sum of absolute differences*/
void p_sad8x8_32f(const float* x, const float* m, float* r, int rows, int cols);
void p_sad16x16_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d sum of mean differences*/
void p_mad8x8_32f(const float* x, const float* m, float* r, int rows, int cols);
void p_mad16x16_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d integral if image */
void p_integral2d_32f(const float* x, const float* m, float* r, int rows, int cols);

/*2d resize image*/
void p_scale2d_32f(const float* x, float* r, int rows, int cols, int opt );

//what else??



