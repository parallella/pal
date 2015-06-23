#pragma once
#include "pal_base.h"
/*
 ****************************************************************
 * Image Processing Functions (raw/native format)
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
void p_conv2d_f32(const float *x, float *m, float *r, int rows, int cols,
                  int mrows, int mcols);

/*2d box (i.e mean) filter(3x3) */
void p_box3x3_f32(const float *x, float *r, int rows, int cols);

/*2d gauss filter (3x3) */
void p_gauss3x3_f32(const float *x, float *r, int rows, int cols);

/*2d median filter (3x3) */
void p_median3x3_f32(const float *x, float *r, int rows, int cols);

/*2d laplace filter (3x3) */
void p_laplace3x3_f32(const float *x, float *r, int rows, int cols);

/*2d scharr filter (3x3) */
void p_scharr3x3_f32(const float *x, float *r, int rows, int cols);

/*2d prewitt filter (3x3) */
void p_prewitt3x3_f32(const float *x, float *r, int rows, int cols);

/*2d sum of absolute differences (8x8), returns scalar */
void p_sad8x8_f32(const float *x, const float *img, float *r, int rows, int cols);

/*2d sum of absolute differences (16x16), returns scalar */
void p_sad16x16_f32(const float *x, const float *img, float *r, int rows, int cols);

/*2d sobel filter (3x3) */
void p_sobel3x3_f32(const float *x, float *r, int rows, int cols);

/*2d harris filter (3x3) */
void p_harris3x3_f32(const float *x, float *r, float *t, int rows, int cols);

/*RGB to Grayscale conversion */
void p_rgb2grayscale_f32(const float *x, float *r, int rows, int cols);
