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
 * p         : number of processors "parallelism"
 * team      : team to work with
 *
 ****************************************************************
 */

/*2d convolution */
void p_conv2d_f32(float *x, float *m, float *r, int rows, int cols, int msize,
		  int p, p_team_t team);

/*2d box (i.e mean) filter(3x3) */
void p_box3x3_f32(float *x, float *r, int rows, int cols, 
		  int p, p_team_t team);

/*2d gauss filter (3x3) */
void p_gauss3x3_f32(float *x, float *r, int rows, int cols,
		    int p, p_team_t team);

/*2d median filter (3x3) */
void p_median3x3_f32(float *x, float *r, int rows, int cols,
		     int p, p_team_t team);

/*2d laplace filter (3x3) */
void p_laplace3x3_f32(float *x, float *r, int rows, int cols,  
		      int p, p_team_t team);

/*2d scharr filter (3x3) */
void p_scharr3x3_f32(float *x, float *r, int rows, int cols,
		     int p, p_team_t team);

/*2d prewitt filter (3x3) */
void p_prewitt3x3_f32(float *x, float *r, int rows, int cols, 
		      int p, p_team_t team);

/*2d sum of absolute differences (8x8), returns scalar */
void p_sad8x8_f32(float *x, float *m, float *r, int cols, 
		  int p, p_team_t team);

/*2d sum of absolute differences (16x16), returns scalar */
void p_sad16x16_f32(float *x, float *m, float *r, int cols, 
		    int p, p_team_t team);

/*2d sobel filter (3x3) */
void p_sobel3x3_f32(float *x, float *r, int rows, int cols, 
		    int p, p_team_t team);

/*2d harris filter (3x3) */
void p_harris3x3_f32(float *x, float *r, float *t, int rows, int cols, 
		    int p, p_team_t team);
