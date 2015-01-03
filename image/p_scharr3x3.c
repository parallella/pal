/*
 * A Scharr 3x3 convolution filter with the following convolution matrix: 
 *
 *       |  3  0 -3 |
 * Gx =  | 10  0 10 | * 1/?
 *       |  3  0 -3 |
 *
 *       |  3 10  3 |
 * Gy =  |  0  0  0 | * 1/?
 *       | -3 10 -3 |
 *
 * G = sqrt (Gx^2 + Gy^2)
 * 
 * Gradient Direction (theta) = atan2(Gy,Gx)
 *
 */

void p_scharr3x3_32f (const float* x, float* r, int rows, int cols){




}
