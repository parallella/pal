/*
 * A median 3x3 filter
 *
 *       | -1  0  1 |
 * Gx =  | -2  0  2 | * 1/8
 *       | -1  0  1 |
 *
 *       | -1 -2 -1 |
 * Gy =  |  0  0  0 | * 1/8
 *       |  1  2  1 |
 *
 * G = sqrt (Gx^2 + Gy^2)
 * 
 * Gradient Direction (theta) = atan2(Gy,Gx)
 *
 */
void p_median3x3_32f (const float* x, const float* m, float* r, int rows, int cols){


/*PLACE CODE HERE*/

}
