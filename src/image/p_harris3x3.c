#include <pal.h>

/* smooth structure tensor */
static void _sst3x3(float *x, float *r, int rows, int cols, 
		    int p, p_team_t team)
{
    int i, j;
    int ic3 = 1 - 3 * cols;
    int cm3 = cols - 3;
    int rm2 = rows - 2;
    int cm2 = cols - 2;
    float Dx, Dy;
    float *px, *pr;

    px = x;
    pr = r;

    for (i = 0; i < rm2; i++) {
        for (j = 0; j < cm2; j++) {

            // sobel
            Dx = -(*px);
            Dy = -(*px++);
            Dy -= (*px++) * 2;
            Dx += (*px);
            Dy -= (*px);
            px += cm2;
            Dx -= (*px) * 2;
            px += 2;
            Dx += (*px) * 2;
            px += cm2;
            Dx -= (*px);
            Dy += (*px++);
            Dy += (*px++) * 2;
            Dx += (*px);
            Dy += (*px);
            px += cm2;

            // sst
            pr[0] = Dx*Dx;
            pr[1] = Dy*Dy;
            pr[2] = Dx*Dy;

            px += ic3; // advance mask matrix in one column
            pr += 3;
        }
        
        px = px + 2; // at the last column in a row, advance pointer to the
                     // beginning of next row.
    }
}

/**
 * A Harris filter (corner detecion)
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param r     Pointer to output image, a 2D array of size 'rows-4' x 'cols-4'
 *
 * @param t     Pointer to tmp image, a 2D array of size 'rows-2' x 'cols-2' x 3
 *
 * @param rows  Number of rows in input image
 *
 * @param cols  Number of columns in input image
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 */
void p_harris3x3_f32(float *x, float *r, float *t, int rows, int cols, 
		    int p, p_team_t team)
{
	int i, j;
	int rm4 = rows - 4;
    int cm4 = cols - 4;
    int ci = (cols - 2)*3 - 8;
    int cj = (cols - 2)*6 + 5;
    float P1, dy2, dxy;
    float *pr, *pt;

    pr = r;
    pt = t;
    
    _sst3x3(x, t, rows, cols, p, team);
	
    for (i = 0; i < rm4; i++) {
        for (j = 0; j < cm4; j++) {
        	
        	float dx2;
            float dy2;
            float dxy;
            
        	// gaussian
        	dx2 = (*pt++);
        	dy2 = (*pt++);
        	dxy = (*pt++);
        	dx2 += (*pt++) * 2;
        	dy2 += (*pt++) * 2;
        	dxy += (*pt++) * 2;
        	dx2 += (*pt++);
        	dy2 += (*pt++);
        	dxy += (*pt);
        	pt += ci;
        	dx2 += (*pt++) * 2;
        	dy2 += (*pt++) * 2;
        	dxy += (*pt++) * 2;
        	dx2 += (*pt++) * 4;
        	dy2 += (*pt++) * 4;
        	dxy += (*pt++) * 4;
        	dx2 += (*pt++) * 2;
        	dy2 += (*pt++) * 2;
        	dxy += (*pt) * 2;
        	pt += ci;
        	dx2 += (*pt++);
        	dy2 += (*pt++);
        	dxy += (*pt++);
        	dx2 += (*pt++) * 2;
        	dy2 += (*pt++) * 2;
        	dxy += (*pt++) * 2;
        	dx2 += (*pt++);
        	dy2 += (*pt++);
        	dxy += (*pt);
        	pt -= cj;
        	
        	// harris response
            *pr = ((dx2 * dy2 - dxy * dxy) / (dx2 + dy2 + 1e-8f)) * 0.015625f; // * 1/64
            pr++;
        }
        
        pt += 6;
    }
}
