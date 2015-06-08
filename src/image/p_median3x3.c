#include <pal.h>

/*
 * The following routines have been built from knowledge gathered
 * around the Web. I am not aware of any copyright problem with
 * them, so use it as you want.
 * N. Devillard - 1998
 */

typedef float pixelvalue ;

#define PIX_SORT(a,b) { if ((a)>(b)) PIX_SWAP((a),(b)); }
#define PIX_SWAP(a,b) { pixelvalue temp=(a);(a)=(b);(b)=temp; }

/*----------------------------------------------------------------------------
   Function :   opt_med9()
   In       :   pointer to an array of 9 pixelvalues
   Out      :   a pixelvalue
   Job      :   optimized search of the median of 9 pixelvalues
   Notice   :   in theory, cannot go faster without assumptions on the
                signal.
                Formula from:
                XILINX XCELL magazine, vol. 23 by John L. Smith
  
                The input array is *NOT* modified in the process
                The result array is guaranteed to contain the median
                value
 ---------------------------------------------------------------------------*/

pixelvalue opt_med9(pixelvalue * pointer)
{
    pixelvalue p[9];
    memcpy(p, pointer, 9*sizeof(pixelvalue) );
    PIX_SORT(p[1], p[2]) ; PIX_SORT(p[4], p[5]) ; PIX_SORT(p[7], p[8]) ;
    PIX_SORT(p[0], p[1]) ; PIX_SORT(p[3], p[4]) ; PIX_SORT(p[6], p[7]) ;
    PIX_SORT(p[1], p[2]) ; PIX_SORT(p[4], p[5]) ; PIX_SORT(p[7], p[8]) ;
    PIX_SORT(p[0], p[3]) ; PIX_SORT(p[5], p[8]) ; PIX_SORT(p[4], p[7]) ;
    PIX_SORT(p[3], p[6]) ; PIX_SORT(p[1], p[4]) ; PIX_SORT(p[2], p[5]) ;
    PIX_SORT(p[4], p[7]) ; PIX_SORT(p[4], p[2]) ; PIX_SORT(p[6], p[4]) ;
    PIX_SORT(p[4], p[2]) ; return(p[4]) ;
}

/*
 * A median 3x3 filter.
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param r     Pointer to output image
 *
 * @param rows  Number of rows in input image
 *
 * @param cols  Number of columns in input image
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

void p_median3x3_f32(const float *x, float *r, int rows, int cols, 
		     int p, p_team_t team)
{
    float buffer[9];
    float sorted[9] ;
    float *px, *pr;
    int i, j, buffer_pointer;

    px = x;
    pr = r;

    for (i = 0; i < rows - 2; i++) {
        // fully filling first window
        buffer[0] = *(px);
        buffer[1] = *(px + cols);
        buffer[2] = *(px + cols + cols);

        buffer[3] = *(px + 1);
        buffer[4] = *(px + cols + 1);
        buffer[5] = *(px + cols + cols + 1);
        buffer_pointer = 6 ;
        // other windows differ only by one column
        // so only one column is added to the place where buffer pointer points
        for (j = 2; j < cols  ; j++) {
            // in each iteration, three values are replaced in the circular queue
            buffer_pointer = buffer_pointer % 9;
            buffer[buffer_pointer]   = *(px + j ) ;
            buffer[buffer_pointer+1] = *(px + j + cols );
            buffer[buffer_pointer+2] = *(px + j + cols + cols );
            buffer_pointer+= 3 ;
            //p_median_f32(buffer, pr, 9, 0, 0);
            *pr = opt_med9(buffer);
            pr++;
        }
        px += cols ;
    }
}
