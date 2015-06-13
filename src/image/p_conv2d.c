/*
// A= [17 24 1 8 15; 23 5 7 14 16; 4 6 13 20 22;10 12 19 21 3; 11 18 25 2 9]
// B=[1 3 1; 0 5 0; 2 1 2]
// E=[0 0 0; 0 1 0; 0 0 0]
// C2 = conv2(A,E,'valid')   <-- works correctly since kernel is symmetric   
// C1 = conv2(A,B,'valid')   <-- Doesn't work correctly

// Response from pal:
// 155.0, 135.0, 200.0, 
// 145.0, 190.0, 230.0, 
// 185.0, 225.0, 270.0,

// Response from FreeMat:
// 120, 165, 205,
// 160, 200, 245,
// 190, 255, 235,

#define W 5
#define W2 (W-2)

int main(int argc, char *argv[])
{

    int i, j;

// http://www.johnloomis.org/ece563/notes/filter/conv/convolution.html
   float src[5*5] = {17,24,1,8,15,
                    23,5,7,14,16,
                    4,6,13,20,22,
                    10,12,19,21,3,
                    11,18,25,2,9 };

   float kernel[3*3] = {
          1,3,1,
          0,5,0,
          2,1,2 };

    float dest[3*3];
    p_conv2d_f32(src, kernel, dest, 5, 5, 3);

    // src 
    for (i = 0; i < W; i++) {
        for (j = 0; j < W; j++) {    	
        	printf("%.1f, ", src[i*W+j]);
        }
        printf("\n");
    }
    
    printf("\n");
    
    // response
    for (i = 0; i < W2; i++) {
        for (j = 0; j < W2; j++) {
        	printf("%.1f, ", dest[i*W2+j]);
        }
        printf("\n");
    }
}
*/



#include <pal.h>

/** Convolution on input image 'x' with a square kernel 'm' of size 'msize'.
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param m     Pointer to convlution kernel
 *
 * @param r     Pointer to output image
 *
 * @param rows  Number of rows in input image
 *
 * @param cols  Number of columns in input image
 *
 * @param mrows number of rows in convolution kernel
 *
 * @param mcols number of cols in convolution kernel
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

void p_conv2d_f32(const float *x, const float *m, float *r, int rows, int cols,
                  int mrows, int mcols, int p, p_team_t team)

{
    int i, j, ki, kj;
    float P;
    const float *px, *pm;
    float *pr;

    px = x;
    pr = r;

    for (i = 0; i < rows - mrows+1  ; i++) {
        for (j = 0; j < cols - mcols+1 ; j++) {
            P = 0.0f;
            pm = m+(mcols * mrows)-1;
            for (ki=0 ; ki< mrows ; ki++){
                for (kj=0 ; kj< mcols ; kj++){
                    // printf("(%f*%f)", *px, *pm); // unroll: remove me 
                    P+= (*px++)* (*pm--) ;
		}
            px += cols - mcols;
            }
            //printf("=[%f]\n", P); // sum : remove me
            px -= (mrows * cols) -1 ;
            *(pr++) = P;
        }
    printf("\n\n");
    px+=mcols-1 ;
    }
}
