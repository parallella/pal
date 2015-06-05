#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pal_image.h>

/* Harris corner detection test */

#define W 13
#define W2 (W-2)
#define W4 (W-4)

int main(int argc, char *argv[])
{
    p_team_t team;
    int i, j;
    
    float src[W*W] = {
    	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0,
    	0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0,
    	0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0,
    	0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0,
    	0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0,
    	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    };
    float dest[W4*W4];
    float tmp[W2*W2*3];
 
    p_harris3x3_f32(src, dest, tmp, W, W, 1, team);
    
    /* src */
    for (i = 0; i < W; i++) {
        for (j = 0; j < W; j++) {
        	
        	printf("%.1f, ", src[i*W+j]);
        }
        printf("\n");
    }
    
    printf("\n\n");
    
    /* harris response */
    for (i = 0; i < W4; i++) {
        for (j = 0; j < W4; j++) {
        	
        	printf("%.1f, ", dest[i*W4+j]);
        }
        printf("\n");
    }
}
