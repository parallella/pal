#include <stdio.h>      /* printf, scanf, puts, NULL */
#include <stdlib.h>     /* srand, rand */
#include <time.h>       /* time */

int main(int argc, char *argv[])
{
    int i;
    srand((unsigned int)time(NULL));
    float x,y,z;
    for (i=0;i<100;i++){
	x =1.0f-(float)rand()/(float)(RAND_MAX/2);
	y =1.0f-(float)rand()/(float)(RAND_MAX/2);	
        printf("%f,%f,0,0\n", x,y);
    }
    return 0;
}
