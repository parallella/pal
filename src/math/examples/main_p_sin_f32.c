#include <stdio.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <stdint.h>
#include <pal.h>
#include <math.h>

int main(int argc, char **argv)
{
    //Getopt vars
    int opt;
    extern char *optarg; 

    float testval;
    int n;   
    int i;

    while ( (opt = getopt(argc, argv, "f:n:")) != -1) {
        switch (opt) {             
        case 'f':
	    testval =  atof(optarg);
            break;
	case 'n':
	    n =  atoi(optarg);
            break;
        case '?':
            break;
        default:
            printf ("??getopt returned character code 0%o??\n", opt);
        }
    }

    //Allocate/init array
    float *a;
    float *c;
    a=malloc(n*sizeof(float));
    c=malloc(n*sizeof(float));    
    for (i=0;i<n;i++){
	a[i]=testval;
      }

    //Execute
    p_sin_f32(a, c, n);    
    
    //Print result
    for (i=0;i<n;i++){
	printf("%f c=%f ref=%f\n", a[i], c[i],sin(a[i]));
    }
}

