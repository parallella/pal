#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>
#include <sys/time.h>
#include <pal_math.h>


#define N 8
#define M 8
#define K 8
#define R 0

float a[N*M+R];
float b[M*K+R];
float c[N*K+R];

typedef struct timeval timeval_t;

int main (){
  int  i,j,k;
  timeval_t start, stop;
  double      wall_time;

  //creating test pattern
  for (i = 0; i < M; i++){
    for (j = 0; j < N; j++){
      a[i*M+j] = (float) 1;//i*M+j
      b[i*M+j] = (float) 1;
      c[i*M+j] = (float) 0;
    }
  }
  
  //Function instantiation and timing measurement
  gettimeofday(&start, NULL);
  //for (i=0; i<M; i++){
  //  for (j=0; j<N; j++){
  //    for (k=0; k<N; k++){
  //	c[i*M+j] += a[i*N+k] * b[k*N+j];
  //    }
  //  }
  //}
  p_matmul_32f(N,M,K,a,b,c);
  gettimeofday(&stop, NULL);


  
  //Checking the result
  for (i=0; i<N; i++){
    for (j = 0; j < M; j++){
      printf("i=%d %f %f %f\n",i,a[i*M+j],b[i*M+j],c[i*M+j]); 
    }
  }
  //Measuring time
  wall_time = (stop.tv_sec - start.tv_sec) * 1000 + ((double) (stop.tv_usec - start.tv_usec) / 1000.0);
  printf("Wall time is %9.10f msec\n", wall_time);
}

