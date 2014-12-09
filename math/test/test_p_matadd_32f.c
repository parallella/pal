#include <pal_math.h>
#include <stdio.h>

#define N 4
#define M 4
#define K 4
#define R 0

float a[N*M+R];
float b[M*K+R];
float c[N*K+R];

int main (){
  int  i,j;
  
  //creating test pattern
  for (i = 0; i < N; i++){
    for (j = 0; j < M; j++){
      a[i*M+j] = (float) i*M+j;
      b[i*M+j] = (float) i*M+j;
    }
  }
  
  //function to test
  p_matadd_32f(N,M,a,b,c);
    
  //printing result
  for (i=0; i<N; i++){
    for (j = 0; j < M; j++){
      printf("i=%d %f %f %f\n",i,a[i*M+j],b[i*M+j],c[i*M+j]); 
    }
  }
}
