#include <pal_math.h>
#include <stdio.h>

#define N 19

float a[N];
float b[N];
float c[N];

int main (){
  int  i,j;
  
  //creating test pattern
  for (i = 0; i < N; i++){
      a[i] = (float) i;
      b[i] = (float) i;
  }
  
  //function to test
  p_add_32f(a,b,c,N);
    
  //printing result
  for (i=0; i<N; i++){
      printf("i=%d %f %f %f\n",i,a[i],b[i],c[i]); 
  }
}

