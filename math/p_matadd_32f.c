#include <stdio.h>

void p_matadd_32f(int m,         //number of rows
		  int n,         //number of columns
		  float* a,      //pointer to input aray a
		  float* b,      //pointer to input array b
		  float* c ){    //pointer to output array c

  int i,j,cnt;   

  //Can't assume CPU has an integer multiply
  cnt=0;
  //Outer loop (per row)
  for(i=0;i<m;i++){
    //Partially unrolled inner loop
    for(j=0;j<(n-(n&0x3));j=j+4){
      //Insert SIMD pragmas here if needed
      *(c+cnt+j)   = *(a+cnt+j)   + *(b+cnt+j);
      *(c+cnt+j+1) = *(a+cnt+j+1) + *(b+cnt+j+1);
      *(c+cnt+j+2) = *(a+cnt+j+2) + *(b+cnt+j+2);
      *(c+cnt+j+3) = *(a+cnt+j+3) + *(b+cnt+j+3);
    }  
    //Computing remaining items
    for(j=(n-(n&0x3));j<n;j++){
      *(c+cnt+j) = *(a+cnt+j) + *(b+cnt+j);
    }
    cnt=cnt+n;
  }
}

  
