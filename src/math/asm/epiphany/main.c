#include <stdio.h>
#define N 16
int p_add_f32(float *a, float *b, float *c, int n);
int main(){

    float a[N],b[N],c[N];
    int i;

    // init data
    for(i=0;i<N;i++){
	a[i]=(float) i;
	b[i]=(float) i;;
    }

    // function
    p_add_f32(a,b,c,N);
}
/*
int p_add_f32(float *a, float *b, float *c, int n){
    int i;
    for(i=0;i<n;i++){
	*(c + i) = *(a + i) + *(b + i);
    }  
}
*/
