//#include <stdio.h>

//why m,n,k, makes no sense!
//1.Work on 4 rows and columns at a time
//2.Bring in 16 A values, 16 B values  (16 loads)
//3.compute c16 (64)
//4.loop around 2&3 until done (8 times for 32x32)
//5.Store c16 (8 stores)


void p_matmul4x4_32f(int n, float *a, float *b, float *c);

void p_matmul_32f(int m,         //number of rows in 'a'
		  int n,         //number of columns in 'a'
		  int k,         //number of columns in 'b' 
		  float* a,      //pointer to input array a
		  float* b,      //pointer to input array b
		  float* c ){    //pointer to output array c

  int i,j,kk;   
  int as,bs;

  as=0;
  for(i=0;i<m;i=i+4){  //4 rows at a time
    for(j=0;j<n;j=j+4){//4 columns at a time
      bs=0;
      for(kk=0;kk<n;kk=kk+4){//inner summation 
	//function below should be assembly
	p_matmul4x4_32f(n, a+as+kk, b+bs+j, c+as+j);	
	bs=bs+n<<2;
      }
    }
    as=as+n<<2;
  }
}


void p_matmul4x4_32f(int n, float *a, float* b, float *c){

  int i;
  int bs=0;
  //calculate the outer product
  //messy addressing, leverage autoincrement addressing mode later
  
  for (i=0; i<4; i++){
    
    c[0]       += a[i] * b[(bs) ];    //2 64 bit loads for B
    c[1]       += a[i] * b[(bs) + 1]; //4 32 bit loads for A
    c[2]       += a[i] * b[(bs) + 2];
    c[3]       += a[i] * b[(bs) + 3];

    c[n]       += a[n+i] * b[(bs) ];
    c[n+1]     += a[n+i] * b[(bs) + 1 ];
    c[n+2]     += a[n+i] * b[(bs) + 2 ];
    c[n+3]     += a[n+i] * b[(bs) + 3 ];
    
    c[n+n]     += a[n+n+i] * b[(bs) ];
    c[n+n+1]   += a[n+n+i] * b[(bs) + 1 ];
    c[n+n+2]   += a[n+n+i] * b[(bs) + 2 ];
    c[n+n+3]   += a[n+n+i] * b[(bs) + 3 ];
    
    c[n+n+n]   += a[n+n+n+i] * b[(bs) ];
    c[n+n+n+1] += a[n+n+n+i] * b[(bs) + 1];
    c[n+n+n+2] += a[n+n+n+i] * b[(bs) + 2];
    c[n+n+n+3] += a[n+n+n+i] * b[(bs) + 3];

    bs=bs+n;
  }
}

