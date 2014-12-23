void p_add_32f ( const float* a,  const float* b,  float* c, int n ){


  int i,cnt;   
 
  //Partially unrolled inner loop
  for(i=0;i<(n-(n&0x3));i=i+4){
    //Insert SIMD pragmas here if needed?
    *(c+i)   = *(a+i)   + *(b+i);
    *(c+i+1) = *(a+i+1) + *(b+i+1);
    *(c+i+2) = *(a+i+2) + *(b+i+2);
    *(c+i+3) = *(a+i+3) + *(b+i+3);
  }  
  //Computing remaining items
  for(i=(n-(n&0x3));i<n;i++){
    *(c+i) = *(a+i) + *(b+i);
  }   
}
