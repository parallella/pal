#include <complex.h>

/*plan vs static structures, really only supports up to a few thousands points, simple*/
/*second layer adds 2D, different library*/ 



/*complex 1D FFT*/
void p_cfft1d (float* r, float* i, int n, int isign, float* wsave);

/*complex 2D FFT*/
void p_cfft2d (float* r, float* i, int n, int isign, float* wsave);
void p_cfft2d (float* r, float* i, int m, int n, int isign )

/*complex 2D FFT, in place, pass a pointer to structure*/
void p_cfft_32f(float* , float* i, int m, int n, int flags )
