/* filters 2 device file */
#include <pal.h>
#include <string.h>

#ifndef __epiphany__
#error "Atm. only Epiphany is supported"
#endif

#define CLAMP(x, low, high) (((x) > (high)) ? (high) : (((x) < (low)) ? (low) : (x)))

void float_to_ubyte(unsigned char *dest, float *src, int size)
{
    int i;
    for (i = 0; i < size; i++)
        dest[i] = (unsigned char)CLAMP(src[i] * 255.0f, 0.0f, 255.0f);
}

void gauss3x3(float *x, float *r, int rows, int cols)
{
    size_t size = rows * cols;
    float my_x[size];
    float my_r[size];

    memcpy(my_x, x, size * sizeof(float));

    p_gauss3x3_f32(my_x, my_r, rows, cols);

    memcpy(r, my_r, size * sizeof(float));
}

/* We still need a symbol for main, even though we won't use it.
 * Without it the program cannot link. */
void main()
{
}
