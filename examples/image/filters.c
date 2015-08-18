#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pal_image.h>

/* stb image */
#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image.h"
#include "stb_image_write.h"

/* macros */
#define CLAMP(x, low, high) (((x) > (high)) ? (high) : (((x) < (low)) ? (low) : (x)))

void set_working_dir(char *argv[])
{
    char *wdir = strdup(argv[0]);
    char *s = strrchr(wdir,'/');
    *s = '\0';
    if (chdir(wdir)) {
        perror(argv[0]);
        exit(EXIT_FAILURE);
    }
    free(wdir);
}

void float_to_ubyte(unsigned char *dest, float *src, int size)
{
    int i;
    for (i = 0; i < size; i++)
        dest[i] = (unsigned char)CLAMP(src[i] * 255, 0, 255);
}


int main(int argc, char *argv[])
{
    float *data;
    float *dest;
    unsigned char *data_ub; /* ubyte data */
    int w, h, n; /* with, height, n-components*/
    int i, size;
    
    set_working_dir(argv);
    
    /* read and convert image */
    {
        float uctof = 1.0f / 255.0f;
        
        /* read */
        data_ub = stbi_load(ABS_TOP_SRCDIR "/examples/image/dataset/lena.tga",
                            &w, &h, &n, 0);
        if (! data_ub) {
            printf("unable to read image\n");
            return EXIT_FAILURE;
        }
        
        /* allocate memory */
        size = w * h;
        data = malloc(size * sizeof(float));
        dest = malloc(size * sizeof(float));
        
        /* convert to float */
        if (n == 1) /* already greyscale */
            for (i = 0; i < size; i++)
                data[i] = (float)data_ub[i] * uctof;
        else if(n >= 3) /* convert to greyscale */
            for (i = 0; i < size; i++)
                data[i] = (data_ub[i*n]*0.2f + data_ub[i*n + 1]*0.7f + data_ub[i*n + 2]*0.1f) * uctof;
    }
    
    /* gaussian */
    {
        p_gauss3x3_f32(data, dest, h, w);
        float_to_ubyte(data_ub, dest, size);
        stbi_write_tga("../dataset/lena_gaussian.tga", w, h, 1, data_ub);
    }
    
    /* harris */
    {
        float *tmp = malloc(size * 3 * sizeof(float));
        p_harris3x3_f32(data, dest, tmp, h, w);
	free(tmp);
        float_to_ubyte(data_ub, dest, size);
        stbi_write_tga("../dataset/lena_harris.tga", w-4, h-4, 1, data_ub);
    }
    
    /* sobel */
    {
        p_sobel3x3_f32(data, dest, h, w);
        float_to_ubyte(data_ub, dest, size);
        stbi_write_tga("../dataset/lena_sobel.tga", w, h, 1, data_ub);
    }

    /* scharr */
    {
        p_scharr3x3_f32(data, dest, h, w);
        float_to_ubyte(data_ub, dest, size);
        stbi_write_tga("../dataset/lena_scharr.tga", w, h, 1, data_ub);
    }

    /* prewitt */
    {
        p_prewitt3x3_f32(data, dest, h, w);
        float_to_ubyte(data_ub, dest, size);
        stbi_write_tga("../dataset/lena_prewitt.tga", w, h, 1, data_ub);
    }
    
    /* box */
    {
        p_box3x3_f32(data, dest, h, w);
        float_to_ubyte(data_ub, dest, size);
        stbi_write_tga("../dataset/lena_box.tga", w-2, h-2, 1, data_ub);
    }
    
    /* median */
    {
        p_median3x3_f32(data, dest, h, w);
        float_to_ubyte(data_ub, dest, size);
        stbi_write_tga("../dataset/lena_median.tga", w-2, h-2, 1, data_ub);
    }
    
    free(dest);
    free(data);
    stbi_image_free(data_ub);
    return EXIT_SUCCESS;
}
