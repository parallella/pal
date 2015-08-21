/* filters 2 device file */
#include <pal.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

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

#if 1
// HACK: PAL will automagically do the function call for us
// Everything below will unnecessary
struct epiphany_ctrl_mem {
    uint32_t status[16]; // status field for team
    uint32_t argsoffset;
} __attribute__((packed));

struct epiphany_args_header {
    uint32_t nargs;
    uint32_t __pad1;
    uint32_t size[P_RUN_MAX_ARGS];
} __attribute__((packed));

int main()
{
    float *out = (float *) 0xdeadbeef;
    float *img;
    int h, w;

#define CTRL_MEM_SIZE 4096
#define CTRL_MEM_EADDR (0x90000000-CTRL_MEM_SIZE)
#define ARGS_MEM_END_EADDR CTRL_MEM_EADDR
    volatile struct epiphany_ctrl_mem *ctrl =
        (struct epiphany_ctrl_mem *) CTRL_MEM_EADDR;

    uintptr_t argsoffset = ctrl->argsoffset;

    struct epiphany_args_header *header = (struct epiphany_args_header *)
        (((uintptr_t) ARGS_MEM_END_EADDR) - argsoffset);

    uint8_t *arg0 = (void *) &header[1];

    uint8_t *argp = arg0;

    memcpy(&h, argp, sizeof(h));
    argp += sizeof(h);
    memcpy(&w, argp, sizeof(w));
    argp += sizeof(w);

    size_t size = w * h;
    img = (float *) argp;
    argp += size * sizeof(float);
    memcpy(&out, argp, sizeof(out));
    argp += sizeof(out);

    gauss3x3(img, out, h, w);

    return 0;
}
#endif
