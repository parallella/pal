#include "../bench_tmpl.h"

declare_image(p_box3x3_f32)
//p_conv2d_f32)
declare_image(p_gauss3x3_f32)
//p_grayscale_f32)
//p_harris3x3_f32)
declare_image(p_laplace3x3_f32)
declare_image(p_median3x3_f32)
declare_image(p_prewitt3x3_f32)
//p_sad16x16_f32)
//p_sad8x8_f32)
declare_image(p_scharr3x3_f32)
declare_image(p_sobel3x3_f32)

const struct p_bench_item benchmark_items[] = {
    item(p_box3x3_f32),
    //p_conv2d_f32),
    item(p_gauss3x3_f32),
    //p_grayscale_f32),
    //p_harris3x3_f32),
    item(p_laplace3x3_f32),
    item(p_median3x3_f32),
    item(p_prewitt3x3_f32),
    //p_sad16x16_f32),
    //p_sad8x8_f32),
    item(p_scharr3x3_f32),
    item(p_sobel3x3_f32),

    { NULL, NULL }
};
