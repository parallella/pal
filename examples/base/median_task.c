#include <stdio.h>
#include <pal.h>

int main()
{
    int a_n=6;
    float a[6] = {1., 2., 3., 4., 5., 6.};
    float a_expected_median=3.5;

    int b_n=5;
    float b[5] = {1., 2., 3., 4., 5.};
    float b_expected_median=3.;

    int match;
    float median;

	puts("Running first test");

    p_median_f32(a, &median, a_n);

    if(median==a_expected_median)
        match=1;
    else
        match=0;

    printf("Calculated median: %f\n", median);
    printf("Matches expected: %s\n", match ? "yes" : "no");

	puts("\nRunning second test");

    p_median_f32(b, &median, b_n);

    if(median==b_expected_median)
        match=1;
    else
        match=0;

    printf("Calculated median: %f\n", median);
    printf("Matches expected: %s\n", match ? "yes" : "no");

	return 0;
}
