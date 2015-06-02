#include "pal.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    p_team_t team0;

    unsigned int size_a = 10;
    unsigned int size_b = 15;
    unsigned int size_c = 7;

    float a[] = {1, 3, 3, 3, 4, 4, 6, 6, 6, 9};
    float b[] = {3.f, 7.f, 5.f, 1.3f, 2.0f, 2.3f, 3.9f, 2.3f, 4.0f, 2.3f, 1.4f, 1.2f, 5.6f, 2.3f, 2.9f};
    float c[] = {19, 8, 29, 35, 19, 28, 15};
    
    float m_a = 0.0f;
    float m_b = 0.0f;
    float m_c = 0.0f;

    unsigned int i = 0;

    p_mode_f32(a, &m_a, size_a, 0, team0);
    p_mode_f32(b, &m_b, size_b, 0, team0);
    p_mode_f32(c, &m_c, size_c, 0, team0);

    printf("[");
    for (i = 0; i < size_a; ++i) {
        printf("%2.5f ", a[i]);
    }
    printf("]\nMode: %2.5f\n", m_a);

    printf("[");
    for (i = 0; i < size_b; ++i) {
        printf("%2.5f ", b[i]);
    }
    printf("]\nMode: %2.5f\n", m_b);

    printf("[");
    for (i = 0; i < size_c; ++i) {
        printf("%2.5f ", c[i]);
    }
    printf("]\nMode: %2.5f\n", m_c);

    return 0;
}
