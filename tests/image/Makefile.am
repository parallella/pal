LIBS = -lm

if HOST_IS_EPIPHANY
    SIMPLE = notest.c
else
    SIMPLE = simple.c
endif

LDADD = $(top_builddir)/src/libpal.la

check_PROGRAMS = check_p_rgb2grayscale \
    check_p_box3x3_f32

check_p_rgb2grayscale_SOURCES = check_p_rgb2greyscale.c \
                        rgb2greyscale_test_data.h

check_p_box3x3_f32_SOURCES = check_p_box3x3_f32.c

TESTS = $(check_PROGRAMS)

# Ubuntu 14.04 has automake-1.11. Need automake-1.12 for nicer test features.
# This will have to do for now.
TESTS_ENVIRONMENT = ../runtest

