SUBDIRS = dsp image math 

# TODO: Move to configure.ac. Use expansion in here instead.
BUILD_CC=gcc

EXTRA_DIST = runtest.noimpl.c runtest.simple.c

if HOST_IS_EPIPHANY
    RUNTEST_SRC = runtest.noimpl.c # TODO: implement
else
    RUNTEST_SRC = runtest.simple.c
endif

BUILT_SOURCES = runtest libutest.la

noinst_LTLIBRARIES = libutest.la
libutest_la_SOURCES = \
    utest.c \
    utest-report.c

noinst_HEADERS = utest.h

runtest: $(RUNTEST_SRC)
	$(BUILD_CC) $(BUILD_CFLAGS) $< -o $@ $(BUILD_LDFLAGS)

check-local: runtest

clean-local:
	-rm -f runtest
