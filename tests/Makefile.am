SUBDIRS = dsp image math 

EXTRA_DIST = runtest.noimpl.c runtest.simple.c

if HOST_IS_EPIPHANY
    RUNTEST_SRC = runtest.noimpl.c # TODO: implement
else
    RUNTEST_SRC = runtest.simple.c
endif

BUILT_SOURCES = runtest$(BUILD_EXEEXT) libutest.la

noinst_LTLIBRARIES = libutest.la
libutest_la_SOURCES = \
    utest.c \
    utest-report.c

noinst_HEADERS = utest.h

runtest$(BUILD_EXEEXT): $(RUNTEST_SRC)
	$(CC_FOR_BUILD) $(BUILD_CFLAGS) $< -o $@ $(BUILD_LDFLAGS)

check-local: runtest

clean-local:
	-rm -f runtest$(BUILD_EXEEXT)
