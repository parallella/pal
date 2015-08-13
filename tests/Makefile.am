SUBDIRS = dsp image math 

EXTRA_DIST = runtest.noimpl.c runtest.simple.c

if HOST_IS_EPIPHANY
RUNTEST_SRC = runtest.epiphany.c
# TODO: Move to configure.ac.
ESDK              = /opt/adapteva/esdk
EINCS             = -I$(ESDK)/tools/host/include
ELIBS             = -L$(ESDK)/tools/host/lib
CFLAGS_FOR_BUILD  += $(EINCS)
LDFLAGS_FOR_BUILD += $(ELIBS) -le-hal -le-loader
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
	$(CC_FOR_BUILD) $(CFLAGS_FOR_BUILD) $< -o $@ $(LDFLAGS_FOR_BUILD)

check-local: runtest

clean-local:
	-rm -f runtest$(BUILD_EXEEXT)
