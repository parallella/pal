if ENABLE_DEV_EPIPHANY
    MAYBE_EPIPHANY = epiphany
    MAYBE_LIB_EPIPHANY = epiphany/libdevice.la
endif

SUBDIRS = $(MAYBE_EPIPHANY)

noinst_LTLIBRARIES = libdevices.la
libdevices_la_SOURCES = devices.h
libdevices_la_LIBADD = $(MAYBE_LIB_EPIPHANY)
