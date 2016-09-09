ACLOCAL_AMFLAGS = -I config/m4
AUTOMAKE_OPTIONS = foreign

host_abs_top_builddir = $(abs_top_builddir)/../..

if HOST_IS_EPIPHANY
LOG_DRIVER = $(SHELL) $(host_abs_top_builddir)/config/test-driver
TEST_LOG_DRIVER = $(SHELL) $(host_abs_top_builddir)/config/test-driver
else
LOG_DRIVER = $(SHELL) config/test-driver
TEST_LOG_DRIVER = $(SHELL) config/test-driver
endif

EXTRA_DIST                        =
SUBDIRS                           =
DIST_SUBDIRS                      =
CLEANFILES                        =
BUILT_SOURCES                     =
lib_LTLIBRARIES                   =
bin_PROGRAMS                      =
include_HEADERS                   =
noinst_HEADERS                    =
noinst_LTLIBRARIES                =
noinst_PROGRAMS                   =
check_PROGRAMS                    =
SUFFIXES                          =

# Automake hooks / local rules
CLEAN_LOCAL_TARGETS               =
INSTALL_HTML_LOCAL_TARGETS        =
UNINSTALL_LOCAL_TARGETS           =
INSTALL_DATA_LOCAL_TARGETS        =
ALL_LOCAL_TARGETS                 =
CHECK_TARGETS                     =
clean-local:        $(CLEAN_LOCAL_TARGETS)
install-html-local: $(INSTALL_HTML_LOCAL_TARGETS)
uninstall-local:    $(UNINSTALL_LOCAL_TARGETS)
install-data-local: $(INSTALL_DATA_LOCAL_TARGETS)
all-local:          $(ALL_LOCAL_TARGETS)
check:              $(CHECK_TARGETS)

# All hooks / local targets are phonies?
PHONIES =                           \
$(CLEAN_LOCAL_TARGETS)              \
$(INSTALL_HTML_LOCAL_TARGETS)       \
$(UNINSTALL_LOCAL_TARGETS)          \
$(INSTALL_DATA_LOCAL_TARGETS)       \
$(ALL_LOCAL_TARGETS)


.PHONY: $(PHONIES)

# DEVICE_RULES: Programs and C sources with dev-XXXX suffix will be built
# with device tools and flags.
DEVICE_OVERRIDE_FLAGS := CC LD CFLAGS CPPFLAGS AM_CFLAGS AM_CPPFLAGS LDFLAGS LDADD AM_LDFLAGS LIBADD LIBS
define DEVICE_RULES_
%-dev-$(1)$(EXEEXT): $(3)=$$($(2)_$(3))
%-dev-$(1).$(OBJEXT): $(3)=$$($(2)_$(3))
endef
define DEVICE_RULES
$(foreach flag,$(DEVICE_OVERRIDE_FLAGS),$(eval $(call DEVICE_RULES_,$(1),$(2),$(flag))))
# Copy from generic dev file if not exists
%-dev-$(1).c: %-dev.c
	cp $$^ $$@
endef
$(eval $(call DEVICE_RULES,epiphany,EPIPHANY))
EPIPHANY_CPPFLAGS:=$(CPPFLAGS)
EPIPHANY_AM_CPPFLAGS:=$(AM_CPPFLAGS)
EPIPHANY_LDADD = devices/epiphany/src/libpal.la


# TODO: Move to configure.ac / become independent
if HOST_IS_EPIPHANY
ESDK              = /opt/adapteva/esdk
EINCS             = -I$(ESDK)/tools/host/include
ELIBS             = -L$(ESDK)/tools/host/lib
CFLAGS_FOR_BUILD  += -DPAL_SOURCE -I$(top_srcdir)/include $(EINCS)
LDFLAGS_FOR_BUILD += -L$(host_abs_top_builddir)/src/.libs \
                     -Wl,-rpath=$(host_abs_top_builddir)/src/.libs -lpal
endif

if BUILD_DEV_EPIPHANY
SUBDIRS += devices/epiphany
-include devices/epiphany/Makemodule.host.am
endif

# Top level targets

EXTRA_DIST += config include bootstrap CHANGELOG.md CODING-STYLE.md \
              CONTRIBUTING.md LICENSE QUESTIONS.md README.md scripts

EXTRA_DIST += devices/epiphany/placeholder.txt

# Reports
PHONIES += reports report-code-size report-stack-usage
report-code-size: all
	@mkdir -p reports
	$(SHELL) $(top_srcdir)/scripts/report-code-size.sh src \
	    > reports/code-size.txt
	@echo Code size report saved to reports/code-size.txt

report-stack-usage: all
	@mkdir -p reports
	$(SHELL) $(top_srcdir)/scripts/report-stack-usage.sh src \
	    > reports/stack-usage.txt
	@echo Stack usage report saved to reports/stack-usage.txt

reports: report-code-size report-stack-usage

clean-local-top:
	-rm -rf reports
	-find . -type f -name "*.su" -delete

CLEAN_LOCAL_TARGETS += clean-local-top


include include/Makemodule.am
include src/Makemodule.am
if ENABLE_DOC
include doc/Makemodule.am
endif
if ENABLE_EXAMPLES
include examples/Makemodule.am
endif
if ENABLE_BENCHMARK
include benchmark/Makemodule.am
endif
if ENABLE_TESTS
include tests/Makemodule.am
endif
