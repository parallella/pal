AUTOMAKE_OPTIONS = foreign

LDADD = -lm $(top_builddir)/src/libpal.la

filters_SOURCES = filters.c stb_image.h stb_image_write.h
filters_CPPFLAGS = $(AM_CPPFLAGS) -DABS_TOP_SRCDIR=\"$(abs_top_srcdir)\"
harris_SOURCES = harris.c
filters2_host_SOURCES = filters2-host.c stb_image.h stb_image_write.h
filters2_dev_sources = filters2-dev.c

if HOST_IS_EPIPHANY
std_programs = harris # filters won't compile due to compiler bug.
harris_LDFLAGS = $(EPIPHANY_FAST_LDFLAGS)
else
std_programs = filters harris
endif

noinst_PROGRAMS = $(std_programs)

all-local:
	$(MKDIR_P) dataset

# TODO: Move to shared rules file
# DEVICE_RULES: Programs and C sources with dev-XXXX suffix will be built
# with device tools and flags.
DEVICE_OVERRIDE_FLAGS := CC LD CFLAGS CPPFLAGS AM_CFLAGS AM_CPPFLAGS LDFLAGS LDADD AM_LDFLAGS LIBADD LIBS
define DEVICE_RULES_
%-dev-$(1)$(EXEEXT): $(3)=$$($(2)_$(3))
%-dev-$(1).$(OBJEXT): $(3)=$$($(2)_$(3))
endef
define DEVICE_RULES
$(foreach flag,$(DEVICE_OVERRIDE_FLAGS),$(eval $(call DEVICE_RULES_,$(1),$(2),$(flag))))
# Symlink to generic dev file if not exists
%-dev-$(1).c: %-dev.c
	$(LN_S) $$^ $$@
endef
$(eval $(call DEVICE_RULES,epiphany,EPIPHANY))

