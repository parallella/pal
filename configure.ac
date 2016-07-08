#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.



# Needed for autoconf 2.64
m4_pattern_allow([AC_CHECK_HEADER_STDBOOL])

AC_PREREQ([2.64])
AC_INIT([Parallel Architectures Library], [0.3],
        [https://github.com/parallella/pal/issues], [pal],
        [https://github.com/parallella/pal])

AC_CONFIG_SRCDIR([src/base/p_init.c])
AC_CONFIG_HEADERS([config.h])

AC_CONFIG_AUX_DIR([config])

AC_CONFIG_MACRO_DIR([config/m4])

# Support cross compilation
AC_CANONICAL_BUILD
AC_CANONICAL_HOST

AM_INIT_AUTOMAKE([subdir-objects -Wall -Werror])

# Support silent build rules. Disable by either passing --disable-silent-rules
# to configure or passing V=1 to make.
AM_SILENT_RULES([yes])



# Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC_C99
AX_PROG_CC_FOR_BUILD # Check for native compiler (cross-compilation)
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AM_PROG_CC_C_O
AM_PROG_AR


AX_CHECK_COMPILE_FLAG([-fstack-usage],
                      [AX_APPEND_FLAG([-fstack-usage])],
                      [AC_MSG_WARN([-fstack-usage not supported, Some reports will be incomplete.])
                      ])

LT_INIT

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h fenv.h float.h inttypes.h limits.h stddef.h stdint.h stdlib.h string.h strings.h sys/ioctl.h time.h unistd.h])
AC_CHECK_HEADER([mach/mach_time.h],
  [AC_DEFINE([HAVE_MACH_TIME], [1],
    [Define to 1 if you have <mach/mach_time.h>])])
# Epiphany headers
AC_CHECK_HEADERS([e-lib.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_FORK
#AC_FUNC_MALLOC # Don't do it. Assume GNU libc compatible malloc.
AC_FUNC_MMAP
AC_CHECK_FUNCS([bzero fesetround floor gettimeofday memset modf munmap pow rint sqrt strdup strerror strtol])

# Older versions of libc need -lrt for clock_gettime()
AC_CHECK_FUNCS(clock_gettime, [], [
  AC_CHECK_LIB(rt, clock_gettime, [
    AC_DEFINE(HAVE_CLOCK_GETTIME, [1], [Have clock_gettime])
    BENCHMARK_LIBS="$BENCHMARK_LIBS -lrt"
  ])
])
AC_SUBST([BENCHMARK_LIBS])

AC_CONFIG_FILES([Makefile
                 benchmark/bench-all.sh
                 Makemodule.host.am])

# Epiphany host support
AM_CONDITIONAL([HOST_IS_EPIPHANY],
               [test "x$host_cpu" = "xepiphany"])

AS_IF([test "x$host_cpu" = "xepiphany"],
      [
        # If linker script is NOT specified in LDFLAGS...
        AS_IF([AS_CASE([$LDFLAGS], [*-T*], [false], [true])],
              [
                # Provide internal/fast so individual programs can override.
                AX_APPEND_LINK_FLAGS([-Wl,-Tinternal.ldf \
                                      -Wl,-T$EPIPHANY_HOME/bsps/current/internal.ldf],
                                      [EPIPHANY_INTERNAL_LDFLAGS])

                AX_APPEND_LINK_FLAGS([-Wl,-Tfast.ldf \
                                      -Wl,-T$EPIPHANY_HOME/bsps/current/fast.ldf],
                                      [EPIPHANY_FAST_LDFLAGS])

                # Default to internal.ldf
                AX_APPEND_LINK_FLAGS([-Wl,-Tinternal.ldf \
                                      -Wl,-T$EPIPHANY_HOME/bsps/current/internal.ldf],
                                      [AM_LDFLAGS])

              ])

        AS_CASE([$LDFLAGS], [*-T*], [ : ],
                [
                  AS_IF([test "x$EPIPHANY_INTERNAL_LDFLAGS" = "x" -o \
                              "x$EPIPHANY_FAST_LDFLAGS" = "x"],
                        [AC_MSG_ERROR([No linker script found. You need to either set EPIPHANY_HOME or provide one/set search path in LDFLAGS])])

                ])
      ])

AC_SUBST([EPIPHANY_INTERNAL_LDFLAGS])
AC_SUBST([EPIPHANY_FAST_LDFLAGS])

# Epiphany simulator support
AC_ARG_ENABLE([device_epiphany_sim],
              AS_HELP_STRING([--enable-device-epiphany-sim],
                             [Enable Epiphany device simulator]))
AS_IF([test "x$enable_device_epiphany_sim" = "xyes"],
      AC_CHECK_HEADERS([esim.h], break;, enable_device_epiphany_sim=no)
      AC_CHECK_LIB(esim, es_init, break;, enable_device_epiphany_sim=no))
AS_IF([test "x$enable_device_epiphany_sim" = "xyes"],
      enable_device_epiphany="yes"
      AC_DEFINE([ENABLE_DEV_EPIPHANY_SIM],
                [1],
                [Epiphany device simulator support]))
AM_CONDITIONAL([ENABLE_DEV_EPIPHANY_SIM],
               [test "x$enable_device_epiphany_sim" = "xyes"])

# Epiphany device support
AC_ARG_ENABLE([device_epiphany],
              AS_HELP_STRING([--enable-device-epiphany],
                             [Enable Epiphany device]))
AM_CONDITIONAL([ENABLE_DEV_EPIPHANY],
               [test "x$enable_device_epiphany" = "xyes"])

AS_IF([test "x$enable_device_epiphany" = "xyes"],
      [PAL_CONFIG_DEVICE([epiphany], [epiphany-elf])])


AC_ARG_ENABLE([benchmark], AS_HELP_STRING([--disable-benchmark], [Disable building benchmark binaries]))
AM_CONDITIONAL([ENABLE_BENCHMARK], [test "x$enable_benchmark" != "xno"])

AC_ARG_ENABLE([examples], AS_HELP_STRING([--disable-examples], [Disable building examples]))
AM_CONDITIONAL([ENABLE_EXAMPLES], [test "x$enable_examples" != "xno"])

AC_ARG_ENABLE([tests],
              AS_HELP_STRING([--disable-tests],
                             [Disable building unit tests]))

# Unit testing
AC_MSG_CHECKING([whether to build unit tests])
AS_IF([test "x${enable_tests}" = "xno"],
      [AC_MSG_RESULT([no])],
      [AC_MSG_RESULT([yes])])
AM_CONDITIONAL(ENABLE_TESTS, [test "x${enable_tests}" != "xno"])

# Documentation
AC_CHECK_PROGS([DOXYGEN], [doxygen])
AS_IF([test "x${DOXYGEN}" = "x"],
      [AC_MSG_WARN([Doxygen not found - continuing without Doxygen support])])

AM_CONDITIONAL([HAVE_DOXYGEN], [test "x${DOXYGEN}" != "x"])
AM_COND_IF([HAVE_DOXYGEN], [AC_CONFIG_FILES([doc/Doxyfile])])

AC_ARG_ENABLE([doc],
              [AS_HELP_STRING([--disable-doc],
                              [Build documentation using doxygen])])
AC_MSG_CHECKING([whether to build documentation])
AS_IF([test "x${enable_doc}" = "xyes"],
      [AS_IF([test "x${DOXYGEN}" = "x"],
             [AC_MSG_RESULT([error])
             AC_MSG_ERROR([Cannot build documentation, doxygen not installed])])])
AS_IF([test "x${enable_doc}" = "x"],
      [AS_IF([test "x${DOXYGEN}" = "x"], [enable_doc="no"])])

AS_IF([test "x${enable_doc}" = "xno"],
      [AC_MSG_RESULT([no])],
      [AC_MSG_RESULT([yes])])

AM_CONDITIONAL(ENABLE_DOC, [test "x${enable_doc}" != "xno"])

# Define the --enable-assert command-line option
AC_ARG_ENABLE(assert,
  [AS_HELP_STRING([--enable-assert], [Build with assertions enabled])],
  [enable_assert=yes], [enable_assert=no])
# Set any CPPFLAGS specific to debugging
AS_IF([test "x$enable_assert" = "xyes"],
  [AC_SUBST(assert_CPPFLAGS, [''])],
  [AC_SUBST(assert_CPPFLAGS, ['-DNDEBUG'])])

# Easiest way to include header files globally
AC_SUBST([AM_CPPFLAGS],
  ['-I$(top_srcdir)/include -I$(top_builddir) $(assert_CPPFLAGS) -DPAL_SOURCE'])

AC_SUBST([AM_LDFLAGS])

AC_OUTPUT
