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
                 benchmark/bench-all.sh])

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
AM_CONDITIONAL([ENABLE_DEV_EPIPHANY_SIM],
               [test "x$enable_device_epiphany_sim" = "xyes"])

AS_IF([test "x$enable_device_epiphany_sim" = "xyes"],
      enable_device_epiphany="yes"
      AC_DEFINE([ENABLE_DEV_EPIPHANY_SIM],
                [1],
                [Epiphany device simulator support]))

# Epiphany device support
AC_ARG_ENABLE([device_epiphany],
              AS_HELP_STRING([--enable-device-epiphany],
                             [Enable Epiphany device]))
AM_CONDITIONAL([ENABLE_DEV_EPIPHANY],
               [test "x$enable_device_epiphany" = "xyes"])


# Same as AX_APPEND_LINK_FLAGS but stop at first good flag
#[LIBS], 
AC_DEFUN([AX_APPEND_LINK_FLAGS2], [
AX_REQUIRE_DEFINED([AX_CHECK_LINK_FLAG])
AX_REQUIRE_DEFINED([AX_APPEND_FLAG])
for path in $2; do
    AX_CHECK_LINK_FLAG([$1 $path], [
        for lib in $1; do
            AX_APPEND_FLAG([$lib], [m4_default([$3], [LDFLAGS])])
        done
        AX_APPEND_FLAG([$path], [m4_default([$3], [LDFLAGS])])
        break
    ], [continue], [$4])
done
])dnl AX_APPEND_LINK_FLAGS2

# Same as AX_APPEND_COMPILE_FLAGS but stop at first good flag
#   AX_APPEND_COMPILE_FLAGS2([FLAG1 FLAG2 ...], [FLAGS-VARIABLE], [EXTRA-FLAGS] [INPUT])
AC_DEFUN([AX_APPEND_COMPILE_FLAGS2], [
AX_REQUIRE_DEFINED([AX_CHECK_COMPILE_FLAG])
AX_REQUIRE_DEFINED([AX_APPEND_FLAG])
for flag in $1; do
    AX_CHECK_COMPILE_FLAG([$flag], [AX_APPEND_FLAG([$flag], [$2]) break; ], [], [$3], [$4])
done
])dnl AX_APPEND_COMPILE_FLAGS2

# Ensure Epiphany toolchain, libraries, and headers are present.
# Dependency on Epiphany libs/headers will go away later, this will do for now.
AS_IF([test "x$enable_device_epiphany" = "xyes"],
      [AC_MSG_NOTICE([checking for Epiphany toolchain...])
       AC_CHECK_PROG(have_epiphany_ar,      epiphany-elf-ar,      yes)
       AC_CHECK_PROG(have_epiphany_cc,      epiphany-elf-gcc,     yes)
       AC_CHECK_PROG(have_epiphany_cxx,     epiphany-elf-g++,     yes)
       AC_CHECK_PROG(have_epiphany_ld,      epiphany-elf-ld,      yes)
       AC_CHECK_PROG(have_epiphany_nm,      epiphany-elf-nm,      yes)
       AC_CHECK_PROG(have_epiphany_objdump, epiphany-elf-objdump, yes)
       AC_CHECK_PROG(have_epiphany_ranlib,  epiphany-elf-ranlib,  yes)
       AC_CHECK_PROG(have_epiphany_strip,   epiphany-elf-strip,   yes)

       AS_IF([test "x$have_epiphany_ar"      = "xyes" -a \
                   "x$have_epiphany_cc"      = "xyes" -a \
                   "x$have_epiphany_cxx"     = "xyes" -a \
                   "x$have_epiphany_ld"      = "xyes" -a \
                   "x$have_epiphany_nm"      = "xyes" -a \
                   "x$have_epiphany_objdump" = "xyes" -a \
                   "x$have_epiphany_ranlib"  = "xyes" -a \
                   "x$have_epiphany_strip"   = "xyes"],
             AC_SUBST([EPIPHANY_AR],      [epiphany-elf-ar])
             AC_SUBST([EPIPHANY_CC],      [epiphany-elf-gcc])
             AC_SUBST([EPIPHANY_CPP],     ["epiphany-elf-gcc -E"])
             AC_SUBST([EPIPHANY_CXX],     [epiphany-elf-g++])
             AC_SUBST([EPIPHANY_CXXCPP],  ["epiphany-elf-g++ -E"])
             AC_SUBST([EPIPHANY_LD],      [epiphany-elf-ld])
             AC_SUBST([EPIPHANY_NM],      ["epiphany-elf-nm -B"])
             AC_SUBST([EPIPHANY_OBJDUMP], [epiphany-elf-objdump])
             AC_SUBST([EPIPHANY_RANLIB],  [epiphany-elf-ranlib])
             AC_SUBST([EPIPHANY_STRIP],   [epiphany-elf-strip]),
             [AC_MSG_ERROR([Epiphany toolchain not found])])
       AC_MSG_NOTICE([Epiphany toolchain found])


#       AC_MSG_NOTICE([checking for Epiphany libraries...])
#       lib_help_str="You need to pass in:
#./configure LDFLAGS\"-L/path/to/adapteva/esdk/tools/host.armv7l/lib\" ..."
#
#       AX_APPEND_LINK_FLAGS2([-le-hal -le-loader],
#                             [-L$EPIPHANY_HOME/tools/host.armv7l/lib \
#                              -L/opt/adapteva/esdk/tools/host.armv7l/lib])
#
#       AC_CHECK_LIB([e-hal], e_init, [],
#                    [AC_MSG_ERROR([$lib_help_str])])
#
#       AC_CHECK_LIB([e-loader], e_load_group, [],
#                    [AC_MSG_ERROR([$lib_help_str])])
#       AC_MSG_NOTICE([Epiphany libraries found])
#
#       AC_MSG_NOTICE([checking for Epiphany headers...])
#       inc_help_str="You need to pass in:
#./configure CPPFLAGS\"-I/path/to/adapteva/esdk/tools/host.armv7l/include\" ..."
#
#       AX_APPEND_COMPILE_FLAGS2(["" \
#                                 "-I$EPIPHANY_HOME/tools/host.armv7l/include" \
#                                 "-I/opt/adapteva/esdk/tools/host.armv7l/include"],
#                               [], [],
#                               [AC_LANG_SOURCE([[#include <e-hal.h>]],
#                                               [[#include <e-loader.h>]])])
#
#       AC_CHECK_HEADER([e-hal.h], [], [AC_MSG_ERROR([$inc_help_str])])
#       AC_CHECK_HEADER([e-loader.h], [], [AC_MSG_ERROR([$inc_help_str])])
#
#       AC_MSG_NOTICE([Epiphany headers found])

       AC_DEFINE([ENABLE_DEV_EPIPHANY], [1], [Epiphany device support])

       ])

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
