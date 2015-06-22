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
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AM_PROG_AR

AX_CHECK_COMPILE_FLAG([-fstack-usage],
                      [AX_APPEND_FLAG([-fstack-usage])],
                      [AC_MSG_WARN([-fstack-usage not supported, Some reports will be incomplete.])
                      ])

LT_INIT

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h fenv.h float.h inttypes.h limits.h stddef.h stdint.h stdlib.h string.h strings.h sys/ioctl.h sys/time.h unistd.h])

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

AC_CONFIG_FILES([Makefile
                 include/Makefile
                 src/Makefile
                 src/base/Makefile
                 src/base/devices/Makefile
                 src/base/devices/epiphany/Makefile
                 src/dsp/Makefile
                 src/fft/Makefile
                 src/image/Makefile
                 src/math/Makefile
                 examples/Makefile
                 examples/base/Makefile
                 examples/image/Makefile
                 tests/Makefile
                 tests/dsp/Makefile
                 tests/math/Makefile
                 tests/image/Makefile
                 doc/Makefile])

# Epiphany host support
AM_CONDITIONAL([HOST_IS_EPIPHANY],
               [test "x$host_cpu" = "xepiphany"])

# Epiphany device support
AC_ARG_ENABLE([device_epiphany],
              AS_HELP_STRING([--enable-device-epiphany],
                             [Enable Epiphany device]))
AM_CONDITIONAL([ENABLE_DEV_EPIPHANY],
               [test "x$enable_device_epiphany" = "xyes"])
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


       AC_MSG_NOTICE([checking for Epiphany libraries...])
       lib_help_str="You probably need to pass in:
./configure LDFLAGS\"-L/opt/adapteva/esdk/tools/host.armv7l/lib\" ..."
       AC_CHECK_LIB([e-hal], e_init, [],
                    [AC_MSG_ERROR([$lib_help_str])])

       AC_CHECK_LIB([e-loader], e_load_group, [],
                    [AC_MSG_ERROR([$lib_help_str])])
       AC_MSG_NOTICE([Epiphany libraries found])


       AC_MSG_NOTICE([checking for Epiphany headers...])
       inc_help_str="You probably need to pass in:
./configure CPPFLAGS\"-I/opt/adapteva/esdk/tools/host.armv7l/include\" ..."
       AC_CHECK_HEADER([e-hal.h], [], [AC_MSG_ERROR([$inc_help_str])])

       AC_CHECK_HEADER([e-loader.h], [], [AC_MSG_ERROR([$inc_help_str])])
       AC_MSG_NOTICE([Epiphany headers found])


       #AC_SUBST([ENABLE_DEV_EPIPHANY], [yes])
       AC_DEFINE([ENABLE_DEV_EPIPHANY], [1], [Epiphany device support])
       ])

# Unit testing
PKG_CHECK_MODULES(CHECK, [check >= 0.9.4],
                  [have_check="yes"],
                  [have_check="no"])
AM_CONDITIONAL(HAVE_CHECK, [test "x${have_check}" = "xyes"])
AC_SUBST(CHECK_CFLAGS)
AC_SUBST(CHECK_LIBS)
AC_ARG_ENABLE([tests],
              AS_HELP_STRING([--disable-tests],
                             [Disable building unit tests]))

AC_MSG_CHECKING([whether to build unit tests])
AS_IF([test "x${enable_tests}" = "xyes"],
      [AS_IF([test "x${have_check}" != "xyes"],
             [AC_MSG_RESULT([error])
             AC_MSG_ERROR([Cannot build unit tests, check not installed])])])
AS_IF([test "x${enable_tests}" = "x"],
      [AS_IF([test "x${have_check}" != "xyes"], [enable_tests="no"])])

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


# Easiest way to include header files globally
AC_SUBST([AM_CPPFLAGS], ['-I$(top_srcdir)/include -I$(top_builddir)'])

AC_OUTPUT
