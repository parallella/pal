dnl This is the messy part. We need to set the host to epiphany-elf and adjust
dnl the prefix for e-lib.
dnl Requires AX_CONFIG_DIR
dnl
dnl Arg 1 subdir
dnl Arg 2 canonical

AC_DEFUN([PAL_CONFIG_DEVICE], [

pal_save_prefix=$prefix
pal_save_exec_prefix=$exec_prefix
pal_save_ac_configure_args=$ac_configure_args
pal_save_CFLAGS=$CFLAGS
pal_save_CPPFLAGS=$CPPFLAGS
pal_save_CXXFLAGS=$CXXFLAGS
pal_save_LDFLAGS=$LDFLAGS

host_ac_configure_args=$ac_configure_args
ac_configure_args=
CFLAGS=${CFLAGS_FOR_TARGET:-"-g -O2"}
CXXFLAGS=${CXXFLAGS_FOR_TARGET:-"-g -O2"}
CPPFLAGS=$CPPFLAGS_FOR_TARGET
LDFLAGS=$LDFLAGS_FOR_TARGET

# Whitelist arguments
for host_arg in $host_ac_configure_args
do
AS_CASE([$host_arg],
        [*--enable-assert*], [ac_configure_args="$ac_configure_args $host_arg"],
        [*--disable-assert*],[ac_configure_args="$ac_configure_args $host_arg"],
        [])
done

# Disable everything but lib
ac_configure_args="--disable-tests --disable-benchmark --disable-examples --disable-doc $ac_configure_args"

# Insert target as host
ac_configure_args="--host=$2 $ac_configure_args"

# Expand default prefix NOW.
test "x$prefix" = xNONE && host_prefix=$ac_default_prefix || host_prefix=$prefix
test "x$target_prefix" = x && target_prefix="${host_prefix}/$2"
prefix=$target_prefix
exec_prefix=$prefix

# Insert target prefix
ac_configure_args="$ac_configure_args --prefix=${target_prefix}"

# Custom command that configures subdir NOW, so we can restore variables after.
AX_CONFIG_DIR($1, $srcdir)

# Restore variables
prefix=$pal_save_prefix
exec_prefix=$pal_save_exec_prefix
ac_configure_args=$pal_save_ac_configure_args
CFLAGS=$pal_save_CFLAGS
CPPFLAGS=$pal_save_CPPFLAGS
CXXFLAGS=$pal_save_CXXFLAGS
LDFLAGS=$pal_save_LDFLAGS

])
