unset CONFIG_MALLOC

unset CONFIG_STATICLIB_MAKE
CONFIG_STATICLIBS=""

ar="$PATHAUTOPREFIX/bin/llvm-ar"
cc="$PATHAUTOPREFIX/bin/clang"
cxx="$PATHAUTOPREFIX/bin/clang++"
cflags="$EXTRA_CFLAGS"
ldflagsalways="$EXTRA_LDFLAGS"
ldflagschrome="$EXTRA_LDFLAGS"
ldflagsnolib=""
ldflagsnolibchrome=""
ldflagslib=""
ldflagslibchrome=""
nm="$PATHAUTOPREFIX/bin/llvm-nm"
ranlib="$PATHAUTOPREFIX/bin/llvm-ranlib"
blacklist=""
run_wrapper=""
post_build_hooks=()

unset prefix
unset prefixbin
unset prefixlib
