#!/bin/bash

set -e

: ${PATHROOT:="$PWD"}
if [ ! -f "$PATHROOT/autosetup.sh" ]; then
	echo "Please execute from the root of the repository or set PATHROOT" >&2
	exit 1
fi

source "$PATHROOT/autosetup/config.inc"
source "$PATHROOT/autosetup/paths.inc"

corecount="`grep '^processor' /proc/cpuinfo|wc -l`"
[ "$corecount" -le "$JOBSMAX" ] || corecount="$JOBSMAX"

: ${EXTRA_CFLAGS:=""}
: ${EXTRA_LDFLAGS:=""}
: ${JOBS="$corecount"}
: ${NO_PACKAGES:=0}
: ${NO_PERL:=0}
: ${DISABLE_OPT:=0}

# framework
: ${VERSIONGPERFTOOLS=632de2975e63f89613af9ab99bc1603a4a6332aa}

# packages
: ${VERSIONAUTOCONF=autoconf-2.68}
: ${VERSIONAUTOMAKE=automake-1.15}
: ${VERSIONBASH=bash-4.3}
: ${VERSIONBINUTILS=binutils-2.26.1}
: ${VERSIONCMAKE=cmake-3.8.2}
: ${VERSIONCMAKEURL=v3.8}
: ${VERSIONCOREUTILS=coreutils-8.22}
: ${VERSIONFREETYPE=freetype-2.6.5}
: ${VERSIONLIBGCRYPT=libgcrypt-1.7.0}
: ${VERSIONLIBTOOL=libtool-2.4.6}
: ${VERSIONLIBUNWIND=libunwind-1.2-rc1}
: ${VERSIONLLVM:=RELEASE_400/final}
: ${VERSIONLLVMPATCH:=4.0}
: ${VERSIONM4=m4-1.4.18}
: ${VERSIONMAKE=make-4.1}
: ${VERSIONPERL=perl-5.8.8} # set to 'none' to avoid Perl install
: ${VERSIONPERLURL=5.0}
: ${VERSIONPYTHON=2.7.13}

# targets
: ${VERSIONFIREFOX:=47.0}

PATHBINUTILS="$PATHAUTOPACKSRC/$VERSIONBINUTILS"
PATHLIBUNWIND="$PATHAUTOPACKSRC/$VERSIONLIBUNWIND"

export PATH="$PATHAUTOPREFIX/bin:$PATH"

logdir="$(dirname "$PATHLOG")"
[ -e "$logdir" ] || mkdir -p "$logdir"
exec 5> "$PATHLOG"

run()
{
	echo -------------------------------------------------------------------------------- >&5
	echo "command:          $*"               >&5
	echo "\$PATH:            $PATH"            >&5
	echo "\$LD_LIBRARY_PATH: $LD_LIBRARY_PATH" >&5
	echo "working dir:      $PWD"             >&5
	echo -------------------------------------------------------------------------------- >&5
	success=0
	if [ "$logsuffix" = "" ]; then
		pathlog="$PATHLOG"
		"$@" >&5 2>&5 && success=1
	else
		pathlog="$PATHLOG.$logsuffix.txt"
		echo "logging to $pathlog" >&5
		"$@" > "$pathlog" 2>&1 && success=1
	fi
	if [ "$success" -ne 0 ]; then
		echo "[done]" >&5
	else
		echo "Command '$*' failed in directory $PWD with exit code $?, please check $pathlog for details" >&2
		exit 1
	fi
}

runscript_common_start()
{
	echo "#!/bin/bash"
	echo "set -e"
	echo "export LD_LIBRARY_PATH=\"$prefixlib:$PATHAUTOPREFIX/lib:\$LD_LIBRARY_PATH\""
	echo "export PATH=\"$PATHAUTOPREFIX/bin:\$PATH\""
	echo "export TCMALLOC_LARGE_ALLOC_REPORT_THRESHOLD=281474976710656"
	echo "PATHROOT=\"$PATHROOT\""
	echo ": \${RUNSCRIPTVERBOSE=0}"
	echo ""
	echo "echo \"[autosetup-runscript] target=$target\""
	echo "echo \"[autosetup-runscript] instancename=$instancename\""
	echo "echo \"[autosetup-runscript] cmd=\$*\""
	echo "echo \"[autosetup-runscript] cwd=\`pwd\`\""
	echo "echo \"[autosetup-runscript] LD_LIBRARY_PATH=\$LD_LIBRARY_PATH\""
	echo "echo \"[autosetup-runscript] PATH=\$PATH\""
	echo "echo \"[autosetup-runscript] PATHROOT=$PATHROOT\""
	echo "echo \"[autosetup-runscript] commit=`git log -n1 --oneline`\""
	echo "echo \"[autosetup-runscript] kernel=\`uname -s\`\""
	echo "echo \"[autosetup-runscript] kernel-release=\`uname -r\`\""
	echo "echo \"[autosetup-runscript] kernel-version=\`uname -v\`\""
	echo "echo \"[autosetup-runscript] machine=\`uname -m\`\""
	echo "echo \"[autosetup-runscript] node=\`uname -n\`\""
	echo "if [ \"\$RUNSCRIPTVERBOSE\" -ne 0 ]; then"
	echo "echo \"[autosetup-runscript] meminfo-start\""
	echo "cat /proc/meminfo"
	echo "echo \"[autosetup-runscript] meminfo-end\""
	echo "echo \"[autosetup-runscript] cpuinfo-start\""
	echo "cat /proc/cpuinfo"
	echo "echo \"[autosetup-runscript] cpuinfo-end\""
	echo "fi"
	echo "echo \"[autosetup-runscript] date-start=\`date +%Y-%m-%dT%H:%M:%S\`\""
	echo ""
}

runscript_common_end()
{
	echo ""
	echo "echo \"[autosetup-runscript] date-end=\`date +%Y-%m-%dT%H:%M:%S\`\""
}

echo "Creating directories"
run mkdir -p "$PATHAUTOBENCHSRC"
run mkdir -p "$PATHAUTOFRAMEWORKSRC"
run mkdir -p "$PATHAUTOPACKSRC"
run mkdir -p "$PATHAUTOSCRIPTSBUILD"
run mkdir -p "$PATHAUTOSCRIPTSRUN"
run mkdir -p "$PATHAUTOSTATE"
run mkdir -p "$PATHAUTOTARGETSRC"

export CFLAGS="-I$PATHAUTOPREFIX/include"
export CPPFLAGS="-I$PATHAUTOPREFIX/include"
export LDFLAGS="-L$PATHAUTOPREFIX/lib"

if [ "$NO_PACKAGES" -eq 0 ]; then
	# build bash to override the system's default shell
	source "$PATHROOT/autosetup/packages/bash.inc"

	# build a sane version of coreutils
	source "$PATHROOT/autosetup/packages/coreutils.inc"

	# build binutils to ensure we have gold
	source "$PATHROOT/autosetup/packages/binutils-gold.inc"

	# build make
	source "$PATHROOT/autosetup/packages/make.inc"

	# build m4
	source "$PATHROOT/autosetup/packages/m4.inc"

	# build autoconf
	source "$PATHROOT/autosetup/packages/autoconf.inc"

	# build automake
	source "$PATHROOT/autosetup/packages/automake.inc"

	# build python
	source "$PATHROOT/autosetup/packages/python.inc"

	# build libtool
	source "$PATHROOT/autosetup/packages/libtool.inc"

	# build cmake, needed to build LLVM
	source "$PATHROOT/autosetup/packages/cmake.inc"

	# gperftools requires libunwind
	source "$PATHROOT/autosetup/packages/libunwind.inc"

	# we need a patched LLVM
	source "$PATHROOT/autosetup/packages/llvm.inc"

	if [ "$VERSIONPERL" != none ]; then
		# build Perl, needed for gperftools autoreconf and SPEC CPU2006
		source "$PATHROOT/autosetup/packages/perl.inc"
	fi
fi

for instance in $INSTANCES; do
	if [ ! -f "$PATHROOT/autosetup/passes/$instance.inc" ]; then
		echo "error: unknown pass: $instance" >&2
		exit 1
	fi
	source "$PATHROOT/autosetup/passes/$instance.inc"
	source "$PATHROOT/autosetup/framework/gperftools.inc"
	for lib in $CONFIG_STATICLIBS; do
		echo "building staticlib-$lib-$instance"
		cd "$PATHROOT/staticlib/$lib"
		run make OBJDIR="$PATHAUTOFRAMEWORKOBJ/staticlib-$lib-$instance" $CONFIG_STATICLIB_MAKE -j"$JOBS"
	done
done

echo "building llvm-plugins"
cd "$PATHROOT/llvm-plugins"
run make -j"$JOBS" GOLDINSTDIR="$PATHAUTOPREFIX" TARGETDIR="$PATHLLVMPLUGINS"

echo "building clang-plugins"
cd "$PATHROOT/clang-plugins"
run make -j"$JOBS" TARGETDIR="$PATHCLANGPLUGINS"

echo "initializing targets"
for target in $TARGETS; do
	if [ ! -d "$PATHROOT/autosetup/targets/$target" ]; then
		echo "error: unknown target: $target" >&2
		exit 1
	fi
	if [ -f "$PATHROOT/autosetup/targets/$target/init.inc" ]; then
		source "$PATHROOT/autosetup/targets/$target/init.inc"
	fi
done

echo "building nothp"
cd "$PATHROOT/nothp"
run make

PATHSPECOUT=""
which prun > /dev/null && PATHSPECOUT="/local/$USER/cpu2006-output-root"

# Configure targets
for instance in $INSTANCES; do
	instancename="$instance$INSTANCESUFFIX"
	source "$PATHROOT/autosetup/passes/$instance.inc"

	cflagsbl="$cflags"
	[ "$blacklist" = "" ] || cflagsbl="$cflagsbl -fsanitize-blacklist=$blacklist"

	for target in $TARGETS; do
		targetsupported "$instance" "$target" || continue

		echo "configuring $target-$instancename"

		if [ -f "$PATHROOT/autosetup/targets/$target/config.inc" ]; then
			source "$PATHROOT/autosetup/targets/$target/config.inc"
		fi
	done
done

# Build targets
for instance in $INSTANCES; do
	instancename="$instance$INSTANCESUFFIX"

	for target in $TARGETS; do
		targetsupported "$instance" "$target" || continue

		echo "building $target-$instancename"

		if [ -f "$PATHROOT/autosetup/targets/$target/build.inc" ]; then
			source "$PATHROOT/autosetup/targets/$target/build.inc"
		fi
	done
done

echo done
