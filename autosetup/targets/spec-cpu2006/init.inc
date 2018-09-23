if [ -f "$PATHSPEC/shrc" ]; then
	echo "using existing SPEC CPU2006 installation in $PATHSPEC"
elif [ -f "$PATHSPECCD/shrc" ]
	echo "installing SPEC CPU2006 to $PATHSPEC"
	cd "$PATHSPECCD"
	run ./install.sh -f -d "$PATHSPEC"
else
	echo "error: SPEC CPU2006 not found; please set PATHSPEC or PATHSPECCD, or set BUILD_SPEC=0" >&2
	exit 1
fi

cd "$PATHSPEC"
if [ ! -f .autosetup.patched-dealII-stddef ]; then
	run patch -p0 < "$PATHROOT/patches/SPEC-CPU2006-dealII-stddef.patch"
	touch .autosetup.patched-dealII-stddef
fi

if [ "$VERSIONPERL" != none ]; then
	# install perlbrew (needed by SPEC CPU2006), fixing its installer in the process
	source "$PATHROOT/autosetup/packages/perlbrew.inc"

	# install perl packages
	source "$PATHROOT/autosetup/packages/perl-packages.inc"
fi

