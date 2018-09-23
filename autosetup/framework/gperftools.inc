if [ default != "$CONFIG_MALLOC" ]; then
	echo "downloading gperftools-$instance"
	cd "$PATHAUTOFRAMEWORKSRC"
	if [ ! -d "gperftools-$instance/.git" ]; then
		run git clone https://github.com/gperftools/gperftools.git "gperftools-$instance"
		cd "gperftools-$instance"
		run git checkout "$VERSIONGPERFTOOLS"
	fi
	cd "$PATHAUTOFRAMEWORKSRC/gperftools-$instance"
	if [ ! -f .autosetup.patched-gperftools-speedup ]; then
		run patch -p1 < "$PATHROOT/patches/GPERFTOOLS_SPEEDUP.patch"
		touch .autosetup.patched-gperftools-speedup
	fi
fi

case "$CONFIG_MALLOC" in
default)
	# no gperftools
	;;
tcmalloc)
	# standard tcmalloc
	echo "preparing gperftools-$instance"
	[ -f configure ] || run autoreconf -fi
	;;
tcmalloc-typed)
	# modified tcmalloc
	echo "preparing gperftools-$instance"
	cd "$PATHAUTOFRAMEWORKSRC/gperftools-$instance"
	if [ ! -f .autosetup.patched-gperftools-typed ]; then
		run patch -p1 < "$PATHROOT/patches/GPERFTOOLS_typed-2.5.patch"
		touch .autosetup.patched-gperftools-typed
	fi
	[ -f configure ] || run autoreconf -fi
	;;
*)
	echo "error: pass $instance does not define CONFIG_MALLOC" >&2
	exit 1
	;;
esac

if [ default != "$CONFIG_MALLOC" ]; then
	echo "building gperftools-$instance"
	run mkdir -p "$PATHAUTOFRAMEWORKOBJ/gperftools-$instance"
	cd "$PATHAUTOFRAMEWORKOBJ/gperftools-$instance"
	[ -f Makefile ] || run "$PATHAUTOFRAMEWORKSRC/gperftools-$instance/configure" --prefix="$PATHAUTOPREFIXBASE/$instance"
	run make -j"$JOBS"
	run make install
fi
