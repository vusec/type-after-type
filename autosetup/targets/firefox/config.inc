pathobj="$PATHFIREFOX/obj-MetAlloc-$instancename"
pathbinwrap="$pathobj/binwrap"
run mkdir -p "$pathbinwrap"

buildffclangwrapper()
{
	echo "#!/bin/bash"
	echo "set -e"
	echo "XARGS=\"$ldflagsalways $ldflagsnolib -lunwind -lstdc++ -lm\""
	echo "for arg in \"\$@\"; do"
	echo "  case \"\$arg\" in"
	echo "  -c|-E|-V)"
	echo "    XARGS=\"\""
	echo "    ;;"
	echo "  -shared)"
	echo "    XARGS=\"$ldflagsalways $ldflagslib\""
	echo "    ;;"
	echo "  esac"
	echo "done"
	echo "$PATHAUTOPREFIX/bin/$1 \"\$@\" \$XARGS"
}
buildffclangwrapper clang > "$pathbinwrap/clang"
run chmod u+x "$pathbinwrap/clang"
buildffclangwrapper clang++ > "$pathbinwrap/clang++"
run chmod u+x "$pathbinwrap/clang++"

buildbinutilswrapper()
{
	echo "#!/bin/bash"
	echo "set -e"
	echo "$PATHAUTOPREFIX/bin/$1 \"\$@\" --plugin $PATHAUTOPREFIX/lib/LLVMgold.so"
}
buildbinutilswrapper ar > "$pathbinwrap/ar"
run chmod u+x "$pathbinwrap/ar"
buildbinutilswrapper nm > "$pathbinwrap/nm"
run chmod u+x "$pathbinwrap/nm"
buildbinutilswrapper ranlib > "$pathbinwrap/ranlib"
run chmod u+x "$pathbinwrap/ranlib"

(
	[ "$blacklist" = "" ] || cat "$blacklist"
) > "$PATHFIREFOX/.blacklist-MetAlloc-$instancename"
cflagsff="$cflags -fsanitize-blacklist=$PATHFIREFOX/.blacklist-MetAlloc-$instancename"

configpath="$PATHFIREFOX/.mozconfig-MetAlloc-$instancename"
(
	echo "export CC=\"$pathbinwrap/clang $cflags\""
	echo "export CXX=\"$pathbinwrap/clang++ $cflags\""
	echo "export CFLAGS=\"$cflagsff\""
	echo "export CXXFLAGS=\"$cflagsff\""
	echo "export LDFLAGS=\"$cflagsff\""
	echo "ac_add_options --disable-jemalloc"
	echo "ac_add_options --disable-crashreporter"
	echo "ac_add_options --disable-elf-hack"
	echo "ac_add_options --disable-debug-symbols"
	echo "ac_add_options --disable-install-strip"
	echo "ac_add_options --disable-debug"
	echo "ac_add_options --disable-tests"
	echo "ac_add_options --enable-llvm-hacks"
	echo "mk_add_options MOZ_OBJDIR=\"$pathobj\""
	echo "mk_add_options MOZ_MAKE_FLAGS=\"-j$JOBS\""
) > "$configpath"
(
	runscript_common_start
	echo "export PATH=\"$pathbinwrap:$PATHAUTOPREFIX/bin:\$PATH\""
	echo "export MOZCONFIG=\"$configpath\""
	echo "cd \"$PATHFIREFOX\""
	echo "make -k -f client.mk || true # somehow the first build fails sometimes, maybe a missing dependency in their build system?"
	echo "make -f client.mk"
	if [ ${#post_build_hooks[@]} -gt 0 ]; then
		echo "cd \"$pathobj/dist/bin\""
		for hook in ${post_build_hooks[@]}; do
			echo "$hook \"$pathobj/dist/bin/firefox\""
		done
	fi
	runscript_common_end
) > "$PATHAUTOSCRIPTSBUILD/build-firefox-$instancename.sh"
run chmod u+x "$PATHAUTOSCRIPTSBUILD/build-firefox-$instancename.sh"

(
	runscript_common_start
	echo "cd \"$pathobj/dist/bin\""
	echo "$run_wrapper ./firefox \"\$@\""
	runscript_common_end
) > "$PATHAUTOSCRIPTSRUN/run-firefox-$instancename.sh"
run chmod u+x "$PATHAUTOSCRIPTSRUN/run-firefox-$instancename.sh"
