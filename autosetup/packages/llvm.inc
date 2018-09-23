echo "building LLVM"
llvmdir="$PATHAUTOPACKSRC/llvm-$VERSIONLLVMPATCH"
[ -d "$llvmdir/.svn" ] || run svn co "http://llvm.org/svn/llvm-project/llvm/tags/$VERSIONLLVM" "$llvmdir"
[ -d "$llvmdir/tools/clang/.svn" ] || run svn co "http://llvm.org/svn/llvm-project/cfe/tags/$VERSIONLLVM" "$llvmdir/tools/clang"
[ -d "$llvmdir/projects/compiler-rt/.svn" ] || run svn co "http://llvm.org/svn/llvm-project/compiler-rt/tags/$VERSIONLLVM" "$llvmdir/projects/compiler-rt"
cd "$llvmdir/projects/compiler-rt"
cd "$llvmdir"
if [ ! -f .autosetup.patched-LLVM-gold-plugins ]; then
	run patch -p0 < "$PATHROOT/patches/LLVM-gold-plugins-$VERSIONLLVMPATCH.diff"
	touch .autosetup.patched-LLVM-gold-plugins
fi
if [ ! -f .autosetup.patched-LLVM-gold-Os ]; then
	run patch -p0 < "$PATHROOT/patches/LLVM-gold-Os-$VERSIONLLVMPATCH.diff"
	touch .autosetup.patched-LLVM-gold-Os
fi
if [ ! -f .autosetup.patched-LLVM-codegen ]; then
	run patch -p0 < "$PATHROOT/patches/LLVM-codegen-$VERSIONLLVMPATCH.diff"
	touch .autosetup.patched-LLVM-codegen
fi
run mkdir -p "$PATHAUTOPACKOBJ/llvm-$VERSIONLLVMPATCH"
cd "$PATHAUTOPACKOBJ/llvm-$VERSIONLLVMPATCH"
[ -f Makefile ] || run cmake -DCMAKE_INSTALL_PREFIX="$PATHAUTOPREFIX" -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_BINUTILS_INCDIR="$PATHBINUTILS/include" "$llvmdir"
run make -j"$JOBS"
run make install
