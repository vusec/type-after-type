echo "downloading Firefox"
cd "$PATHAUTOTARGETSRC"
[ -f "firefox-$VERSIONFIREFOX.source.tar.xz" ] || run wget "https://archive.mozilla.org/pub/firefox/releases/$VERSIONFIREFOX/source/firefox-$VERSIONFIREFOX.source.tar.xz"
[ -d "firefox-$VERSIONFIREFOX" ] || run tar xf "firefox-$VERSIONFIREFOX.source.tar.xz"
PATHFIREFOX="$PATHAUTOTARGETSRC/firefox-$VERSIONFIREFOX"
cd "$PATHFIREFOX"
if [ ! -f .autosetup.patched-NS_InvokeByIndex ]; then
	run patch -p0 < "$PATHROOT/patches/FIREFOX_NS_InvokeByIndex.patch"
	touch .autosetup.patched-NS_InvokeByIndex
fi
if [ ! -f .autosetup.patched-DisableStackRecurseCheck ]; then
	run patch -p1 < "$PATHROOT/patches/FIREFOX_DisableStackRecurseCheck.patch"
	touch .autosetup.patched-DisableStackRecurseCheck
fi
