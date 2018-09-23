# helper for pass config files
#
# set a compiler wrapper for clang that performs a source-to-source
# transformation using the specified frontend plugin, saves the transformed
# source to a temporary file, and calls the original compiler on a 
#
# usage: add_clang_plugin plugin-name
set_clang_plugin() {
    cc="$PATHROOT/scripts/clang-plugin-wrapper.sh $cc $PATHCLANGPLUGINS/libplugins.so $1"
    cxx="$PATHROOT/scripts/clang-plugin-wrapper.sh $cxx $PATHCLANGPLUGINS/libplugins.so $1"
}

# usage: set_clang_pass pass-name
set_clang_pass() {
    cc="$PATHROOT/scripts/clang-pass-wrapper.sh $PATHCLANGPLUGINS/libplugins.so $1 $cc --"
    cxx="$PATHROOT/scripts/clang-pass-wrapper.sh $PATHCLANGPLUGINS/libplugins.so $1 $cxx --"
    #cflags="$cflags -Xclang -load -Xclang $PATHCLANGPLUGINS/libplugins.so -mllvm $1 "
}
