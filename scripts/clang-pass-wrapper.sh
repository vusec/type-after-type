#!/usr/bin/env bash
passlib="$1"
passname="$2"
cc="$3"
shift 3
while [ $# -gt 0 ]; do
    if [ "$1" == -- ]; then
        shift
        break
    fi
    cc+=" $1"
    shift
done
exec $cc -Xclang -load -Xclang "$passlib" -mllvm $passname "$@"
