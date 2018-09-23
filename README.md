Type-after-Type quick setup guide
=================================

Installation
------------

For this manual, we assume that you are running Ubuntu Server 16.04 LTS.
Other distributions or Ubuntu versions might require different packages
to be able to perform the setup.

We also assume that you have a copy of SPEC CPU2006 installed in $HOME/cpu2006.
If your SPEC install is located somewhere else, adjust the value of PATHSPEC
accordingly. If you do not have a copy of SPEC CPU2006, set the environment
variable BUILD_SPEC to 0. If you want the program to use a new, separate
installation of SPEC CPU2006, set PATHSPECCD to the location of your
SPEC CPU2006 installation CD-ROM instead.

If you want to build Firefox, you will need to install the Firefox build
prerequisites beforehand. Instructions are found here:

https://developer.mozilla.org/en-US/docs/Mozilla/Developer_guide/Build_Instructions/Linux_Prerequisites

If you do not want to build Firefox, set BUILD_FIREFOX=0.

Follow the following instructions:

    sudo apt-get install autoconf bison build-essential libtool libtool-bin
    sudo apt-get install git subversion
    git clone https://github.com/vusec/type-after-type.git
    cd type-after-type
    PATHSPEC="$HOME/cpu2006" ./autosetup.sh

This sets up Type-after-Type and instruments all C/C++ SPEC CPU2006 benchmarks
as well as Firefox using all supported configurations:

* baseline-lto    - default compilation with uninstrumented tcmalloc and LTO
* tat-stack       - Type-after-Type stack protection
* tat-heap        - Type-after-Type heap protection
* tat-heap-inline - Type-after-Type heap protection with wrapper inlining
* tat-both        - Type-after-Type stack+heap protection
* tat-both-inline - Type-after-Type stack+heap protection with wrapper inlining


Firefox
-------

To run Firefox, type:

    ./run-firefox-tat-both.sh

'tat-both' can be replaced with any of the other configurations without inlining.

Please note that Firefox requires a graphical interface, which Ubuntu Server
does not offer by default. Ensure access to an X11 server and export
the correct value of $DISPLAY before running this script.


SPEC CPU2006
------------

To run the microbenchmarks, type:

    ./run-spec-tat-both.sh benchmarks

'tat-both' can be replaced with any of the other configurations.

'benchmarks' can be set to one or more of the following to run a subset
of SPPEC CPU2006:

* 400.perlbench
* 401.bzip2
* 403.gcc
* 429.mcf
* 433.milc
* 444.namd
* 445.gobmk
* 447.dealII
* 450.soplex
* 453.povray
* 456.hmmer
* 458.sjeng
* 462.libquantum
* 464.h264ref
* 470.lbm
* 471.omnetpp
* 473.astar
* 482.sphinx3
* 483.xalancbmk

