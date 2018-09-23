source "$PATHROOT/autosetup/targets/spec-cpu2006/benchmarks.inc" # BENCHMARKS_SPEC

: ${BUILD_FIREFOX=1}
: ${BUILD_SPEC=1}
TARGETSDEFAULT=""
[ "$BUILD_FIREFOX" -ne 0 ] && TARGETSDEFAULT="$TARGETSDEFAULT firefox"
[ "$BUILD_SPEC"    -ne 0 ] && TARGETSDEFAULT="$TARGETSDEFAULT spec-cpu2006"

: ${INSTANCES=baseline-lto tat-stack tat-heap tat-heap-inline tat-both tat-both-inline}
: ${INSTANCESUFFIX=}
: ${JOBSMAX=16}
: ${TARGETS="$TARGETSDEFAULT"}

targetsupported()
{
	instance="$1"
	target="$2"
	if [ "$target" = firefox ]; then
		case "$instance" in
		*-inline)
			return 1
			;;
		esac
	fi
	return 0
}

