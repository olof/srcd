#!/bin/sh
N=0
test_name=${1##*/}
test_dir=${1%/*}
test_protocol_v=${test_dir##*/v}
test_protocol_v=${test_protocol_v:-2}

git() {
#	printf "executing: %s -c protocol.version=%s %s" >&2 \
#		"${GIT_CMD:-git}" "$test_protocol_v" "$*"
	command ${GIT_CMD:-git} \
		-c protocol.version="$test_protocol_v" \
		"$@"
}

_tap() {
	local status="$1" msg="$2"
	N=$((N+1))
	echo "$status $N ${msg:+- $msg}"
}

plan() {
	echo 1..$1
}

is() {
	[ "$1" = "$2" ]
	case $? in
		0) _tap ok "$3" ;;
		*) echo "Got:      <<<$1>>>"
		   echo "Expected: <<<$2>>>"
		   _tap 'not ok' "$3"
	esac
}

dir_exists() {
	if [ -d "$1" ]; then
		_tap ok "$2"
	else
		_tap 'not ok' "$2"
	fi
}

tmpd="${test_name}_proto_v${test_protocol_v}"
mkdir "$tmpd" && cd "$tmpd" && . "$1"
