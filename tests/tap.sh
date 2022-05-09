#!/bin/sh
N=0

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

. "$1"
