#!/bin/sh
N=0
TAP_STATUS=0
test_name=${1##*/}
test_dir=${1%/*}

export LANG=C
export REMOTE_USER=git
export PORT=22222
export HOST=127.199.23.92
export BASE_URL=ssh://$REMOTE_USER@$HOST:$PORT

export GIT_AUTHOR_EMAIL='srcd@example.invalid'
export GIT_AUTHOR_NAME='Sauce Daemon'
export GIT_AUTHOR_DATE=1654519284
export GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"
export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME"
export GIT_COMMITTER_DATE="$GIT_AUTHOR_DATE"
export GIT_TRACE=$PWD/git.log
export GIT_TRACE_PACKET=$PWD/git.packet.log

git() {
#	printf "executing: %s -c protocol.version=%s %s" >&2 \
#		"${GIT_CMD:-git}" "$test_protocol_v" "$*"
	command ${GIT_CMD:-git} \
		${PROTO_VERSION:+-c protocol.version="$PROTO_VERSION"} \
		"$@"
}

die() {
	EXIT_REASON="$*"
	exit 1
}

_dump_log() {
	[ "$NO_DUMP_LOGS" ] && return
	cmd=${3:-cat}
	[ -e "$2" ] && {
		printf "%s:\n" "$1"
		$cmd "$2"
		echo
	}
}

_cleanup() {
	status=$?

	[ "$SRV_PID" ] && [ -d "/proc/$SRV_PID" ] &&
		{ echo "#" killing sshd @ $SRV_PID >&2; kill -9 "$SRV_PID"; }
	[ "$SSH_AGENT_PID" ] && [ -d "/proc/$SSH_AGENT_PID" ] &&
		kill "$SSH_AGENT_PID";

	if [ $status -ne 0 ] || [ "$TAP_STATUS" -ne 0 ]; then
		_dump_log 'git trace' git.log
		_dump_log 'git packet trace' git.packet.log
		_dump_log 'application log' app.log
		_dump_log 'git packet file' git.pack hd
		[ -z "$EXIT_REASON" ] || echo "ERROR: $EXIT_REASON" >&2
	fi

	[ -d "$test_dir" ] && rm -rf "$test_dir"
}

_tap() {
	local status="$1" msg="$2"
	case $status in
		'ok') ;;
		*) TAP_STATUS=1 ;;
	esac
	N=$((N+1))
	echo "$status $N ${msg:+- $msg}"
}

_ok_ret() {
	case $? in
		0) _tap ok "$3" ;;
		*) echo "Got:      $1"
		   echo "Expected: $2"
		   _tap 'not ok' "$3"
	esac
}

_wait_until_started() {
	local pid="$1" host="$2" port="$3" user="$4"
	local sleep_t=1 retries=7

	# Exec the true command on remote host. It should not succeed, as we do
	# not provide a "true" command, but we should at least get an expected
	# error message. If we get econnrefused, we try again, with an
	# exponential backoff.
	#
	# We mix stdout and stderr of the test commands as we have output on
	# stderr if we have errors from ssh (e.g. connection refused). On stdout
	# we expected errors from the application ("invalid command" is what we
	# want). If we get anything else, we bail. (If we get an empty response,
	# that means that we successfully ran true. But we don't provide that,
	# so that likely means that we connected to something that isn't us.)
	while :; do
		output=$(ssh -o StrictHostKeyChecking=accept-new \
			     -o UserKnownHostsFile=/dev/null \
			     -o IdentityFile="$SSH_USER_KEY" \
			     -p "$port" "$user@$host" true 2>&1 | tr -d \\r)

		case "$output" in
			*"**Error** invalid command"*) break ;;
			"ssh: connect to host $host port $port: Connection refused")
				[ -d "/proc/$pid" ] || die "srcd has died"

				[ "$retries" -gt 0 ] ||
					die "still refused ($max_tries tries)"

				echo "# sshd not ready, retry in ${sleep_t}s" >&2
				sleep $sleep_t;
				sleep_t=$(($sleep_t * 2))
				retries=$((retries-1))
				;;
			[])
				die "unexpected ssh success; wrong remote?"
				;;
			*)
				die "unexpected ssh response: <<<$output>>>"
		esac
	done
}

start() {
	$SRCDIR/_build/test/rel/srcd/bin/srcd foreground \
		2>&1 >$test_dir/app.log &

	SRV_PID=$!
	echo "# server started, pid $SRV_PID" >&2
	_wait_until_started "$SRV_PID" "$HOST" "$PORT" "$REMOTE_USER"
}

plan() {
	echo 1..$1
}

description() {
	cat >/dev/null
}

fixture() {
	echo "# Loading fixtures '$1'"
	cp "$TESTDIR/fixtures/$1/"* "$SRCD_REPO_DIR/"
}

is() {
	[ "$1" = "$2" ]
	_ok_ret "<<<$1>>>" "<<<$2>>>" "$3"
}

isnt() {
	[ "$1" != "$2" ]
	_ok_ret "$1" "not <<<$2>>>" "$3"
}

dir_exists() {
	if [ -d "$1" ]; then
		_tap ok "$2"
	else
		_tap 'not ok' "$2"
	fi
}

contains() {
	local haystack="$1" needle="$2"
	case "$1" in
		*"$2"*) true ;;
		*) false ;;
	esac
	_ok_ret "<<<$1>>>" "something that contains <<<$2>>>" "$3"
}

test_dir=$(mktemp -d "/tmp/srcd-test-${test_name}-XXXXXX")
trap _cleanup EXIT
cd "$test_dir" || exit 1

mkdir -p repos

export SRCD_REPO_DIR=$test_dir/repos
export GIT_TRACE=$test_dir/git.log
export GIT_TRACE_PACKET=$test_dir/git.packet.log
export GIT_TRACE_PACKFILE=$test_dir/git.pack

. "$1"
