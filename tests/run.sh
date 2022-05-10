#!/bin/sh
clean() {
	echo "killing $SRV_PID and $SSH_AGENT_PID, removing $tmpd" >&2
	[ -z "$SRV_PID" ] || kill -9 "$SRV_PID";
	[ -z "$SSH_AGENT_PID" ] || kill "$SSH_AGENT_PID";
	! [ -d "$tmpd" ] || rm -rf "$tmpd";
}

unset SSH_AGENT_PID SRV_PID tmpd

trap clean EXIT

export LANG=C
export USER=git
export PORT=22222
export HOST=127.199.23.92

PATH=$PATH:$PWD/tests/utils

tmpd="$(mktemp -d /tmp/srcd-test-XXXXX)"

_build/default/rel/srcd/bin/srcd foreground 2>&1 >$tmpd/app.log &
SRV_PID=$!

ssh-keygen -t rsa -f "$tmpd/key" -N "" >/dev/null

eval `ssh-agent`

ssh-add "$tmpd/key"

sleep_t=1
retries=7

while :; do
	output=$(ssh -o StrictHostKeyChecking=accept-new \
	             -o UserKnownHostsFile=/dev/null \
	             -p $PORT $USER@$HOST true 2>&1 | tr -d \\r)
	# Why do we mix stdout and stderr?
	# On stdout we expect an application level error ("invalid command")
	# On stderr we expect ssh level errors (econrefused)
	# If neither of them exist in the mixed stream, we bail.
	case "$output" in
		*"**Error** invalid command"*) break ;;
		"ssh: connect to host $HOST port $PORT: Connection refused")
			echo "sshd not ready yet, retrying in ${sleep_t}s" >&2
			sleep $sleep_t;
			sleep_t=$(($sleep_t * 2))
			[ "$retries" -le 0 ] || continue
			retries=$((retries-1))
			echo "econnrefused, tried ${max_tries} times; bail" >&2
			cat $tmpd/app.log >&2
			echo "<<<$output>>>" >&2
			exit 1 ;;
		*)
			echo "unexpected response from ssh, bailing" >&2
			cat $tmpd/app.log >&2
			echo "<<<$output>>>" >&2
			exit 1
	esac
done

export SRCDIR=$PWD
export GIT_SSH_COMMAND="ssh -o StrictHostKeyChecking=accept-new -o UserKnownHostsFile=/dev/null"
cd "$tmpd"
export GIT_TRACE=$PWD/git.log
export GIT_TRACE_PACKET=$PWD/git.packet.log
if ! prove -e "$SRCDIR/tests/tap.sh" -v "$SRCDIR/tests/proto/"*/*.t; then
	echo "git trace:"
	cat git.log
	echo
	echo "git packet trace:"
	cat git.packet.log
	echo
	echo "application log:"
	cat app.log
	echo
	echo "tests failed"
	exit 1
fi
