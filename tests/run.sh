#!/bin/sh
# Set up things in the environment that survives throughout all the suites:
# ssh keys, git setup, stuff like that.
set -e

clean() {
	! [ -d "$tmpd" ] || rm -rf "$tmpd";
}

unset tmpd
export LANG=C.UTF-8
PATH=$PATH:$PWD/tests/utils

pwd

trap clean EXIT
tmpd="$(mktemp -d /tmp/srcd-test-XXXXX)"
mkdir -p "$tmpd/ssh/user" "$tmpd/ssh/host" "$tmpd/empty"

export SSH_USER_KEY_DIR=$tmpd/ssh/user
export SSH_USER_KEY=$SSH_USER_KEY_DIR/id_rsa
export SSH_USER_KNOWN_HOSTS=$SSH_USER_KEY_DIR/known_hosts
export SSH_HOST_KEY_DIR=$tmpd/ssh/host
export SSH_HOST_KEY=$SSH_HOST_KEY_DIR/ssh_host_rsa_key

ssh-keygen -t rsa -f "$SSH_USER_KEY" -N "" >/dev/null
ssh-keygen -t rsa -f "$SSH_HOST_KEY" -N "" >/dev/null

git config --global init.templateDir "$tmpd/empty"

export SRCDIR=$PWD
export TESTDIR=$PWD/tests
ssh=ssh
ssh="$ssh -o IdentityFile=$SSH_USER_KEY"
ssh="$ssh -o StrictHostKeyChecking=accept-new"
ssh="$ssh -o UserKnownHostsFile=$SSH_USER_KNOWN_HOSTS"
export GIT_SSH_COMMAND="$ssh"

cd "$tmpd"

prove -e "$SRCDIR"/tests/tap.sh -v "$SRCDIR"/tests/*.t
