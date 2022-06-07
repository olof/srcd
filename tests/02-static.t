plan 39

description <<'EOF'
Initialize a default set of repos and perform read only operations,
with the various supported protocol versions.
EOF

fixture default

start

## Tests for non-existing repo

for PROTO_VERSION in 0 1 2; do
	v=$PROTO_VERSION
	output=$(git ls-remote "$BASE_URL/does-not-exist.git" 2>&1)

	isnt $? 0 "ls-remote non-existing repo fails, pack v$v"
	contains "$output" "no_such_repo" \
	         "error from ls-remote mentions reason, pack v$v"

	output=$(git clone "$BASE_URL/does-not-exist.git" test-$v.git 2>&1)

	isnt $? 0 "cloning non-existing repo fails, pack v$v"
	contains "$output" "no_such_repo" \
	         "error from clone mentions reason, pack v$v"
done
PROTO_VERSION=

## Tests for empty repo

for PROTO_VERSION in 0 1 2; do
	v=$PROTO_VERSION
	output=$(git ls-remote "$BASE_URL/empty.git" 2>&1)

	is $? 0 "ls-remote on empty repo succeeds, pack v$v"
	is "$output" "" \
	   "ls-remote output on empty repo is empty, pack v$v"

	output=$(git clone --bare "$BASE_URL/empty.git" empty-$v.git 2>&1)

	is $? 0 "cloning existing repo succeeds, pack v$v"
	contains "$output" "empty repository" \
		"output from git mentions that repo is empty, pack v$v"

	is "$(find empty-$v.git/objects -type f | wc -l)" 0 \
	   "empty.git should contain no objects, pack v$v"
done
PROTO_VERSION=

## Tests for simple non-empty repo

for PROTO_VERSION in 0 1 2; do
	v=$PROTO_VERSION
	output=$(git ls-remote "$BASE_URL/simple.git" 2>&1)

	is $? 0 "ls-remote simple.git succeeds, pack v$v"
	# TODO: Ugly formatting...
	is "$output" \
	"399a62bb6d769a769f5ebc6007c8e0db80a251c4	HEAD
399a62bb6d769a769f5ebc6007c8e0db80a251c4	refs/heads/master" \
		"expected output from ls-remote for simple.git, pack v$v"

	output=$(git clone --bare "$BASE_URL/simple.git" simple-$v.git 2>&1)
	r=$?
	is $r 0 "cloning simple.git succeeds, pack v$v"
	if [ $r -ne 0 ]; then
		echo "$output" >&2
	fi

	#find simple.git -type f
	git-clone-unpacked simple-$v.git simple-unpacked-$v.git

	#find simple-unpacked.git -type f
	is "$(find simple-unpacked-$v.git/objects -type f | wc -l)" 6 \
	   "simple.git should contain six objects, pack v$v"
done
PROTO_VERSION=

:
