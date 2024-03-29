description <<'EOF'
Test will push the srcd repo itself to srcd, commit by commit
and see how well it handles a simple real world git repository.
In case of failures, we know the commit causing the problem.
It will abort at first sign of trouble.

After each successful step, the debug packfile written by git
is removed, to make sure that if a test fail any packfile found
is related to the failure.
EOF

fixture default

start

git clone "$BASE_URL/empty.git" push || die "failed to clone empty.git"
git clone "$BASE_URL/empty.git" fetch || die "failed to clone empty.git copy"
git -C push remote add real $SRCDIR
git -C push fetch real || die "could not fetch srcd repo"

alias commits="git -C push log --reverse --format=%h ${KNOWN_PUSHABLE_COMMIT:-real/master}"
count=$(commits | wc -l)

plan $(($count * 3))

for commit in $(commits); do
	rm -f "$GIT_TRACE_PACKFILE"
	echo "# pushing $commit"
	#git -C push show $commit >&2
	output=$(git -C push push origin $commit:refs/heads/master 2>&1)
	r=$?
	is $r 0 "pushing srcd repo commit $commit, exit status"
	[ $r -eq 0 ] || die "$output"

	rm -f "$GIT_TRACE_PACKFILE"
	output=$(git -C fetch fetch 2>&1)
	r=$?
	is $r 0 "fetching after push, exit status"
	[ $r -eq 0 ] || die "$output"

	is "$(git -C fetch log --format=%h -1 origin/master)" \
		"$commit" \
		"expected commit of origin/master after fetch" || exit 1
done

:
