description <<'EOF'
Test will push the srcd repo itself to srcd, commit by commit
and see how well it handles a simple real world git repository.
In case of failures, we know the commit causing the problem.
EOF

fixture default

start

git clone "$BASE_URL/empty.git" push || die "failed to clone empty.git"
git clone "$BASE_URL/empty.git" fetch || die "failed to clone empty.git copy"
git -C push remote add real $SRCDIR
git -C push fetch real || die "could not fetch srcd repo"

alias commits="git -C push log --reverse --format=%h real/master"
count=$(commits | wc -l)

plan $(($count * 3))

set -x
for commit in $(commits); do
	git -C push show $commit >&2
	set +x

	exit 1
	output=$(git -C push push origin $commit:refs/heads/master)
	r=$?
	is $r 0 "pushing srcd repo commit $commit, exit status"
	[ $r -eq 0 ] || die "$output"

	output=$(git -C fetch fetch 2>&1)
	r=$?
	is $r 0 "fetching after push, exit status"
	[ $r -eq 0 ] || die "$output"

	is "$(git -C log --format=%h -1 origin/master)" \
   		"$commit" \
   		"expected commit of origin/master after fetch"
done

:
