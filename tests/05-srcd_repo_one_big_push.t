plan 3

description <<'EOF'
Test will push the srcd repo itself to srcd, and see how well it
handles a simple real world git repository.
EOF

fixture default

start

commit=${KNOWN_PUSHABLE_COMMIT:-real/master}

git clone "$BASE_URL/empty.git" repo || die "failed to clone empty.git"
git -C repo remote add real $SRCDIR
git -C repo fetch real || die "could not fetch srcd repo"

target="$(git -C repo rev-parse $commit)"

git -C repo push origin $commit:refs/heads/master
[ $? -eq 0 ] || exit 1
is $? 0 "pushing srcd repo to empty.git, exit status"

output=$(git clone "$BASE_URL/empty.git" repo-copy 2>&1)
r=$?
is $r 0 "cloning empty.git after pushing changes, exit status"
[ $r -eq 0 ] || die "$output"

is "$(git -C repo-copy rev-parse HEAD)" \
   "$target" \
   "new clone of empty.git has updated rev"

:
