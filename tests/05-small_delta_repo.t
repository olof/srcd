plan 4

description <<'EOF'
Test will push the srcd repo itself to srcd, and see how well it
handles a simple git repository designed to make git use deltas.
EOF

fixture default

start

set -x
git clone "$BASE_URL/empty.git" repo || die "failed to clone empty.git"
cat $SRCDIR/README.md >>repo/README.md
cat $SRCDIR/README.md >>repo/README.md
cat $SRCDIR/README.md >>repo/README.md
cat $SRCDIR/README.md >>repo/README.md
cat $SRCDIR/README.md >>repo/README.md
git -C repo add README.md
git -C repo commit -m 'initial commit'

# Generate ref_delta based on blob
echo testsuite was here >>repo/README.md
git -C repo commit -m 'addendum' README.md
target=$(git -C repo rev-parse HEAD --)

# Generate ref_delta based on ref_delta
echo testsuite was here once more >>repo/README.md
git -C repo commit -m 'addendum 2' README.md

git -C repo push origin HEAD:refs/heads/master
is $? 0 "pushing to empty.git, full push, exit status" || exit 1

# Generate ref_delta based on ref_delta
echo testsuite was here many times >>repo/README.md
git -C repo commit -m 'addendum 3' README.md

target=$(git -C repo rev-parse HEAD --)

git -C repo push origin HEAD:refs/heads/master
is $? 0 "pushing to empty.git, incremental push, exit status" || exit 1

output=$(git clone "$BASE_URL/empty.git" repo-copy 2>&1)
r=$?
is $r 0 "cloning empty.git after pushing changes, exit status"
[ $r -eq 0 ] || die "$output"

is "$(git -C repo-copy rev-parse HEAD --)" \
   "$target" \
   "new clone of empty.git has updated rev"

:
