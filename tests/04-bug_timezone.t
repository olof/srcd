plan 10

description <<'EOF'
A bug previously caused the zlib inflater to overread the input
stream; the test case that was identified for this was varying the
TZ between Europe/Stockholm and UTC. This test is left as a
regression test.

The error that we saw previously was the ssh connection dying
with an error message of "{case_clause,0}". This was caused by
the srcd_object parser started reading at the wrong location and
got an invalid object type id.
EOF

fixture mutations

start

export TZ=UTC

output=$(git clone "$BASE_URL/test0.git" 2>&1)
r=$?
is $r 0 "cloning test0.git, exit status"
[ $r -eq 0 ] || echo "$output" >&2

is "$(git -C test0 rev-parse HEAD)" \
   "399a62bb6d769a769f5ebc6007c8e0db80a251c4" \
   "test0.git starts at expected commit (399a62b)"

# Test that we can push commits with no new tree/blob objects
git -C test0 commit -q --allow-empty --message 'nochange'
is "$(git -C test0 rev-parse HEAD)" \
   "4f20cc3e260d19f0607ee3d2bab9644126923e36" \
   "test0.git local update to 4f20cc3" || git -C test0 log --format=fuller

git -C test0 push origin HEAD:refs/heads/master
is $r 0 "pushing 4f20cc3 to test0.git, exit status"

output=$(git clone "$BASE_URL/test0.git" test0-copy1 2>&1)
r=$?
is $r 0 "cloning test0.git again, exit status"
[ $r -eq 0 ] || echo "$output" >&2

is "$(git -C test0-copy1 rev-parse HEAD)" \
   "4f20cc3e260d19f0607ee3d2bab9644126923e36" \
   "new clone of test0.git has updated rev of 4f20cc3"

# Test that we can push real changes
echo >>test0/file
git -C test0 add file
git -C test0 commit -q --message 'test change'

is "$(git -C test0 rev-parse HEAD)" \
   "47149ecc09c619ac7abf91f6623b72a868375422" \
   "test0.git locally updated to 47149ec"

git -C test0 push origin HEAD:refs/heads/master
r=$?
is $r 0 "pushing 47149ec to test0.git, exit status"
[ $r -eq 0 ] || echo "$output" >&2

output=$(git clone "$BASE_URL/test0.git" test0-copy2 2>&1)
r=$?
is $r 0 "cloning test0.git again, exit status"
[ $r -eq 0 ] || echo "$output" >&2

is "$(git -C test0-copy2 rev-parse HEAD)" \
   "47149ecc09c619ac7abf91f6623b72a868375422" \
   "new clone of test0.git has updated rev of 47149ec"

:
