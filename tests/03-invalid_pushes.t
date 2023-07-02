plan 2

description <<'EOF'
Initiailize a default set of repos and perform invalid
write operations on them (i.e non fast-forward pushes).
EOF

fixture mutations

start

git clone "$BASE_URL/test0.git"

# Initialize a new repo, distinct from test0
git init test
echo >>test/file
git -C test add file
git -C test commit -q --message 'test change'

# Push this new repo to test0, should be rejected
git -C test remote add origin "$BASE_URL/test0.git"
git -C test push -q origin HEAD:refs/heads/master 2>/dev/null

# **Error** not_implementedTo ssh://127.199.23.92:22222/test0.git
#  ! [rejected]        HEAD -> master (fetch first)
# error: failed to push some refs to 'ssh://127.199.23.92:22222/test0.git'
#
# It *is* getting rejected, but the above "not_implemented" error
# looks suspicious.

isnt $? 0 "bad push gets rejected"

# With --force it should be accepted
git -C test push -q --force origin HEAD:refs/heads/master
is $? 0 "bad push gets accepted when forced"
