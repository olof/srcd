plan 2

description <<'EOF'
Initiailize an empty server (no fixtures, no repos loaded).
EOF

start

output=$(git clone "$BASE_URL/test.git" 2>&1)

isnt $? 0 "cloning non-existing repo fails"
contains "$output" "no_such_repo" "error from sshd should mention reason"

:
