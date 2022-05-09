plan 2
REPO_BASE=ssh://${USER}@localhost:${PORT}
ls_remote() {
	git ls-remote "$REPO_BASE/$1" 2>/dev/null
}

is "$(ls_remote empty.git)" "" "empty.git: no remote refs"
is "$(ls_remote content.git)" \
"2000f31abf7f7fb344a9e9f4ad3e396f1b8fe46a	HEAD
2000f31abf7f7fb344a9e9f4ad3e396f1b8fe46a	refs/heads/master" \
"content.git: remote refs"
