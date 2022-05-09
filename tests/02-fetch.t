plan 2
REPO_BASE="ssh://${USER}@localhost:${PORT}"

git clone -q "$REPO_BASE/empty.git" repo >/dev/null 2>&1 ||
	exit 1

is "$(find repo/.git/objects -type f)" "" \
	"no transferred objects on initial clone"
git -C repo remote rm origin
git -C repo remote add origin $REPO_BASE/content.git
git -C repo fetch -p origin >/dev/null 2>&1
is "$(find repo/.git/objects -type f -printf '%f ' | sort)" \
	"69488f7fb1f4b56a8c0e5eb48cecbfadfa9219 d8bdbdc6661187350f6e6141577c3d7cda1ac6 825dc642cb6eb9a060e54bf8d69288fbee4904 39464be82b4c0c6f26551a9ae5905fe80747c8 5ddb3241c127daa27cf1ba74adba1f284f6693 00f31abf7f7fb344a9e9f4ad3e396f1b8fe46a " \
	"transferred objects on fetch"
