plan 12

git clone -q ssh://${USER}@$HOST:${PORT}/empty.git
is $? 0 "empty.git: successful exit status of git clone"
dir_exists empty/.git/objects "empty.git: git repo structure"
is "$(git -C empty log --oneline 2>/dev/null)" "" "empty.git: no commit log"
is "$(find empty/.git/objects -type f)" "" "empty.git: no transferred objects"

git clone -q ssh://${USER}@$HOST:${PORT}/no-content.git
is $? 0 "no-content.git: successful exit status of git clone"
dir_exists no-content/.git/objects "no-content.git: git repo structure"
is "$(git -C no-content log --oneline 2>/dev/null)" "4a5ddb3 second
4bd8bdb test" "no-content.git: commit log"

git-clone-unpacked no-content no-content-unpacked

is "$(find no-content-unpacked/objects -type f -printf '%f\n' | sort)" \
"5ddb3241c127daa27cf1ba74adba1f284f6693
825dc642cb6eb9a060e54bf8d69288fbee4904
d8bdbdc6661187350f6e6141577c3d7cda1ac6" \
   "no-content.git: transferred objects"

git clone -q ssh://${USER}@$HOST:${PORT}/content.git
is $? 0 "content.git: successful exit status of git clone"
dir_exists content/.git/objects "content.git: git repo structure"
is "$(git -C content log --oneline 2>/dev/null)" "2000f31 first data
4a5ddb3 second
4bd8bdb test" "content.git: commit log"

git-clone-unpacked content content-unpacked

is "$(find content-unpacked/objects -type f -printf '%f\n' | sort)" \
"00f31abf7f7fb344a9e9f4ad3e396f1b8fe46a
39464be82b4c0c6f26551a9ae5905fe80747c8
5ddb3241c127daa27cf1ba74adba1f284f6693
69488f7fb1f4b56a8c0e5eb48cecbfadfa9219
825dc642cb6eb9a060e54bf8d69288fbee4904
d8bdbdc6661187350f6e6141577c3d7cda1ac6" \
   "content.git: transferred objects"
