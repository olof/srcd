plan 12

git clone -q ssh://${USER}@localhost:${PORT}/empty.git >/dev/null 2>&1
is $? 0 "empty.git: successful exit status of git clone"
dir_exists empty/.git/objects "empty.git: git repo structure"
is "$(git -C empty log --oneline 2>/dev/null)" "" "empty.git: no commit log"
is "$(find empty/.git/objects -type f)" "" "empty.git: no transferred objects"

git clone -q ssh://${USER}@localhost:${PORT}/no-content.git >/dev/null 2>&1
is $? 0 "no-content.git: successful exit status of git clone"
dir_exists no-content/.git/objects "no-content.git: git repo structure"
is "$(git -C no-content log --oneline 2>/dev/null)" "4a5ddb3 second
4bd8bdb test" "no-content.git: commit log"

is "$(find no-content/.git/objects -type f -printf '%f ')" \
   "pack-0dc8876e7e344ba1cfc9008c12536e86206a6e88.pack pack-0dc8876e7e344ba1cfc9008c12536e86206a6e88.idx " \
   "no-content.git: transferred objects"

git clone -q ssh://${USER}@localhost:${PORT}/content.git >/dev/null 2>&1
is $? 0 "content.git: successful exit status of git clone"
dir_exists content/.git/objects "content.git: git repo structure"
is "$(git -C content log --oneline 2>/dev/null)" "2000f31 first data
4a5ddb3 second
4bd8bdb test" "content.git: commit log"

is "$(find content/.git/objects -type f -printf '%f ')" \
   "pack-d3067fb4d2efa4701685faa69835ab6d6e311d69.idx pack-d3067fb4d2efa4701685faa69835ab6d6e311d69.pack " \
   "content.git: transferred objects"
