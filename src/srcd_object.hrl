-record(stamp, {name, email, time, tz}).
-record(commit, {tree, author, committer, msg, parents=[]}).
-record(tree, {items}).
-record(tree_node, {mode, name, object}).
-record(blob, {data}).
