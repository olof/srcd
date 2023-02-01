% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-define(PACK_MAGIC, "PACK").
-define(PACK_VERSION, 2).

-record(object, {id, data}).
-record(stamp, {name, email, time, tz}).
-record(commit, {tree, author, committer, msg, parents=[]}).
-record(tree, {items}).
-record(tree_node, {mode, name, object}).
-record(blob, {data}).
-record(ref_delta, {ref, size_base, size_target, instructions=[]}).
-record(pack, {version, count, objects, hash}).
