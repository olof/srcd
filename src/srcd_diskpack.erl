% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_diskpack).
-behavior(srcd_persistence).

-include("srcd_object.hrl").
-include_lib("kernel/include/logger.hrl").

-export([init/0, init/2, load/1, dump/3, list/0]).

init() -> init("/tmp/test.pack", "/test").
init(Packfile, Name) ->
  % Make repo files don't exist first?

  ok = write_meta(Packfile, Name),

  {ok, Fh} = open(pack, Packfile, [write]),
  {ok, EmptyPack} = srcd_packfile:build(Name, []),
  ok = io:put_chars(Fh, EmptyPack),
  ok = file:close(Fh),

  {ok, FhRefs} = open(refs, Packfile, [write]),
  % Good job!
  ok = file:close(FhRefs),

  ok.

load(Packfile) ->
  % 1. open packfile, (but also metadata and references)
  % 2. parse packfile,
  % 3. load objects (and also metadata and references)
  ?LOG_NOTICE("loading packfile: ~p", [Packfile]),
  {ok, Meta} = read_meta(Packfile),
  {ok, Refs} = read_refs(Packfile),
  {ok, Objects} = read_objects(Packfile),
  {ok, Meta, Refs, Objects}.

dump(Packfile, Refs, Objects) ->
  % TODO make the writes atomic (i.e. tmpfile and rename)
  ok = append_objects(Packfile, Objects),
  ok = write_refs(Packfile, Refs),
  ok.

list() ->
  {ok, Dir} =  application:get_env(srcd, data_dir),
  {ok, Files} = file:list_dir(Dir),
  {ok, [filename:absname_join(Dir, filename:basename(Pack, ".pack")) ||
   Pack <- Files,
   filename:extension(Pack) =:= ".pack"]}.

open(Type, Packfile)       -> open(Type, Packfile, [read]).
open(pack, Packfile, Opts) -> file:open(Packfile ++ ".pack", Opts);
open(refs, Packfile, Opts) -> file:open(Packfile ++ ".pack.refs", Opts);
open(meta, Packfile, Opts) -> file:open(Packfile ++ ".pack.meta", Opts).

read_meta(Packfile) ->
  {ok, Fh} = open(meta, Packfile),
  Res = [{name, string:trim(io:get_line(Fh, ""))}],
  ok = file:close(Fh),
  {ok, Res}.

write_meta(Packfile, Name) ->
  {ok, Fh} = open(meta, Packfile, [write]),
  ok = io:put_chars(Fh, Name),
  ok = file:close(Fh).

read_refs(Packfile) ->
  {ok, Fh} = open(refs, Packfile),
  Refs = lists:keysort(1, read_refs(Fh, [])),
  ok = file:close(Fh),
  {ok, Refs}.
read_refs(Fh, Refs) ->
  case io:get_line(Fh, "") of
    eof -> Refs;
    Line ->
      [Hash, Ref] = string:split(Line, " "),
      [{string:trim(Ref), string:trim(Hash)} | Refs]
  end.

write_refs(Packfile, Cmds) when is_list(Packfile) ->
  {ok, Old} = read_refs(Packfile),
  {ok, New} = update_refs(maps:from_list(Old), Cmds),
  {ok, Fh} = open(refs, Packfile, [write]),
  write_refs(Fh, lists:keysort(1, maps:to_list(New)));
write_refs(Fh, []) -> file:close(Fh);
write_refs(Fh, [{Ref, Id}|New]) ->
  ok = io:put_chars(Fh, lists:concat([Id, " ", Ref])),
  write_refs(Fh, New).

update_refs(New, []) -> {ok, New};
update_refs(Old, [Cmd|Cmds]) -> update_refs(apply_ref_cmd(Old, Cmd), Cmds).

apply_ref_cmd(Old, {create, Ref, Id}) ->
  case maps:is_key(Ref, Old) of
    true -> error;
    false -> Old#{Ref => Id}
  end;
apply_ref_cmd(Old, {update, Ref, {Prev, New}}) ->
  case maps:get(Ref, Old) of
    Prev -> Old#{Ref => New};
    _ -> error
  end;
apply_ref_cmd(Old, {delete, Ref, Id}) ->
  case maps:get(Ref, Old) of
    Id -> maps:remove(Ref, Old);
    _ -> error
  end.

append_objects(Packfile, Objects) when is_list(Packfile) ->
  {ok, Fh1} = open(pack, Packfile, [read, binary]),
  <<"PACK", 2:32, OldCount:32>> = io:get_chars(Fh1, "", 12),
  NewCount = OldCount + length(Objects),
  ok = file:close(Fh1),

  {ok, Fh2} = open(pack, Packfile, [write, read]),
  {ok, Pos} = file:position(Fh2, {eof, -20}),
  ok = file:truncate(Fh2),
  ok = write_objects(Fh2, Objects),
  {ok, 0} = file:position(Fh2, bof),
  file:write(Fh2, <<"PACK", 2:32, NewCount:32>>),
  ok = file:close(Fh2),

  {ok, Fh3} = open(pack, Packfile, [read, binary]),
  {ok, Digest} = hash_packfile(Fh3),
  ok = file:close(Fh3),

  {ok, Fh4} = open(pack, Packfile, [write, append, binary]),
  ok = file:write(Fh4, Digest),
  ok = file:close(Fh4),

  ok.

write_objects(Fh, []) -> ok;
write_objects(Fh, [Obj|Objects]) ->
  ok = write_object(Fh, Obj),
  write_objects(Fh, Objects).

write_object(Fh, Obj) ->
  Packed = srcd_object:pack(Obj),
  ?LOG_NOTICE("packed object: ~p", [Packed]),
  io:put_chars(Fh, srcd_object:pack(Obj)).

hash_packfile(Fh) ->
  D = hash_packfile(Fh, crypto:hash_init(sha)),
  {ok, crypto:hash_final(D)}.
hash_packfile(Fh, D) ->
  case file:read(Fh, 4096) of
    eof -> D;
    {ok, X} -> hash_packfile(Fh, crypto:hash_update(D, X))
  end.

read_objects(Packfile) ->
  {ok, Fh} = open(pack, Packfile),
  {ok, #pack{objects=Objects}} = srcd_packfile:read(Fh),
  ok = file:close(Fh),
  ?LOG_NOTICE("loaded objects: ~p", [Objects]),
  {ok, Objects}.
