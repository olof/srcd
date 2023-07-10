% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_packfile).
-export([read/0, read/1, build/2, build/3, object_ids/1, object_deps/1]).

-include("srcd_object.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include("tests/packfile.trl").
-endif.

-define(STATE_FUN(F), fun (State) -> F(State) end).

read() -> read(standard_io).
read(IoDevice) ->
  srcd_utils:pipe({IoDevice, #pack{}}, [
    ?STATE_FUN(read_packfile_magic),
    ?STATE_FUN(read_packfile_version),
    ?STATE_FUN(read_packfile_object_count),
    ?STATE_FUN(read_packfile_objects),
    ?STATE_FUN(read_packfile_signature)
  ]).

build(Repo, Ids) -> build(Repo, [], Ids).
build(Repo, Have, Ids) when is_list(Have) ->
  build(Repo, maps:from_list([{Id, 1} || Id <- Have]), Ids);
build(Repo, Have = #{}, Ids) ->
  {Count, Objects} = objects(Repo, Have, Ids),
  case build_header(Count) of
    {ok, Header} -> {ok, append_hash(Header ++ Objects)};
    {error, Err} -> {error, Err}
  end.

object_ids(#pack{objects=Objects}) -> object_ids(Objects, []).
object_ids([], ObjectIds) -> lists:reverse(ObjectIds);
object_ids([#object{id=Id} | Objects], ObjectIds) ->
  object_ids(Objects, [Id | ObjectIds]).

object_deps(#pack{objects=Objects}) -> object_deps(Objects, []).
object_deps([], ObjectIds) -> ObjectIds;
object_deps([#object{data=Obj} | Objects], ObjectIds) ->
  object_deps(Objects, lists:concat([ObjectIds, srcd_object:deps(Obj)])).

read_packfile_magic({IoDevice, #pack{} = State}) ->
  case srcd_utils:read(IoDevice, 4, crypto:hash_init(sha)) of
    {"PACK", D} -> {ok, {IoDevice, State#pack{hash=D}}};
    Unknown -> {error, {bad_magic, Unknown}}
  end.

read_packfile_version({IoDevice, #pack{hash=Digest} = State}) ->
  case srcd_utils:read_u32(IoDevice, Digest) of
    {2, D} -> {ok, {IoDevice, State#pack{version=2, hash=D}}};
    _ -> {error, unsupported_packfile_version}
  end.

read_packfile_object_count({IoDevice, #pack{hash=Digest} = State}) ->
  {Count, D} = srcd_utils:read_u32(IoDevice, Digest),
  {ok, {IoDevice, State#pack{count=Count, hash=D}}}.

read_packfile_objects({IoDevice, #pack{count=Count, hash=Digest} = State}) ->
  {ok, Objects, D} = read_packfile_objects(IoDevice, Digest, Count, []),
  {ok, {IoDevice, State#pack{objects=Objects, hash=D}}}.
read_packfile_objects(_, Digest, 0, Res) -> {ok, lists:reverse(Res), Digest};
read_packfile_objects(IoDevice, Digest, Count, Res) ->
  {ok, Object, D} = srcd_object:read(IoDevice, Digest),
  case Object of
    #object{id=Id} ->
      ?LOG_NOTICE("Object ~p unpacked (~p remaining): ~p",
                  [Id, Count - 1, srcd_object:type(Object)]);
    #ref_delta{ref=Id} ->
      ?LOG_NOTICE("Ref delta unpacked (~p remaining): ~p", [Count - 1, Id])
  end,
  read_packfile_objects(IoDevice, D, Count - 1, [Object | Res]).

read_packfile_signature({IoDevice, #pack{hash=D} = State}) ->
  Hash = crypto:hash_final(D),
  case list_to_binary(srcd_utils:read(IoDevice, 20)) of
    Hash -> {ok, State#pack{hash=srcd_utils:bin_to_hex(Hash)}};
    ShouldB -> ?LOG_NOTICE("Hash mismatch: got vs should be ~p",
                           [{Hash, ShouldB}]),
               {error, packfile_hash_fail}
  end.

build_header(Count) when Count >= 0 andalso Count < 4294967296 ->
  {ok, lists:concat([?PACK_MAGIC, [0, 0, 0, ?PACK_VERSION],
                     binary_to_list(<<Count:32>>)])};
build_header(_) -> {error, badarg}.

append_hash(Packfile) ->
  Hash = crypto:hash(sha, list_to_binary(Packfile)),
  Packfile ++ binary_to_list(Hash).

objects(Repo, Ids) -> objects(Repo, Ids, #{}, []).
objects(Repo, Have, Ids) -> objects(Repo, Ids, Have, []).
objects(_, [], _, Res) ->
  Count = length(Res),
  Res2 = [srcd_object:pack(Obj) || Obj <- Res],
  {Count, lists:flatten(lists:reverse(Res2))};
objects(Repo, [Id | Ids], Seen, Res) ->
  case maps:is_key(Id, Seen) of
    true -> objects(Repo, Ids, Seen, Res);
    false ->
      case srcd_repo:object(Repo, Id) of
        {ok, Object} ->
          Deps = srcd_object:deps(Object),
          objects(Repo, Deps ++ Ids, Seen#{Id => 1}, [Object | Res]);
        {error, nomatch} ->
          {error, invalid_ref}
      end
  end.
