-module(srcd_packfile).
-export([read/0, build/2]).

-include("srcd_object.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(state_funwrap(F), fun (State) -> F(State) end).

read() ->
  srcd_utils:pipe(#pack{}, [
    ?state_funwrap(read_packfile_magic),
    ?state_funwrap(read_packfile_version),
    ?state_funwrap(read_packfile_object_count),
    ?state_funwrap(read_packfile_objects),
    ?state_funwrap(read_packfile_signature)
  ]).

build(Repo, Ids) ->
  {Count, Objects} = objects(Repo, Ids),
  ?LOG_NOTICE("Got ~p objects: ~p", [Count, Objects]),
  case build_header(Count) of
    {ok, Header} -> {ok, append_hash(Header ++ Objects)};
    {error, Err} -> {error, Err}
  end.

read_packfile_magic(#pack{} = State) ->
  case srcd_utils:read(4, crypto:hash_init(sha)) of
    {"PACK", D} -> {ok, State#pack{hash=D}};
    Unknown -> {error, {bad_magic, Unknown}}
  end.

read_packfile_version(#pack{hash=Digest} = State) ->
  case srcd_utils:read_u32(Digest) of
    {2, D} -> {ok, State#pack{version=2, hash=D}};
    _ -> {error, unsupported_packfile_version}
  end.

read_packfile_object_count(#pack{hash=Digest} = State) ->
  {Count, D} = srcd_utils:read_u32(Digest),
  {ok, State#pack{count=Count, hash=D}}.

read_packfile_objects(#pack{count=Count, hash=Digest} = State) ->
  {ok, Objects, D} = read_packfile_objects(Digest, Count, []),
  {ok, State#pack{objects=Objects, hash=D}}.
read_packfile_objects(Digest, 0, Res) -> {ok, lists:reverse(Res), Digest};
read_packfile_objects(Digest, Count, Res) ->
  {ok, Object, D} = srcd_object:read(Digest),
  ?LOG_NOTICE("Object unpacked (~p remaining): ~p", [Count-1, Object]),
  read_packfile_objects(D, Count-1, [Object | Res]).

read_packfile_signature(#pack{hash=D} = State) ->
  Hash = crypto:hash_final(D),
  case list_to_binary(srcd_utils:read(20)) of
    Hash -> {ok, State#pack{hash=srcd_utils:bin_to_hex(Hash)}};
    ShouldB -> ?LOG_NOTICE("Hash mismatch: got vs should be ~p",
                           [{Hash, ShouldB}]),
               {error, packfile_hash_fail}
  end.

build_header(Count) when Count >= 0 andalso Count < 4294967296 ->
  {ok, ?PACK_MAGIC ++ [0,0,0,?PACK_VERSION] ++ binary_to_list(<<Count:32>>)};
build_header(_) -> {error, badarg}.

append_hash(Packfile) ->
  Hash = crypto:hash(sha, list_to_binary(Packfile)),
  Packfile ++ binary_to_list(Hash).

-ifdef(TEST).

packfile_header_test_() ->
  Prefix = [$P, $A, $C, $K, 0, 0, 0, 2], % PackVersion = 2
  [
    ?_assertEqual({ok, Prefix ++ [0,0,0,0]}, build_header(0)),
    ?_assertEqual({ok, Prefix ++ [0,0,0,1]}, build_header(1)),
    ?_assertEqual({ok, Prefix ++ [0,0,0,128]}, build_header(128)),
    ?_assertEqual({ok, Prefix ++ [0,0,0,255]}, build_header(255)),
    ?_assertEqual({ok, Prefix ++ [0,0,1,0]}, build_header(256)),
    ?_assertEqual({ok, Prefix ++ [0,0,255,255]}, build_header(65535)),
    ?_assertEqual({ok, Prefix ++ [0,255,255,255]}, build_header(16777215)),
    ?_assertEqual({ok, Prefix ++ [1,0,0,0]}, build_header(16777216)),
    ?_assertEqual({ok, Prefix ++ [255,255,255,255]}, build_header(4294967295)),
    ?_assertEqual({error, badarg}, build_header(4294967296)),
    ?_assertEqual({error, badarg}, build_header(-1))
  ].
-endif.

objects(Repo, Ids) -> objects(Repo, Ids, #{}, []).
objects(_, [], _, Res) ->
  ?LOG_NOTICE("RESSS = ~p", [Res]),
  Count = length(Res),
  Res2 = [srcd_object:pack(Obj) || Obj <- Res],
  {Count, lists:flatten(lists:reverse(Res2))};
objects(Repo, [Id | Ids], Seen, Res) ->
  case maps:is_key(Id, Seen) of
    true -> objects(Repo, Ids, Seen, Res);
    false ->
      case srcd_repo:object(Repo, Id) of
        {ok, Object} ->
	  ?LOG_NOTICE("Obj: ~p", [Object]),
	  Deps = srcd_object:deps(Object),
	  ?LOG_NOTICE("Obj deps: ~p", [Deps]),
          objects(Repo, Deps ++ Ids, Seen#{Id => 1}, [Object | Res]);
        {error, nomatch} ->
          {error, invalid_ref}
      end
  end.
