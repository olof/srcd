-module(srcd_pack_file).
-export([build/2]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MAGIC, "PACK").
-define(VERSION, 2).

header(Count) when Count < 4294967296 ->
  {ok, ?MAGIC ++ [0,0,0,?VERSION] ++ binary_to_list(<<Count:32>>)};
header(_) -> {error, too_many_objects}.

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
  Res2 = [srcd_object:encode(Obj) || Obj <- Res],
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

build(Repo, Ids) ->
  {Count, Objects} = objects(Repo, Ids),
  case header(Count) of
    {ok, Header} -> {ok, append_hash(Header ++ Objects)};
    {error, Err} -> {error, Err}
  end.
