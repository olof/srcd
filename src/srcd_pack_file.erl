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
-define(_test_header(Expect, Count),
        ?_assertEqual({ok, ?MAGIC ++ [0, 0, 0, ?VERSION] ++ Expect},
	              header(Count))).

header_test_() ->
  [
    ?_test_header([0, 0, 0, 0], 0),
    ?_test_header([0, 0, 0, 1], 1),
    ?_test_header([0, 0, 0, 128], 128),
    ?_test_header([0, 0, 0, 255], 255),
    ?_test_header([0, 0, 1, 0], 256),
    ?_test_header([0, 0, 255, 255], 65535),
    ?_test_header([0, 255, 255, 255], 16777215),
    ?_test_header([1, 0, 0, 0], 16777216),
    ?_test_header([255, 255, 255, 255], 4294967295),
    ?_assertEqual({error, too_many_objects}, header(4294967296))
  ].
-endif.

objects(Repo, Ids) -> objects(Repo, Ids, #{}, []).
objects(_, [], _, Res) ->
  ?LOG_NOTICE("RESSS = ~p", [Res]),
  Count = length(Res),
  Res2 = [srcd_pack_object:build(Obj) || Obj <- Res],
  {Count, lists:flatten(lists:reverse(Res2))};
objects(Repo, [Id | Ids], Seen, Res) ->
  case maps:is_key(Id, Seen) of
    true -> objects(Repo, Ids, Seen, Res);
    false ->
      case srcd_repo:object(Repo, Id) of
        {ok, Object} ->
	  ?LOG_NOTICE("Obj: ~p", [Object]),
	  Deps = srcd_pack_object:deps(Object),
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
