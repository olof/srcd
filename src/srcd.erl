-module(srcd).
-export([new/1, info/1]).

new(Name) ->
  ok = srcd_repo:create(Name),
  case supervisor:start_child(srcd_sup, #{
    id => sample_empty_repo,
    start => {srcd_repo, start_link, [Name]}
  }) of
    {ok, _} -> ok;
    {ok, _, _} -> ok;
    Err -> Err
 end.

info(Name) ->
  srcd_repo:info(Name).
