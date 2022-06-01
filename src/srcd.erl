-module(srcd).
-export([new/1, info/1]).

new(Name) ->
  ok = srcd_repo:create(Name).

info(Name) ->
  srcd_repo:info(Name).
