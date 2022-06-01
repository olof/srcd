-module(srcd_persist).

% Trigger persistance (i.e write to disk), using a pluggable storage module;
% we currently only have a sqlite implementation,.

-include("srcd_object.hrl").
-include_lib("kernel/include/logger.hrl").
-define(forward(A, Default), case application:get_env(srcd, persistance) of
  {ok, PersistanceMod} -> apply(PersistanceMod, ?FUNCTION_NAME, A);
  _ -> Default
end).
-define(forward(A), ?forward(A, ok)).

-export([init/2, load/1, dump/3, list/0]).

init(Db, Name) -> ?forward([Db, Name]).
load(Db) -> ?forward([Db]).
dump(Db, Refs, Objects) ->
  Res = ?forward([Db, Refs, Objects]),
  case application:get_env(srcd, persistance_copy) of
    {ok, Mod} -> Mod:dump(Db, Refs, Objects);
    _ -> Res
  end.
list() -> ?forward([], {ok, []}).
