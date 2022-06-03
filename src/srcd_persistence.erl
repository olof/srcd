% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_persistence).

% Trigger persistence (i.e write to disk), using a pluggable storage module;
% we currently only have a sqlite implementation,.

-callback init(Db :: term(), Name :: term()) -> ok.
-callback load(Db :: term()) ->
  {ok, Meta :: list(), Refs :: list(), Objects :: list()}.
-callback dump(Db :: term(), Refs :: term(), Objects :: term()) -> ok.
-callback list() -> {ok, Res :: list()}.

-include("srcd_object.hrl").
-include_lib("kernel/include/logger.hrl").
-define(forward(A, Default), case application:get_env(srcd, persistence) of
  {ok, PersistanceMod} -> apply(PersistanceMod, ?FUNCTION_NAME, A);
  _ -> Default
end).
-define(forward(A), ?forward(A, ok)).

-export([init/2, load/1, dump/3, list/0]).

init(Db, Name) -> ?forward([Db, Name]).
load(Db) -> ?forward([Db]).
dump(Db, Refs, Objects) ->
  Res = ?forward([Db, Refs, Objects]),
  case application:get_env(srcd, persistence_copy) of
    {ok, Mod} -> Mod:dump(Db, Refs, Objects);
    _ -> Res
  end.
list() -> ?forward([], {ok, []}).
