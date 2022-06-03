% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%% @doc srcd public API
%% @end
%%%-------------------------------------------------------------------

-module(srcd).
-behavior(application).

-export([start/2, stop/1]).
-export([new/1, info/1]).

start(_StartType, _StartArgs) -> srcd_sup:start_link().
stop(_State) -> ok.

new(Name) -> ok = srcd_repo:create(Name).
info(Name) -> srcd_repo:info(Name).
