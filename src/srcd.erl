%%%-------------------------------------------------------------------
%% @doc srcd public API
%% @end
%%%-------------------------------------------------------------------

-module(srcd).
-behaviour(application).

-export([start/2, stop/1]).
-export([new/1, info/1]).

start(_StartType, _StartArgs) -> srcd_sup:start_link().
stop(_State) -> ok.

new(Name) -> ok = srcd_repo:create(Name).
info(Name) -> srcd_repo:info(Name).
