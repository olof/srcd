%%%-------------------------------------------------------------------
%% @doc gitd public API
%% @end
%%%-------------------------------------------------------------------

-module(gitd_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gitd_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
