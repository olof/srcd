%%%-------------------------------------------------------------------
%% @doc srcd public API
%% @end
%%%-------------------------------------------------------------------

-module(srcd_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    srcd_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
