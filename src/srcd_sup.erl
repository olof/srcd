%%%-------------------------------------------------------------------
%% @doc srcd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(srcd_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("srcd_object.hrl").
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
      #{
        id => sshd,
        start => {srcd_ssh, start_link, []}
      },
      #{
        id => repo_sup,
        start => {srcd_repo_sup, start_link, []},
        type => supervisor
      }
    ],
    {ok, {SupFlags, ChildSpecs}}.
