%%%-------------------------------------------------------------------
%% @doc srcd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(srcd_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

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
        id => sample_repo,
	start => {srcd_repo, start_link, ["/asd"]}
      }
    ],
    {ok, {SupFlags, ChildSpecs}}.
