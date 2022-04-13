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
        id => sample_empty_repo,
	start => {srcd_repo, start_link, ["/asd"]}
      },
      #{
        id => sample_repo,
	start => {srcd_repo, start_link, ["/foo", [
          {"refs/heads/master", "de362a47d3ac19eaa8fa5759f653ba430447d371"},
          {"refs/misc/dud", "de362a47d3ac19eaa8fa5759f653ba430447d371"}
	]]}
      }
    ],
    {ok, {SupFlags, ChildSpecs}}.
