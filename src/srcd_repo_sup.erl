-module(srcd_repo_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include_lib("kernel/include/logger.hrl").
-include("srcd_object.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

initial_children() ->
  {ok, Repos} = srcd_persist:list(),
  initial_children(Repos, []).
initial_children([], Res) -> lists:reverse(Res);
initial_children([Repo|Repos], Res) ->
  {ok, Meta, Refs, Objects} = srcd_persist:load(Repo),
  Name = proplists:get_value(name, Meta),
  initial_children(Repos, [#{
    id => Name,
    start => {srcd_repo, start_link, [Name, Refs, Objects]}
  } | Res]).

init([]) ->
  {ok, {
    #{strategy => one_for_one,
      intensity => 2,
      period => 4},
    initial_children()
  }}.
