-module(srcd_repo_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([add_child/1]).

-include_lib("kernel/include/logger.hrl").
-include("srcd_object.hrl").

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

mfa(Name, Refs, Objects) ->
  {srcd_repo, start_link, [Name, Refs, Objects]}.

childspec(Name, Refs, Objects) ->
  #{id => Name, start => mfa(Name, Refs, Objects)}.

initial_children() ->
  {ok, Repos} = srcd_persist:list(),
  initial_children(Repos, []).
initial_children([], Res) -> lists:reverse(Res);
initial_children([Repo|Repos], Res) ->
  {ok, Meta, Refs, Objects} = srcd_persist:load(Repo),
  Name = proplists:get_value(name, Meta),
  initial_children(Repos, [childspec(Name, Refs, Objects) | Res]).

init([]) ->
  {ok, {
    #{strategy => one_for_one,
      intensity => 2,
      period => 4},
    initial_children()
  }}.

add_child(Name) -> add_child(Name, [], []).
add_child(Name, Refs, Objects) ->
  supervisor:start_child({global, ?MODULE}, childspec(Name, Refs, Objects)).
