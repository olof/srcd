% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_repo_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([add_child/1, add_child/2]).

-include_lib("kernel/include/logger.hrl").
-include("srcd_object.hrl").

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

mfa(Name, Profile, Refs, Objects) ->
  {srcd_repo, start_link, [Name, Profile, Refs, Objects]}.

childspec(Name, Profile, Refs, Objects) ->
  #{id => Name, start => mfa(Name, Profile, Refs, Objects)}.

initial_children() ->
  {ok, Repos} = srcd_persistence:list(),
  initial_children(Repos, []).
initial_children([], Res) -> lists:reverse(Res);
initial_children([Repo | Repos], Res) ->
  {ok, Meta, Refs, Objects} = srcd_persistence:load(Repo),
  Name = proplists:get_value(name, Meta),
  Profile = proplists:get_value(profile, Meta),
  initial_children(Repos, [childspec(Name, Profile, Refs, Objects) | Res]).

init([]) ->
  {ok, {
    #{strategy => one_for_one,
      intensity => 2,
      period => 4},
    initial_children()
  }}.

add_child(Name) -> add_child(Name, repo).
add_child(Name, Profile) -> add_child(Name, Profile, [], []).
add_child(Name, Profile, Refs, Objects) ->
  supervisor:start_child({global, ?MODULE},
                         childspec(Name, Profile, Refs, Objects)).
