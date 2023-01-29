% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_repo).
-behavior(gen_server).
-export([start_link/1, start_link/4, create/1, create/2, init/1,
         handle_call/3, handle_cast/2]).
-export([exists/1, exists/2, head/1, head/2, refs/1, object/2,
         default_branch/1, write/3]).
-export([info/1]).

-define(STATE, ?MODULE).
-record(?STATE, {
  name,
  fs,
  profile=repo,
  head="refs/heads/master",
  refs=[],
  objects=#{}
}).
-define(gproc_name(Repo), {via, gproc, {n, l, {?MODULE, Repo}}}).

-include("srcd_object.hrl").
-include_lib("kernel/include/logger.hrl").

fs_name(Repo) ->
  DbName = filename:basename(Repo, ".git"),
  case application:get_env(srcd, data_dir) of
    {ok, Path} -> filename:absname_join(Path, DbName);
    undefined -> undefined
  end.

create(Repo) -> create(Repo, repo).
create(Repo, Profile) ->
  ?LOG_NOTICE("Creating new repo ~p with profile ~p", [Repo, Profile]),
  srcd_persistence:init(fs_name(Repo), Repo),
  {ok, _Pid} = srcd_repo_sup:add_child(Repo, Profile),
  ok.

start_link(Repo) ->
  gen_server:start_link(?gproc_name(Repo), ?MODULE,
                        [Repo], []).
start_link(Repo, Profile, Refs, Objects) ->
  gen_server:start_link(?gproc_name(Repo), ?MODULE,
                        [Repo, Profile, Refs, Objects], []).

init([Repo]) -> {ok, #?STATE{name=Repo, fs=fs_name(Repo)}};
init([Repo, Profile, Refs0, Objects]) ->
  Refs = lists:keysort(1, Refs0),
  {ok, Map} = build_object_index(Objects),
  {ok, #?STATE{
    name=Repo,
    profile=Profile,
    fs=fs_name(Repo),
    refs=Refs,
    objects=Map
  }}.

build_object_index(Objects) -> build_object_index(Objects, #{}).
build_object_index([], Res) -> {ok, Res};
build_object_index([#object{id=Id, data=Data} | Objects], Res) ->
  build_object_index(Objects, Res#{Id => Data}).

handle_call(head, _, #?STATE{head=Ref} = State) ->
  {reply, {ok, Ref}, State};
handle_call({head, "HEAD"}, _, #?STATE{refs=Refs, head=Ref} = State) ->
  Head = proplists:get_value(Ref, Refs),
  {reply, {ok, Head}, State};
handle_call({head, Ref}, _, #?STATE{refs=Refs} = State) ->
  Head = proplists:get_value(Ref, Refs, {error, nomatch}),
  {reply, Head, State};
handle_call(refs, _, #?STATE{head=Head, refs=Refs} = State) ->
  {reply, {ok, Refs}, State};
handle_call({object, Id}, _, #?STATE{objects=Objs} = State) ->
  case maps:is_key(Id, Objs) of
    true -> #{Id := Obj} = Objs,
            {reply, {ok, #object{id=Id, data=Obj}}, State};
    false -> {reply, {error, nomatch}, State}
  end;
handle_call({exists, Id}, _, #?STATE{objects=Objs} = State) ->
  {reply, maps:is_key(Id, Objs), State};
handle_call({write, Cmds, #pack{objects=NewObjs}}, _,
            #?STATE{fs=Fs, refs=Refs, objects=Objects} = State) ->
  NewObjects = add_objects(Objects, NewObjs),
  case apply_ref_cmds(Refs, NewObjects, Cmds) of
    {ok, NewRefs} ->
      srcd_persistence:dump(Fs, Cmds, NewObjs),
      {reply, ok, State#?STATE{refs=NewRefs, objects=NewObjects}}
  end;
handle_call(info, _, #?STATE{refs=Refs, objects=Objects} = State) ->
  {reply, [
    {refs, Refs},
    {object_count, length(Objects)}
  ], State}.

handle_cast(_, State) -> {noreply, State}.

apply_ref_cmds(Refs, Objects, []) -> {ok, Refs};
apply_ref_cmds(Refs, Objects, [Cmd | Cmds]) ->
  case apply_ref_cmd(Refs, Objects, Cmd) of
    {ok, NewRefs} -> apply_ref_cmds(NewRefs, Objects, Cmds);
    {error, Error} -> {error, Error}
  end.

apply_ref_cmd(Refs, Objects, {update, Ref, {Old, New}}) ->
  RefExists = lists:keymember(Ref, 1, Refs),
  NewObjExists = maps:is_key(New, Objects),
  OldDest = case lists:keyfind(Ref, 1, Refs) of
    false -> invalid;
    {Ref, Target} -> Target
  end,

  % TODO: Verify that there's a parent chain from New to Old.
  % TODO: Verify that the refs are pointing to commits?

  case {RefExists, NewObjExists, OldDest} of
    {true, true, Old} -> {ok, lists:keyreplace(Ref, 1, Refs, {Ref, New})};
    {false, _, _} -> {error, unknown_ref};
    {_, false, _} -> {error, unknown_object};
    {_, _, _} -> {error, do_a_git_fetch}
  end;
apply_ref_cmd(Refs, Objects, {create, Ref, New}) ->
  RefExists = lists:keymember(Ref, 1, Refs),
  NewObjExists = maps:is_key(New, Objects),

  case {RefExists, NewObjExists} of
    {true, true} ->
      {ok, lists:keysort(1, lists:keymerge(1, [{Ref, New}], Refs))};
    {false, _} -> {error, unknown_ref};
    {_, false} -> {error, unknown_object};
    {_, _} -> {error, do_a_git_fetch}
  end;
apply_ref_cmd(Refs, _, {delete, Ref}) ->
  % TODO: only failure mode i can think of is deleting an existing ref
  %       without permissions to do so. But i haven't started looking
  %       at permissions at all yet...
  {ok, lists:keydelete(Ref, 1, Refs)}.

add_objects(Map, []) -> Map;
add_objects(Map, [#object{id=Id, data=Obj} | Objects]) ->
  add_objects(Map#{Id => Obj}, Objects).

get_pid(Repo) -> gproc:lookup_local_name({?MODULE, Repo}).
exists(Repo) -> is_pid(get_pid(Repo)).

-define(call(Repo, Msg), case exists(Repo) of
  true -> gen_server:call(?gproc_name(Repo), Msg);
  false -> {error, no_such_repo}
end).

default_branch(Repo)            -> ?call(Repo, head).
head(Repo)                      -> ?call(Repo, {head, "HEAD"}).
head(Repo, Ref)                 -> ?call(Repo, {head, Ref}).
refs(Repo)                      -> ?call(Repo, refs).
object(Repo, Id)                -> ?call(Repo, {object, Id}).
exists(Repo, Id)                -> ?call(Repo, {exists, Id}).
write(Repo, Cmds, Packfile)     -> ?call(Repo, {write, Cmds, Packfile}).

info(Repo)                      -> ?call(Repo, info).
