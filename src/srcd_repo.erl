-module(srcd_repo).
-behaviour(gen_server).
-export([start_link/1, start_link/3, init/1, handle_call/3, handle_cast/2]).
-export([exists/1, exists/2, head/1, head/2, refs/1, object/2, default_branch/1]).

-record(?MODULE, {name, head="refs/heads/master", refs=[], objects=#{}}).
-define(gproc_name(Repo), {via, gproc, {n, l, {?MODULE, Repo}}}).

-include_lib("kernel/include/logger.hrl").

start_link(Repo)                -> gen_server:start_link(?gproc_name(Repo),
                                                         ?MODULE, [Repo], []).
start_link(Repo, Refs, Objects) -> gen_server:start_link(?gproc_name(Repo),
                                                         ?MODULE,
                                                         [Repo, Refs, Objects],
                                                         []).
init([Repo])                -> {ok, #?MODULE{name=Repo}};
init([Repo, Refs0, Objects]) ->
   Refs = lists:keysort(1, Refs0),
   {ok, Map} = build_index(Refs, Objects),
   {ok, #?MODULE{name=Repo, refs=Refs, objects=Map}}.

build_index(Refs, Objects) -> build_index(Refs, Objects, #{}).
build_index(Refs, [], Res) -> {ok, Res};
build_index(Refs, [{Id, Data}|Objects], Res) ->
  build_index(Refs, Objects, Res#{Id => Data}).

handle_call(head, _, #?MODULE{head=Ref} = State) ->
  {reply, {ok, Ref}, State};
handle_call({head, "HEAD"}, _, #?MODULE{refs=Refs, head=Ref} = State) ->
  Head = proplists:get_value(Ref, Refs),
  {reply, {ok, Head}, State};
handle_call({head, Ref}, _, #?MODULE{refs=Refs} = State) ->
  Head = proplists:get_value(Ref, Refs, {error, nomatch}),
  {reply, Head, State};
handle_call(refs, _, #?MODULE{head=Head, refs=Refs} = State) ->
  {reply, {ok, Refs}, State};
handle_call({object, Id}, _, #?MODULE{objects=Objs} = State) ->
  case maps:is_key(Id, Objs) of
    true -> #{Id := Obj} = Objs,
            {reply, {ok, Obj}, State};
    false -> {reply, {error, nomatch}, State}
  end;
handle_call({exists, Id}, _, #?MODULE{objects=Objs} = State) ->
  {reply, maps:is_key(Id, Objs), State};
handle_call({update, Ref, Id}, _, #?MODULE{refs=Refs} = State) ->
  New = lists:keysort(1, lists:keymerge(1, [{Ref, Id}], Refs)),
  {reply, ok, New}.

handle_cast(_, State) -> {noreply, State}.

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
