-module(gitd_repo).
-behaviour(gen_server).
-export([d/0, start_link/1, init/1, handle_call/3, handle_cast/2]).
-export([exists/1, head/1, head/2, refs/1, default_branch/1]).

-record(?MODULE, {name, head="heads/master", refs=[]}).
-define(gproc_name(Repo), {via, gproc, {n, l, {?MODULE, Repo}}}).
-define(call(Repo, Msg), gen_server:call(?gproc_name(Repo), Msg)).

-include_lib("kernel/include/logger.hrl").

d() -> start_link("/asd").
start_link(Repo) -> gen_server:start_link(?gproc_name(Repo), ?MODULE, [Repo], []).
init([Repo])     -> {ok, #?MODULE{name=Repo}}.

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
handle_call({update, Ref, Id}, _, #?MODULE{refs=Refs} = State) ->
  New = lists:keysort(1, lists:keymerge(1, [{Ref, Id}], Refs)),
  {reply, ok, New}.

handle_cast(_, State)    -> {noreply, State}.

get_pid(Repo) -> gproc:lookup_local_name({?MODULE, Repo}).
exists(Repo) -> is_pid(get_pid(Repo)).

-define(exists(AndThen), case exists(Repo) of
  true -> AndThen;
  false -> {error, enoent}
end).

default_branch(Repo) -> ?exists(?call(Repo, head)).
head(Repo) -> ?exists(?call(Repo, {head, "HEAD"})).
head(Repo, Ref) -> ?exists(?call(Repo, {head, Ref})).
refs(Repo) -> ?exists(?call(Repo, refs)).
