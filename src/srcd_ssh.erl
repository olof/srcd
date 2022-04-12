-module(srcd_ssh).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([caps/0]).

-record(?MODULE, {daemon}).
-record(proto, {version}).
-define(INVALID, {error, "invalid command\n"}).

-include_lib("kernel/include/logger.hrl").

caps() -> [
  quiet,
  atomic,
  {'object-format', sha1},
  {agent, "ethup/2.32.0"}
].

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Settings = application:get_env(srcd, sshd, []),
  Host = proplists:get_value(host, Settings, any),
  Port = proplists:get_value(port, Settings, 22),
  Dir = proplists:get_value(keydir, Settings, []),
  Fun = fun (Cmd, User, Addr, Env) -> srcd_cmd:exec(Cmd, env_to_opts(Env)) end,

  Opts = lists:concat([
    [
      {system_dir, Dir},
      {key_cb, {srcd_ssh_keys, []}},
      {shell, disabled},
      {subsystems, [{"test", {srcd_ssh_ch, [10]}}]},
      {exec, {direct, Fun}},
      {auth_methods, "publickey"}
    ],
    proplists:get_value(opts, Settings, [])
  ]),
  case ssh:daemon(Host, Port, Opts) of
    {ok, Daemon} -> {ok, #?MODULE{daemon=Daemon}};
    {error, Err} -> {stop, {error, Err}}
  end.

handle_call(_, _, State) -> {reply, '?', State}.
handle_cast(_, State)    -> {noreply, State}.

terminate(_, #?MODULE{daemon=Daemon}) ->
  ssh:stop_daemon(Daemon).

env_to_opts(#{<<"GIT_PROTOCOL">> := Proto}) -> proto_params(Proto);
env_to_opts(_) -> proto_params([]).
proto_params([<<"version=", N/binary>>|_]) ->
  #{version => case N of
    <<"1">> -> 1;
    <<"2">> -> 2;
    _ -> 0
  end};
proto_params([_|Tail]) -> proto_params(Tail);
proto_params([]) -> #{version => 0};
proto_params(Proto) -> proto_params(string:split(Proto, " ", all)).
