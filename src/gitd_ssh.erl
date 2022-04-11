-module(gitd_ssh).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(?MODULE, {daemon}).
-record(proto, {version}).
-define(INVALID, {error, "invalid command\n"}).

-include_lib("kernel/include/logger.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Settings = application:get_env(gitd, sshd, []),
  Host = proplists:get_value(host, Settings, any),
  Port = proplists:get_value(port, Settings, 22),
  Dir = proplists:get_value(keydir, Settings, []),

  Fun = fun (Cmd, User, Addr, Env) ->
    case gitd_cmd:parse(Cmd) of
      {ok, Command, Opts} ->
        ?LOG_NOTICE("Executing command ~p ~p", [Command, Opts]),
        handle_cmd(Command, Opts, {User, Addr}, Env);
      {error, invalid} ->
        ?LOG_NOTICE("Invalid command, syntax error: ~p",
                    [Cmd]),
        ?INVALID
    end
  end,

  MsgDbgFun = fun (A, B, C, D) -> io:format("SSH message: ~p~n", [C]) end,

  Opts = lists:concat([
    [
      {system_dir, Dir},
      {key_cb, {gitd_ssh_keys, []}},
      {shell, disabled},
      {subsystems, [{"test", {gitd_ssh_ch, [10]}}]},
      {exec, {direct, Fun}},
      {auth_methods, "publickey"},
      {ssh_msg_debug_fun, MsgDbgFun}
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

git_proto(#{<<"GIT_PROTOCOL">> := Proto}) ->
  git_proto(binary_to_list(Proto));
git_proto(#{}) -> [];
git_proto(Proto) ->
  git_proto([list_to_tuple(string:split(P, "=")) ||
             P <- string:split(Proto, ":", all)], []).
git_proto([], Res) ->
  #proto{version=proplists:get_value(version, Res, 0)};
git_proto([{"version", V}|Attrs], Res) ->
  git_proto(Attrs, [{version, V}]).

handle_cmd(Cmd, Args, Auth, Env) when is_map(Env) ->
  handle_cmd(Cmd, Args, Auth, git_proto(Env));
handle_cmd("git-receive-pack", [{repo, Repo}], _Auth, Proto) ->
  case gitd_pack:receive_pack(Repo) of
    {ok, Resp} -> {ok, Resp};
    {error, Error} -> {error, Error}
  end;
handle_cmd("git-upload-pack", Opts, _Auth, _Proto) ->
  Repo = proplists:get_value(repo, Opts),
  gitd_pack:upload_pack(Repo, Opts);
handle_cmd(Cmd, [_Repo], _Auth, _Proto) ->
  ?LOG_NOTICE("Unsupported command ~p", [Cmd]),
  ?INVALID.
