-module(srcd_cmd).
-export([exec/2]).
-include_lib("kernel/include/logger.hrl").

cmd_module("git-receive-pack") -> srcd_receive_pack;
cmd_module("git-upload-pack") -> srcd_upload_pack;
cmd_module(_) -> invalid.

exec(Cmd, Env) ->
  ?LOG_NOTICE("ENV = ~p", [Env]),
  case parse(Cmd) of
    {ok, Prog, Args} ->
      case cmd_module(Prog) of
        invalid -> {error, invalid_cmd};
        Mod -> enter_fsm(Mod, Args, Env)
      end;
    {error, invalid} -> {error, invalid_cmd}
  end.

enter_fsm(Mod, Args, Env) ->
  ?LOG_NOTICE("initial ~p:~p (~p)", [Mod, Args, Env]),
  case Mod:init(Args, Env) of
    {ok, State, Data} -> step_fsm(Mod, State, Data);
    {error, Reason} -> {error, Reason}
  end.

step_fsm(Mod, State, Data) ->
  ?LOG_NOTICE("step ~p:~p (~p)", [Mod, State, Data]),
  case Mod:State(Data) of
    {next_state, NewState, NewData} -> step_fsm(Mod, NewState, NewData);
    {next_state, NewState, Output, NewData} ->
      io:put_chars(Output),
      step_fsm(Mod, NewState, NewData);
    ok -> {ok, ""};
    {ok, Res} -> {ok, Res};
    {error, Reason} -> {error, Reason}
  end.

parse(Cmdline) ->
  case srcd_utils:cmd_split(Cmdline) of
    {ok, [Cmd | Args]} -> {ok, Cmd, Args};
    invalid -> {error, invalid}
  end.
