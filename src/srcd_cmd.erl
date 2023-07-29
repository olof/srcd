% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_cmd).
-export([exec/2]).
-include_lib("kernel/include/logger.hrl").

-callback init(Version :: integer(), Args :: list()) ->
  {ok, State :: atom(), Data :: term()}.

cmd_module("git-upload-pack", 2) -> srcd_pack_v2;
cmd_module("git-receive-pack", _) -> srcd_receive_pack;
cmd_module("git-upload-pack", _) -> srcd_upload_pack;
cmd_module(_, _) -> invalid.

invalid_command() -> {error, "invalid command\n"}.

exec(Cmd, #{version := Version}) ->
  case parse(Cmd) of
    {ok, Prog, Args} ->
      case cmd_module(Prog, Version) of
        invalid -> invalid_command();
        Mod -> enter_fsm(Mod, Version, Args)
      end;
    {error, invalid} -> invalid_command()
  end.

enter_fsm(Mod, Version, Args) ->
  ?LOG_NOTICE("initial ~p:~p (~p)", [Mod, Version, Args]),
  case Mod:init(Version, Args) of
    {ok, State, Data} -> step_fsm(Mod, State, Data);
    {error, Reason} -> {error, Reason}
  end.

step_fsm(Mod, State, Data) ->
  ?LOG_NOTICE("step ~p:~p", [Mod, State]),
  case Mod:State(Data) of
    {next_state, NewState, NewData} -> step_fsm(Mod, NewState, NewData);
    {next_state, NewState, Output, NewData} ->
      io:put_chars(Output),
      ?LOG_INFO("exec partial result: ~p", [Output]),
      step_fsm(Mod, NewState, NewData);
    ok ->
      ?LOG_INFO("exec finnished successfully"),
      {ok, ""};
    {ok, Res} ->
      ?LOG_INFO("exec finnished successfully, with some more bytes"),
      {ok, Res};
    {error, Reason} ->
      ?LOG_INFO("exec failed: ~p", [Reason]),
      {error, Reason}
  end.

parse(Cmdline) ->
  case srcd_utils:cmd_split(Cmdline) of
    {ok, [Cmd | Args]} -> {ok, Cmd, Args};
    invalid -> {error, invalid}
  end.
