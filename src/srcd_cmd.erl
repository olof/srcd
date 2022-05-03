-module(srcd_cmd).
-export([exec/2]).
-include_lib("kernel/include/logger.hrl").

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
  ?LOG_NOTICE("step ~p:~p (~p)", [Mod, State, Data]),
  case Mod:State(Data) of
    {next_state, NewState, NewData} -> step_fsm(Mod, NewState, NewData);
    {next_state, NewState, Output, NewData} ->
      ?LOG_NOTICE("> ~p", [Output]),
      io:put_chars(Output),
      step_fsm(Mod, NewState, NewData);
    ok ->
      ?LOG_INFO("exec finnished successfully"),
      {ok, ""};
    {ok, Res} ->
      ?LOG_NOTICE("> ~p", [Res]),
      ?LOG_INFO("exec finnished successfully"),
      {ok, Res};
    {error, Reason} ->
      ?LOG_NOTICE("> ~p", [Reason]),
      ?LOG_INFO("exec failed"),
      {error, Reason}
  end.

parse(Cmdline) ->
  case srcd_utils:cmd_split(Cmdline) of
    {ok, [Cmd | Args]} -> {ok, Cmd, Args};
    invalid -> {error, invalid}
  end.
