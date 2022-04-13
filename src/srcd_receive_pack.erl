-module(srcd_receive_pack).
-export([callback_mode/0, init/2]).
-export([advertise/1, wait_for_cmd/1, process_line/1]).
-include_lib("kernel/include/logger.hrl").

-record(?MODULE, {repo, version=0}).

callback_mode() -> state_functions.

caps() -> srcd_ssh:caps() ++ [].

init(Version, [Repo]) ->
  {ok, advertise, #?MODULE{repo=Repo, version=Version}}.

advertise(#?MODULE{repo=Repo, version=Version} = Data) ->
  ?LOG_NOTICE("doing receive_pack adveritse"),
  case advertisement(Repo, Version) of
    {ok, Resp} ->
      ?LOG_NOTICE("Resp: ~p", [Resp]),
      {next_state, wait_for_cmd, Resp, Data};
    {error, Error} -> {error, Error}
  end.

wait_for_cmd(#?MODULE{repo=Repo} = Data) ->
  ?LOG_NOTICE("wait for cmd"),
  Len = list_to_integer(io:get_chars("", 4), 16),
  case Len of
    0 -> ok;
    Len ->
      Line = io:get_chars("", Len),
      ?LOG_NOTICE("got line(~p): ~p", [Len, Line]),
      ?LOG_NOTICE("got line(~p): ~p", [Len, string:split(Line, "\0")]),
      {next_state, process_line, {Data, Line}}
  end.

process_line(Data) ->
  ?LOG_NOTICE("process_line"),
  not_implemented.

advertisement(Repo, Version) ->
  Refs = case srcd_repo:refs(Repo) of
    {ok, []} ->
      [
        {"capabilities^{}", "0000000000000000000000000000000000000000"}
      ];
    {ok, Refs0} ->
      {ok, Head} = srcd_repo:default_branch(Repo),
      case proplists:get_value(Head, Refs0) of
        undefined -> Refs0;
        Commit -> [{"HEAD", Commit} | Refs0]
      end;
    {error, enoent} -> {error, "No such repo\n"}
  end,
  case Refs of
    {error, Err} -> {error, Err};
    Refs -> srcd_pack:advertisement(Version, Refs, caps())
  end.
