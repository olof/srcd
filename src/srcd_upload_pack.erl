-module(srcd_upload_pack).
-export([init/2, caps/0]).
-export([advertise/1, handshake/1, wait_for_cmd/1, process_cmd/1, ls_refs/1]).

-include_lib("kernel/include/logger.hrl").
-record(?MODULE, {repo, opts=[], version=0}).

caps() -> srcd_ssh:caps() ++ [].

init(Args, #{version := Version}) ->
  {ok, {Repo, Opts}} = parse_args(Args),
  Data = #?MODULE{repo=Repo, version=Version, opts=Opts},
  case Version of
    2 -> {ok, handshake, Data};
    _ -> {ok, advertise, Data}
  end.

handshake(Data) ->
  {ok, Greeting} = srcd_pack:build_pkt([
    "version 2\n",
    "agent=srcd/0\n",
    "ls-refs\n",
    "object-format=sha1\n",
    flush
  ]),
  {next_state, wait_for_cmd, Greeting, Data}.

advertise(#?MODULE{repo=Repo, version=Version, opts=Opts} = Data) ->
  case srcd_repo:refs(Repo) of
    {ok, Refs} ->
      {ok, Adv} = srcd_pack:advertisement(Version, Refs, caps()),
      case proplists:get_value(advertise_refs, Opts) of
        true -> {ok, Adv};
        _ -> {next_state, wait_for_cmd, Adv, Data}
      end;
    {error, enoent} ->
      {error, "No such repo\n"}
  end.

wait_for_cmd(#?MODULE{repo=Repo} = Data) ->
  case read_command() of
    flush -> ok;
    {Cmd, Caps, Args} ->
      ?LOG_NOTICE("got upload-pack cmd ~p ~p (with caps: ~p)",
                  [Cmd, Args, Caps]),
      {next_state, process_cmd, {Data, Cmd, Caps, Args}}
  end.

process_cmd({Data, Cmd, Caps, Args}) ->
  case Cmd of
    "ls-refs" -> {next_state, ls_refs, {Data, Caps, Args}};
    _ -> {error, "unsupported command"}
  end.

ls_refs({#?MODULE{repo=Repo} = Data, Caps, Args}) ->
  case srcd_repo:refs(Repo) of
    {ok, Refs} ->
      {ok, Adv} = srcd_pack:advertisement(2, Refs, []),
      {next_state, wait_for_cmd, Adv, Data};
    {error, enoent} ->
      {error, "No such repo"}
  end.

parse_args(Args) -> parse_args(Args, []).
parse_args([Repo], Res) -> {ok, {Repo, Res}};
parse_args(["--strict"|Args], Res) ->
  parse_args(Args, [strict | Res]);
parse_args(["--stateless-rpc"|Args], Res) ->
  parse_args(Args, [stateless_rpc | Res]);
parse_args(["--advertise-refs"|Args], Res) ->
  parse_args(Args, [advertise_refs | Res]).

hex(Str) -> list_to_integer(Str, 16).
read(Len) -> io:get_chars("", Len).
read_length() -> hex(read(4)) - 4.

read_command() ->
  Len = read_length(),
  case Len of
    -4 -> flush;
    Len when Len >= 0 ->
      Input = read(Len),
      ["command", Command] = string:split(Input, "="),
      read_command(string:trim(Command), [], [], false)
  end.
read_command(Cmd, Caps, Args, DelimSeen) ->
  Len = read_length(),
  case Len of
    -4 -> {Cmd, Caps, lists:reverse(Args)};
    -3 -> read_command(Cmd, lists:reverse(Caps), Args, true);
    Len ->
      Input = string:trim(read(Len)),
      if DelimSeen -> read_command(Cmd, Caps, [Input|Args], DelimSeen);
         true      -> read_command(Cmd, [parse_cap(Input)|Caps], Args,
	                           DelimSeen)
      end
  end.

parse_cap(Str) ->
  case string:split(Str, "=") of
    [Str] -> cap_atom(Str);
    [Key, Val] -> {cap_atom(Key), Val}
  end.

cap_atom("agent") -> agent;
cap_atom("object-format") -> 'object-format';
cap_atom(C) -> C.
