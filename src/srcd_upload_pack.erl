-module(srcd_upload_pack).
-export([init/2, caps/0]).
-export([
  advertise/1,
  handshake/1,
  read_command/1,
  process_command/1,
  ls_refs/1
]).

-include_lib("kernel/include/logger.hrl").
-record(?MODULE, {repo, opts=[], version=0}).

caps() -> srcd_ssh:caps() ++ [].

init(Version, Args) ->
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
  {next_state, read_command, Greeting, Data}.

advertise(#?MODULE{repo=Repo, version=Version, opts=Opts} = Data) ->
  case srcd_repo:refs(Repo) of
    {ok, Refs} ->
      {ok, Adv} = srcd_pack:advertisement(Version, Refs, caps()),
      case proplists:get_value(advertise_refs, Opts) of
        true -> {ok, Adv};
        _ -> {next_state, read_command, Adv, Data}
      end;
    {error, enoent} ->
      {error, "No such repo\n"}
  end.

read_command(#?MODULE{repo=Repo} = Data) ->
  case srcd_pack:read_command() of
    flush -> ok;
    {Cmd, Caps, Args} ->
      ?LOG_NOTICE("got upload-pack cmd ~p ~p (with caps: ~p)",
                  [Cmd, Args, Caps]),
      {next_state, process_command, {Data, Cmd, Caps, Args}}
  end.

process_command({Data, "ls-refs", Caps, Args}) ->
  {next_state, ls_refs, {Data, Caps, ls_ref_args(Args)}};
process_command({_, _, _, _}) ->
  {error, "unsupported command"}.

ls_ref_args(Args) -> ls_ref_args(Args, []).
ls_ref_args([], Res) -> lists:reverse(Res);
ls_ref_args([Arg|Args], Res) ->
  [Cmd|MaybeArgArgs] = string:split(Arg, " "),
  ls_ref_args(Args, case Cmd of
    "peel" -> [peel|Res];
    "symrefs" -> [symrefs|Res];
    "ref-prefix" ->
      [ArgArgs] = MaybeArgArgs,
      [{prefix, ArgArgs}|Res]
  end).

ls_refs({#?MODULE{repo=Repo} = Data, Caps, Args}) ->
  case srcd_repo:refs(Repo) of
    {ok, []} ->
      {ok, Adv} = srcd_pack:build_pkt([flush]),
      ?LOG_NOTICE("Reflines = ~p", [[]]),
      {next_state, read_command, Adv, Data};
    {ok, Refs0} ->
      Refs = case proplists:get_all_values(prefix, Args) of
        []       -> Refs0;
        Prefixes -> [{Ref, Id} ||
                     {Ref, Id} <- Refs0, match_any_prefix(Prefixes, Ref)]
      end,
      Reflines = srcd_pack:reflines_with_head(Refs, Repo),
      ?LOG_NOTICE("Reflines = ~p", [Reflines]),
      {ok, Adv} = srcd_pack:build_pkt(Reflines ++ [flush]),
      {next_state, read_command, Adv, Data};
    {error, enoent} ->
      {error, "No such repo"}
  end.

match_any_prefix([], Term) -> false;
match_any_prefix([Prefix|Prefixes], Term) ->
  case string:prefix(Term, Prefix) of
    nomatch -> match_any_prefix(Prefixes, Term);
    _ -> true
end.

parse_args(Args) -> parse_args(Args, []).
parse_args([Repo], Res) -> {ok, {Repo, Res}};
parse_args(["--strict"|Args], Res) ->
  parse_args(Args, [strict | Res]);
parse_args(["--stateless-rpc"|Args], Res) ->
  parse_args(Args, [stateless_rpc | Res]);
parse_args(["--advertise-refs"|Args], Res) ->
  parse_args(Args, [advertise_refs | Res]).
