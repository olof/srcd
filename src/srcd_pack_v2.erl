-module(srcd_pack_v2).
-export([init/2, caps/0]).
-export([
  handshake/1,
  read_command/1,
  process_command/1,
  ls_refs/1,
  fetch/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("kernel/include/logger.hrl").
-record(?MODULE, {repo, opts=[], version=0}).

caps() -> srcd_ssh:caps() ++ [].

init(Version = 2, Args) ->
  {ok, {Repo, Opts}} = parse_args(Args),
  Data = #?MODULE{repo=Repo, version=Version, opts=Opts},
  {ok, handshake, Data}.

handshake(Data) ->
  {ok, Greeting} = srcd_pack:build_pkt([
    "version 2\n",
    "agent=srcd/0\n",
    "ls-refs\n",
    "fetch=wait-for-done\n",
    "object-format=sha1\n",
    "server-option\n",
    flush
  ]),
  {next_state, read_command, Greeting, Data}.

read_command(#?MODULE{repo=Repo} = Data) ->
  case srcd_pack:read_command() of
    flush -> ok;
    {Cmd, Caps, Args} ->
      ?LOG_NOTICE("got upload-pack cmd ~p ~p (with caps: ~p)",
                  [Cmd, Args, Caps]),
      {next_state, process_command, {Data, Cmd, {Caps, Args}}}
  end.

process_command({Data, "ls-refs", {Caps, Args}}) ->
  {next_state, ls_refs, {Data, Caps, ls_ref_args(Args)}};
process_command({Data, "fetch", {Caps, Args}}) ->
  {next_state, fetch, {Data, Caps, fetch_args(Args)}};
process_command({_, _, _}) ->
  {error, "unsupported command"}.

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
      Symrefs = proplists:get_value(symrefs, Args, false),
      Reflines = reflines_with_head(Refs, Repo, Symrefs),
      ?LOG_NOTICE("Reflines = ~p", [Reflines]),
      {ok, Adv} = srcd_pack:build_pkt(Reflines ++ [flush]),
      {next_state, read_command, Adv, Data};
    {error, enoent} ->
      {error, "No such repo"}
  end.

fetch({#?MODULE{repo=Repo} = Data, Caps, Args}) ->
  ?LOG_NOTICE("Client args ~p", [Args]),
  Wants = proplists:get_all_values(want, Args),
  ?LOG_NOTICE("Client wants ~p", [Wants]),
  Haves = proplists:get_all_values(have, Args),
  ?LOG_NOTICE("Client has ~p", [Haves]),
  case proplists:get_bool(done, Args) of
    true -> fetch_packfile(Repo, Wants, Haves);
    false -> fetch_ack(Data, Repo, Wants, Haves)
  end.

fetch_packfile(Repo, Wants, Haves) ->
  {ok, Data} = srcd_pack_file:build(Repo, Wants),
  ?LOG_NOTICE("fetch_packfile got ~p", [Data]),
  srcd_pack:build_pkt([
    "packfile\n",
    [2|"ok this is happening\n"],
    [1|Data],
    flush
  ]).

fetch_ack(Data, Repo, Wants, Haves) ->
  case has_all_oids(Repo, Haves) of
    true ->
      {ok, Packfile} = fetch_packfile(Repo, Wants, Haves),
      {ok, Acks} = srcd_pack:build_pkt(lists:concat([
        ["acknowledgments\n"],
        [lists:concat(["ACK ", Oid, "\n"]) || Oid <- Haves],
        ["ready\n", delim]
      ])),
      {ok, Acks ++ Packfile};
    false ->
      {ok, Acks} = srcd_pack:build_pkt(lists:concat([
        ["acknowledgments\n"],
        [ack_oid(Repo, Oid) || Oid <- Haves],
        [flush]
      ])),
      {next_state, read_command, Acks, Data}
  end.

has_all_oids(Repo, []) -> true;
has_all_oids(Repo, [Oid|Oids]) ->
  case srcd_repo:exists(Repo, Oid) of
    true -> has_all_oids(Repo, Oids);
    false -> false
  end.

ack_oid(Repo, Oid) ->
  Res = case srcd_repo:exists(Repo, Oid) of
    true -> "ACK";
    false -> "NAK"
  end,
  lists:concat([Res, " ", Oid, "\n"]).

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

-define(KNOWN_ARG(F,Str),
        F([[Str]|T], Res) -> F(T, [list_to_atom(Str)|Res]);
        F([[Str, Arg]|T], Res) -> F(T, [{list_to_atom(Str), Arg}|Res])
       ).

fetch_args(Args) -> fetch_args([string:split(Arg, " ") || Arg <- Args], []).
fetch_args([], Res) -> lists:reverse(Res);
?KNOWN_ARG(fetch_args, "want");
?KNOWN_ARG(fetch_args, "have");
?KNOWN_ARG(fetch_args, "done");
?KNOWN_ARG(fetch_args, "thin-pack");
?KNOWN_ARG(fetch_args, "ofs-delta");
?KNOWN_ARG(fetch_args, "no-progress");
?KNOWN_ARG(fetch_args, "shallow");
?KNOWN_ARG(fetch_args, "deepen");
?KNOWN_ARG(fetch_args, "deepen-relative");
?KNOWN_ARG(fetch_args, "deepen-since");
?KNOWN_ARG(fetch_args, "deepen-not");
?KNOWN_ARG(fetch_args, "filter");
?KNOWN_ARG(fetch_args, "want-ref");
?KNOWN_ARG(fetch_args, "sideband-all");
?KNOWN_ARG(fetch_args, "packfile-uris");
?KNOWN_ARG(fetch_args, "wait-for-done");
?KNOWN_ARG(fetch_args, "include-tag").

reflines_with_head(Refs, Repo, Symrefs) ->
  {ok, Ref} = srcd_repo:default_branch(Repo),
  ?LOG_NOTICE("default ref = ~p", [Ref]),
  case proplists:get_value(Ref, Refs) of
    undefined -> reflines(Refs);
    Oid -> reflines([{"HEAD", Oid, [{"symref-target", Ref}]} | Refs])
  end.
reflines(Refs) -> reflines(Refs, []).
reflines([], Res) -> lists:reverse(Res);
reflines([{Name, Commit} | Refs], Res) ->
  reflines([{Name, Commit, []} | Refs], Res);
reflines([{Name, Commit, []} | Refs], Res) ->
  reflines(Refs, [
    string:join([Commit, Name], " ") | Res
  ]);
reflines([{Name, Commit, Attrs} | Refs], Res) ->
  AttrStrs = [string:join([Attr, Val], ":") || {Attr, Val} <- Attrs],
  reflines(Refs, [
    string:join([Commit, Name] ++ AttrStrs, " ") | Res
  ]).

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
