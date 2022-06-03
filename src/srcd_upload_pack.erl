% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_upload_pack).
-behavior(srcd_cmd).
-export([init/2, caps/0]).
-export([
  advertise/1,
  handshake/1,
  read_command/1,
  process_command/1,
  process_lines/1,
  wait_for_input/1,
  ls_refs/1
]).

-include("srcd.hrl").

caps() -> srcd_ssh:caps() ++ [].

init(Version, Args) ->
  {ok, {Repo, Opts}} = parse_args(Args),
  Data = #pack_repo{repo=Repo, version=Version, opts=Opts},
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

wait_for_input(Data) -> wait_for_input(Data, [], []).
wait_for_input(Data, Res, Caps0) ->
  case srcd_pack:read_line() of
    flush -> {next_state, process_lines, {Data,
                                          lists:reverse(Caps0),
                                          lists:reverse(Res)}};
    {data, Line2} ->
      Line1 = string:trim(Line2),
      ?LOG_NOTICE("Got line: ~p", [Line1]),
      {Line, Caps} = capture_caps(Line1, []),
      ?LOG_NOTICE("Got line [~p]: ~p", [Caps, Line]),
      ?LOG_NOTICE("Got line [~p]: ~p", [Caps, parse_line(Line)]),
      wait_for_input(Data, [parse_line(Line)|Res], Caps)
  end.

process_lines({#pack_repo{repo=Repo} = Data, Caps, Args}) ->
  ?LOG_NOTICE("Client args ~p", [Args]),
  Wants = proplists:get_all_values(want, Args),
  ?LOG_NOTICE("Client wants ~p", [Wants]),
  Haves = proplists:get_all_values(have, Args),
  ?LOG_NOTICE("Client has ~p", [Haves]),
  case proplists:get_bool(done, Args) of
    true -> fetch_packfile(Repo, Wants, Haves);
    false -> fetch_ack(Data, Repo, Wants, Haves)
  end.

parse_line("done") -> done;
parse_line(Line) ->
  ?LOG_NOTICE("parse_line: ~p", [Line]),
  ?LOG_NOTICE("parse_line: ~p", [string:prefix(Line, "want ")]),
  case string:prefix(Line, "want ") of
    nomatch -> Line;
    Oid -> {want, Oid}
  end.

fetch_packfile(Repo, Wants, Haves) ->
  srcd_packfile:build(Repo, Wants).

fetch_ack(Data, Repo, Wants, Haves) ->
  case Haves of
    [] ->
      {ok, Packfile} = fetch_packfile(Repo, Wants, Haves),
      {ok, Acks} = srcd_pack:build_pkt(["NAK\n"]),
      ?LOG_NOTICE("nak sent for initial clone, going for it"),
      {ok, Acks ++ Packfile};
    Haves ->
      {ok, Acks} = srcd_pack:build_pkt(lists:concat([
        [ack_oid(Repo, Oid) || Oid <- Wants],
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

alt_delims(Line0, []) -> nomatch;
alt_delims(Line0, [Delim|Alts]) ->
  case string:split(Line0, Delim) of
    [Line0] -> alt_delims(Line0, Alts);
    [Line, Caps] -> {Line, Caps}
  end.

capture_caps(Line1, []) ->
  case string:prefix(Line1, "want ") of
    nomatch -> Line1;
    Line0 -> case alt_delims(Line0, ["\0", " "]) of
      {Line, Caps} -> {"want " ++ Line, parse_caps(Caps)};
      nomatch -> {"want " ++ Line0, []}
    end
  end;
capture_caps(Line, Caps) -> {Line, Caps}.

parse_caps(Caps) -> filter_caps(string:split(Caps, " ", all), []).
filter_caps([], Res) -> lists:reverse(Res);
filter_caps([Cap0|Caps], Res) ->
  case parse_cap(string:split(Cap0, "=")) of
    skip -> filter_caps(Caps, Res);
    Cap -> filter_caps(Caps, [Cap|Res])
  end.
parse_cap(["agent", UA]) -> {agent, UA};
parse_cap(["object-format", Hash]) -> {object_format, Hash};
parse_cap(["side-band-64k"]) -> 'side-band-64k';
parse_cap([[]]) -> skip.

reflines_with_head(Refs, Repo, Symrefs) ->
  {ok, Ref} = srcd_repo:default_branch(Repo),
  ?LOG_NOTICE("default ref = ~p", [Ref]),
  case proplists:get_value(Ref, Refs) of
    undefined -> reflines(Refs);
    Oid -> reflines([{"HEAD", Oid} | Refs])
  end.
reflines(Refs) -> reflines(Refs, []).
reflines([], Res) -> lists:reverse(Res);
reflines([{Name, Commit} | Refs], Res) ->
  reflines(Refs, [
    string:join([Commit, Name], " ") | Res
  ]).

advertise(#pack_repo{repo=Repo, version=Version, opts=Opts} = Data) ->
  case srcd_repo:refs(Repo) of
    {ok, Refs} ->
      Reflines = reflines_with_head(Refs, Repo, false),
      {ok, Adv} = srcd_pack:build_pkt(Reflines ++ [flush]),
      case proplists:get_value(advertise_refs, Opts) of
        true -> {ok, Adv};
        _ -> {next_state, wait_for_input, Adv, Data}
      end;
    {error, enoent} ->
      {error, "No such repo\n"}
  end.

read_command(#pack_repo{repo=Repo} = Data) ->
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

ls_refs({#pack_repo{repo=Repo} = Data, Caps, Args}) ->
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
