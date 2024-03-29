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
  build_packfile/1,
  wait_for_input/1,
  ls_refs/1
]).

-include("srcd_session.hrl").
-ifdef(TEST).
-include("tests/upload_pack.trl").
-endif.

caps() -> srcd_ssh:caps() ++ [].

init(Version, Args) ->
  {ok, {Repo, Opts}} = parse_args(Args),
  Data = #session{repo=Repo, version=Version, opts=Opts},
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

wait_for_input(Data = #session{}) -> wait_for_input(Data, [], []).
wait_for_input(Data = #session{}, Res, Caps0) ->
  case srcd_pack:read_line() of
    flush -> {next_state, process_lines, {Data#session{caps=lists:reverse(Caps0)},
                                          lists:reverse(Res)}};
    {data, Line2} ->
      Line1 = string:trim(Line2),
      {Line, Caps} = capture_caps(Line1, []),
      wait_for_input(Data, [parse_line(Line) | Res], Caps)
  end.

process_lines({Session = #session{}, Args}) ->
  Wants = proplists:get_all_values(want, Args),
  Haves = proplists:get_all_values(have, Args),
  case proplists:get_bool(done, Args) of
    true -> {next_state, build_packfile, {Session, Wants, Haves}};
    false -> fetch_ack(Session, Wants, Haves)
  end.

parse_line("done") -> done;
parse_line(Line) ->
  case string:prefix(Line, "want ") of
    nomatch -> Line;
    Oid -> {want, Oid}
  end.

build_packfile({#session{repo=Repo} = Session, Wants, Haves}) ->
  ?LOG_NOTICE("Creating pack on ~p:~n    Haves: ~p~n    Wants: ~p",
              [Repo, Haves, Wants]),
  % TODO: In default v0 mode, we don't even length prefix the
  % packfile like we usually do. If we use side-bands, we do
  % length prefix it. In v2, the packfile is always side-banded.
  % Meaning: we have something that works right now, but we
  % should add support for the optional side-band capabilities.
  {ok, Packfile} = srcd_packfile:build(Repo, Haves, Wants).

fetch_ack(#session{repo=Repo} = Session, Wants, Haves) ->
  case Haves of
    [] ->
      {ok, Acks} = srcd_pack:build_pkt(["NAK\n"]),
      ?LOG_NOTICE("nak sent for initial clone, going for it"),
      {next_state, build_packfile, Acks, {Session, Wants, Haves}};
    Haves ->
      {ok, Acks} = srcd_pack:build_pkt(lists:concat([
        [ack_oid(Repo, Oid) || Oid <- Wants],
        [flush]
      ])),
      {next_state, read_command, Acks, Session}
  end.

has_all_oids(Repo, []) -> true;
has_all_oids(Repo, [Oid | Oids]) ->
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
alt_delims(Line0, [Delim | Alts]) ->
  case string:split(Line0, Delim) of
    [Line0] -> alt_delims(Line0, Alts);
    [Line, Caps] -> {Line, Caps}
  end.

capture_caps(Line1, []) ->
  case string:prefix(Line1, "want ") of
    nomatch -> {Line1, []};
    Line0 -> case alt_delims(Line0, ["\0", " "]) of
      {Line, Caps} -> {"want " ++ Line, parse_caps(Caps)};
      nomatch -> {"want " ++ Line0, []}
    end
  end;
capture_caps(Line, Caps) -> {Line, Caps}.

parse_caps(Caps) -> filter_caps(string:split(Caps, " ", all), []).
filter_caps([], Res) -> lists:reverse(Res);
filter_caps([Cap0 | Caps], Res) ->
  case parse_cap(string:split(Cap0, "=")) of
    skip -> filter_caps(Caps, Res);
    Cap -> filter_caps(Caps, [Cap | Res])
  end.
parse_cap(["agent", UA]) -> {agent, UA};
parse_cap(["object-format", Hash]) -> {object_format, Hash};
parse_cap(["side-band-64k"]) -> 'side-band-64k';
parse_cap([[]]) -> skip;
parse_cap(Cap) ->
  ?LOG_NOTICE("Unknown capability requested: ~p", [Cap]),
  skip.

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

advertise(#session{repo=Repo, version=Version, opts=Opts} = Data) ->
  case srcd_repo:refs(Repo) of
    {ok, Refs} ->
      Reflines = reflines_with_head(Refs, Repo, false),
      {ok, Adv} = srcd_pack:build_pkt(Reflines ++ [flush]),
      case proplists:get_value(advertise_refs, Opts) of
        true -> {ok, Adv};
        _ -> {next_state, wait_for_input, Adv, Data}
      end;
    {error, no_such_repo} ->
      {error, "No such repo~n"}
  end.

read_command(#session{repo=Repo} = Data) ->
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
ls_ref_args([Arg | Args], Res) ->
  [Cmd | MaybeArgArgs] = string:split(Arg, " "),
  ls_ref_args(Args, case Cmd of
    "peel" -> [peel | Res];
    "symrefs" -> [symrefs | Res];
    "ref-prefix" ->
      [ArgArgs] = MaybeArgArgs,
      [{prefix, ArgArgs} | Res]
  end).

ls_refs({#session{repo=Repo} = Data, Caps, Args}) ->
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
    {error, no_such_repo} ->
      {error, "No such repo~n"}
  end.

match_any_prefix([], Term) -> false;
match_any_prefix([Prefix | Prefixes], Term) ->
  case string:prefix(Term, Prefix) of
    nomatch -> match_any_prefix(Prefixes, Term);
    _ -> true
end.

parse_args(Args) -> parse_args(Args, []).
parse_args([Repo], Res) -> {ok, {Repo, Res}};
parse_args(["--strict" | Args], Res) ->
  parse_args(Args, [strict | Res]);
parse_args(["--stateless-rpc" | Args], Res) ->
  parse_args(Args, [stateless_rpc | Res]);
parse_args(["--advertise-refs" | Args], Res) ->
  parse_args(Args, [advertise_refs | Res]).
