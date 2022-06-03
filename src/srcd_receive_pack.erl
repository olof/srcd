-module(srcd_receive_pack).
-export([callback_mode/0, init/2]).
-export([advertise/1, wait_for_input/1, read_packfile/1,
         verify_packfile/1, process_cmds/1, process_lines/1]).
-include_lib("kernel/include/logger.hrl").

-record(?MODULE, {repo, version=0}).

callback_mode() -> state_functions.

caps() -> srcd_ssh:caps() ++ ['side-band-64k'].

init(Version, [Repo]) ->
  {ok, advertise, #?MODULE{repo=Repo, version=Version}}.

advertise(#?MODULE{repo=Repo, version=Version} = Data) ->
  case advertisement(Repo, Version) of
    {ok, Resp} ->
      ?LOG_NOTICE("Resp: ~p", [Resp]),
      {next_state, wait_for_input, Resp, Data};
    {error, Error} -> {error, Error}
  end.

wait_for_input(Data) -> wait_for_input(Data, [], []).
wait_for_input(Data, Res, Caps0) ->
  case srcd_pack:read_line() of
    flush -> {next_state, process_lines, {Data, lists:reverse(Res),
                                                lists:reverse(Caps0)}};
    {data, Line} ->
      case Caps0 of
        [] -> [Line1, Caps] = string:split(Line, "\0"),
              wait_for_input(Data, [Line1|Res], parse_caps(Caps));
        _ -> wait_for_input(Data, [Line|Res], Caps0)
      end
  end.

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
parse_cap(["quiet"]) -> quiet;
parse_cap([[]]) -> skip.

process_lines({#?MODULE{repo=Repo} = Data, [], Caps}) ->
  {error, not_implemented};
process_lines({#?MODULE{repo=Repo} = Data, Lines, Caps}) ->
  Cmds = [parse_line(Line, Caps) || Line <- Lines],
  Status = check_cmds(Repo, Cmds, Caps),
  case Status of
    ok ->
      {ok, Flush} = srcd_pack:build_pkt([flush]),
      {next_state, read_packfile, Flush, {Data, Cmds}}
  end.

read_packfile({Data, Cmds}) ->
  {ok, Packfile} = srcd_packfile:read(),
  {next_state, verify_packfile, {Data, Cmds, Packfile}}.

verify_packfile({#?MODULE{repo=Repo} = Data, Cmds, Packfile}) ->
  Oids = srcd_packfile:object_ids(Packfile),
  Deps = srcd_packfile:object_deps(Packfile),
  Unresolved = [D || D <- Deps, not lists:member(D, Oids)],
  Missing = [D || D <- Unresolved, not srcd_repo:exists(Repo, D)],
  case Missing of
    [] -> {next_state, process_cmds, {Data, Cmds, Packfile}};
    Oids -> {error, "incomplete packfile"}
  end.

process_cmds({#?MODULE{repo=Repo} = Data, Cmds, Packfile}) ->
  srcd_repo:write(Repo, Cmds, Packfile).

check_cmds(Repo, [], Caps) -> ok;
check_cmds(Repo, [Cmd|Cmds], Caps) ->
  case check_cmd(Repo, Caps, Cmd) of
    ok -> check_cmds(Repo, Cmds, Caps);
    {error, Reason} -> {error, invalid_cmd, Cmd, Reason}
  end.

check_cmd(Repo, Caps, {create, Name, _}) -> ok;
check_cmd(Repo, Caps, {delete, Name, Hash}) ->
  case is_expected_oid(Repo, Name, Hash) of
    true -> ok;
    false -> error
  end;
check_cmd(Repo, Caps, {update, Name, {Old, _}}) ->
  case is_expected_oid(Repo, Name, Old) of
    true -> ok;
    false -> error
  end.

is_expected_oid(Repo, Name, Oid) ->
  srcd_repo:head(Repo, Name) =:= Oid.

-define(OID_ZERO, "0000000000000000000000000000000000000000").

parse_line(Line, Caps) ->
  case string:split(Line, " ", all) of
    [?OID_ZERO, New, Name] -> {create, Name, New};
    [Old, ?OID_ZERO, Name] -> {delete, Name, Old};
    [Old, New, Name] -> {update, Name, {Old, New}}
  end.

advertisement(Repo, Version) ->
  Refs = case srcd_repo:refs(Repo) of
    {ok, []}    -> [{"capabilities^{}", ?OID_ZERO}];
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
