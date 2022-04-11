-module(gitd_pack).
-export([upload_pack/1, upload_pack/2, receive_pack/1]).

-include_lib("kernel/include/logger.hrl").
-define(CAPABILITIES, [
  atomic,
  quiet,
  {'object-format', sha1},
  {agent, "ethup/0"}
]).

pkt_line(flush) -> "0000";
pkt_line(Line) ->
  lists:concat([io_lib:format("~4.16.0b", [length(Line) + 4]), Line]).

no_such_repo(Repo) ->
  ?LOG_NOTICE("requested repo ~p does not exist", [Repo]),
  {error, "no such repo\n"}.

upload_pack(Repo) -> upload_pack(Repo, []).
upload_pack(Repo, Opts) ->
  Advertise = proplists:get_value(advertise_refs, Opts, false),
  upload_pack(Repo, Opts, Advertise).

upload_pack(Repo, Opts, true) ->
  case gitd_repo:refs(Repo) of
    {ok, Refs} -> encode_refs(Refs);
    {error, enoent} -> no_such_repo(Repo)
  end;
upload_pack(Repo, Opts, false) ->
  not_implemented.

receive_pack(Repo) ->
  case gitd_repo:refs(Repo) of
    {ok, []} ->
      encode_refs([
        {"capabilities^{}", "0000000000000000000000000000000000000000"}
      ]);
    {ok, Refs0} ->
      {ok, Head} = gitd_repo:default_branch(Repo),
      Refs = case proplists:get_value(Head, Refs0) of
        undefined -> Refs0;
        Commit -> [{"HEAD", Commit} | Refs0]
      end,
      {ok, encode_refs(Refs)};
    {error, enoent} ->
      no_such_repo(Repo)
  end.

ref_val(Refs, Ref) ->
  proplists:get_value(Ref, Refs).

encode_refs([]) -> build_pkt([flush]);
encode_refs(Refs) ->
  [Top | RefLines] = [string:join([Commit, Name], " ") || {Name, Commit} <- Refs],
  CapLine = string:join([Top, [0], capabilities()], ""),
  build_pkt([CapLine | RefLines ++ [flush]]).

build_pkt(Lines) ->
  {ok, [pkt_line(Line) || Line <- Lines]}.

capabilities() -> string:join([capability(C) || C <- ?CAPABILITIES], " ").
capability({Key, Value}) when is_atom(Key) ->
  capability({atom_to_list(Key), Value});
capability({Key, Value}) when is_atom(Value) ->
  capability({Key, atom_to_list(Value)});
capability({Key, Value}) ->
  lists:concat([Key, "=", Value]);
capability(Key) when is_atom(Key) -> capability(atom_to_list(Key));
capability(Key) -> Key.
