-module(srcd_pack).
-export([advertisement/3]).

-include_lib("kernel/include/logger.hrl").

pkt_line(flush) -> "0000";
pkt_line(Line) ->
  lists:concat([io_lib:format("~4.16.0b", [length(Line) + 4]), Line]).

no_such_repo(Repo) ->
  ?LOG_NOTICE("requested repo ~p does not exist", [Repo]),
  {error, "no such repo\n"}.

ref_val(Refs, Ref) ->
  proplists:get_value(Ref, Refs).

refs_to_reflines(Refs) ->
  [string:join([Commit, Name], " ") || {Name, Commit} <- Refs].

advertisement(Version, [], _) ->
  build_pkt(prepend_version(Version, [flush]));
advertisement(Version, Refs, []) ->
  build_pkt(prepend_version(Version, refs_to_reflines(Refs) ++ [flush]));
advertisement(Version, Refs, Caps) ->
  [Top | RefLines] = refs_to_reflines(Refs),
  CapLine = string:join([Top, [0], capstring(Caps)], ""),
  build_pkt(prepend_version(Version, [CapLine | RefLines ++ [flush]])).

prepend_version(0, Pkt) -> Pkt;
prepend_version(1, Pkt) -> ["version 1\n"|Pkt];
prepend_version(2, Pkt) -> ["version 2\n"|Pkt].

build_pkt(Lines) ->
  {ok, [pkt_line(Line) || Line <- Lines]}.

capstring(Caps) -> string:join([capability(C) || C <- Caps], " ").
capability({Key, Value}) when is_atom(Key) ->
  capability({atom_to_list(Key), Value});
capability({Key, Value}) when is_atom(Value) ->
  capability({Key, atom_to_list(Value)});
capability({Key, Value}) ->
  lists:concat([Key, "=", Value]);
capability(Key) when is_atom(Key) -> capability(atom_to_list(Key));
capability(Key) -> Key.
