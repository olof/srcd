% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_pack).
-export([
  advertisement/3,
  build_pkt/1,
  line_split/1,
  line_split/2,
  line_split_ch/2,
  line_split_ch/3,
  max_data_len/0,
  read_line/0,
  read_command/0
]).
-export([reflines/1, reflines_with_head/2]).

-include_lib("kernel/include/logger.hrl").

-define(MAX_PACKET_DATA_LEN, 65516).

-ifdef(TEST).
-include("tests/pack.trl").
-endif.

max_data_len() -> ?MAX_PACKET_DATA_LEN.

line_split(Str) -> line_split(?MAX_PACKET_DATA_LEN, Str).
line_split(Len, Str) -> line_split(Len, Str, []).
line_split(Len, [], Acc) -> lists:reverse(Acc);
line_split(Len, Str, Acc) when length(Str) < Len -> lists:reverse([Str | Acc]);
line_split(Len, Str, Acc) ->
  {Head, Tail} = lists:split(Len, Str),
  line_split(Len, Tail, [Head | Acc]).

line_split_ch(Ch, Str) -> line_split_ch(?MAX_PACKET_DATA_LEN, Ch, Str).
line_split_ch(Len, Ch, Str) -> [[Ch | Line] || Line <- line_split(Len-1, Str)].

pkt_line(flush) -> "0000";
pkt_line(delim) -> "0001";
pkt_line(Line) when length(Line) =< ?MAX_PACKET_DATA_LEN ->
  [io_lib:format("~4.16.0b", [length(Line) + 4]), Line].

no_such_repo(Repo) ->
  ?LOG_NOTICE("requested repo ~p does not exist", [Repo]),
  {error, "no such repo\n"}.

ref_val(Refs, Ref) -> proplists:get_value(Ref, Refs).

reflines_with_head(Refs, Repo) ->
  {ok, Ref} = srcd_repo:default_branch(Repo),
  ?LOG_NOTICE("default ref = ~p", [Ref]),
  case proplists:get_value(Ref, Refs) of
    undefined -> reflines(Refs);
    Oid -> reflines([{"HEAD", Oid} | Refs])
  end.
reflines(Refs) ->
  [string:join([Commit, Name], " ") || {Name, Commit} <- Refs].

advertisement(Version, [], _) -> build_pkt(prepend_version(Version, [flush]));
advertisement(Version, Refs, []) ->
  build_pkt(prepend_version(Version, reflines(Refs) ++ [flush]));
advertisement(Version, Refs, Caps) ->
  [Top | RefLines] = reflines(Refs),
  CapLine = string:join([Top, [0], capstring(Caps)], ""),
  build_pkt(prepend_version(Version, [CapLine | (RefLines ++ [flush])])).

prepend_version(0, Pkt) -> Pkt;
prepend_version(1, Pkt) -> ["version 1\n" | Pkt];
prepend_version(2, Pkt) -> ["version 2\n" | Pkt].

build_pkt(Lines) ->
  {ok, lists:flatten([pkt_line(Line) || Line <- Lines])}.

capstring(Caps) -> string:join([capability(C) || C <- Caps], " ").
capability({Key, Value}) when is_atom(Key) ->
  capability({atom_to_list(Key), Value});
capability({Key, Value}) when is_atom(Value) ->
  capability({Key, atom_to_list(Value)});
capability({Key, Value}) ->
  lists:concat([Key, "=", Value]);
capability(Key) when is_atom(Key) -> capability(atom_to_list(Key));
capability(Key) -> Key.

read_line_length() -> list_to_integer(srcd_utils:read(4), 16) - 4.

read_line() ->
  case read_line_length() of
    -4 -> flush;
    -3 -> delim;
    Len when Len >= 0 -> {data, srcd_utils:read(Len)};
    T -> ?LOG_NOTICE("unknown pkt type: ~p", [T]), error
  end.

read_command() ->
  case read_line() of
    {data, Input} ->
      ["command", Command] = string:split(Input, "="),
      read_command(string:trim(Command), [], [], false);
    Line -> Line
  end.
read_command(Cmd, Caps, Args, DelimSeen) ->
  case read_line() of
    flush -> {Cmd, Caps, lists:reverse(Args)};
    delim -> read_command(Cmd, lists:reverse(Caps), Args, true);
    {data, Input} -> case DelimSeen of
      true -> read_command(Cmd, Caps, [Input | Args], DelimSeen);
      _ -> read_command(Cmd, [parse_cap(Input) | Caps], Args, DelimSeen)
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
