% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_utils).

-export([cmd_split/1, hex_to_int/1, bin_to_hex/1, bytes_to_hex/1,
         hex_to_bin_sha1/1, pipe/2, read/1, read/2, read/3,
         read_u32/0, read_u32/1, read_u32/2, iolist_to_list/1]).

-ifdef(TEST).
-include("tests/utils.trl").
-endif.

hex_to_int(Hex) ->
  {ok, [N], []} = io_lib:fread("~16u", Hex),
  N.

hex_to_bin_sha1(Hex) when length(Hex) =:= 40 ->
  N = hex_to_int(Hex),
  binary_to_list(<<N:160>>).

byte_to_hex(Byte) when Byte >= 0 andalso Byte < 256 ->
  N = integer_to_list(Byte, 16),
  string:lowercase(case length(N)  of
    1 -> [$0 | N];
    2 -> N
  end).

bin_to_hex(Bytes) ->
  bytes_to_hex(binary_to_list(Bytes)).

bytes_to_hex(Bytes) ->
  lists:concat([byte_to_hex(Byte) || Byte <- Bytes]).

cmd_split(Cmd) ->
  cmd_split(Cmd, "", false, false, []).

cmd_split([], This, false, false, Res) ->
  {ok, lists:reverse([lists:reverse(This) | Res])};
cmd_split([], _, T, _, _) when T =/= false ->
  error;
cmd_split([], _, _, true, _) ->
  error;
cmd_split([$\  | Cmd], This, false, false, Res) ->
  cmd_split(Cmd, "", false, false, [lists:reverse(This) | Res]);
cmd_split([$\n | Cmd], This, false, false, Res) ->
  cmd_split(Cmd, "", false, false, [lists:reverse(This) | Res]);
cmd_split([$' | Cmd], This, false, false, Res) ->
  cmd_split(Cmd, This, $', false, Res);
cmd_split([$' | Cmd], This, $', false, Res) ->
  cmd_split(Cmd, This, false, false, Res);
cmd_split([$" | Cmd], This, false, false, Res) ->
  cmd_split(Cmd, This, $", false, Res);
cmd_split([$" | Cmd], This, $", false, Res) ->
  cmd_split(Cmd, This, false, false, Res);
cmd_split([$\\ | Cmd], This, Q, false, Res) ->
  cmd_split(Cmd, This, Q, true, Res);
cmd_split([Ch | Cmd], This, Q, true, Res) ->
  cmd_split(Cmd, [Ch | This], Q, false, Res);
cmd_split([Ch | Cmd], This, Q, false, Res) ->
  cmd_split(Cmd, [Ch | This], Q, false, Res).

pipe(State, []) -> {ok, State};
pipe(State, [Step | Steps]) ->
  case Step(State) of
    {error, Err} -> {error, Err};
    {ok, NewState} -> pipe(NewState, Steps)
  end.

read(Len) -> read(standard_io, Len).
read(IoDevice, Len) when is_integer(Len) -> io:get_chars(IoDevice, "", Len);
read(Len, Digest) when is_integer(Len) -> read(standard_io, Len, Digest).
read(IoDevice, Len, Digest) ->
  Bytes = read(IoDevice, Len),
  {Bytes, crypto:hash_update(Digest, Bytes)}.

read_u32() -> read_u32(standard_io).
read_u32(IoDevice) when is_pid(IoDevice) ->
  Bytes = srcd_utils:read(4),
  <<N:32>> = list_to_binary(Bytes),
  N;
read_u32(Digest) -> read_u32(standard_io, Digest).
read_u32(IoDevice, Digest) ->
  {Bytes, D} = srcd_utils:read(IoDevice, 4, Digest),
  <<N:32>> = list_to_binary(Bytes),
  {N, D}.

iolist_to_list(IoList) ->
  binary_to_list(iolist_to_binary(IoList)).
