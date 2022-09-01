-module(flate_utils).
-export([reverse_byte/1, reverse_bits/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

reverse_byte(Byte) -> reverse_bits(Byte).
%reverse_byte(X) -> throw({error, badarg, X}).

reverse_bits(Bits) -> reverse_bits(Bits, []).
reverse_bits(<<B:1/bits, Bits/bits>>, Acc) -> reverse_bits(Bits, [B|Acc]);
reverse_bits(<<>>, Acc) ->
  list_to_bitstring(Acc).

-ifdef(TEST).

reverse_bits_test_() -> lists:concat([
  [?_assertEqual(X, reverse_bits(Y)), ?_assertEqual(Y, reverse_bits(X))] ||
    {X, Y} <- [
      {<<0:3>>, <<0:3>>},
      {<<1:3>>, <<4:3>>},
      {<<1:8>>, <<128>>}
    ]
]).

reverse_byte_test_() -> lists:concat([
  [?_assertEqual(X, reverse_byte(Y)), ?_assertEqual(Y, reverse_byte(X))] ||
    {X, Y} <- [
      {<<0>>, <<0>>},
      {<<1>>, <<128>>},
      {<<3>>, <<192>>},
      {<<75>>, <<210>>}
    ]
]).

-endif.
