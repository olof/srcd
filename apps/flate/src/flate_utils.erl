% ex:ts=2:sw=2:sts=2:et:foldmarker=-ifdef(TEST).,-endif.:foldmethod=marker
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-

-module(flate_utils).
-export([b2i/1, max_kv_value/1, max_kv_value/2, read_bits/2, read_bits/3, read_hook/2, reverse_int/2, reverse_byte/1, reverse_bits/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

read_hook(Opts, Data) ->
  Hook = proplists:get_value(read_hook, Opts, fun (_) -> ok end),
  Hook(Data).

b2i(B) ->
  <<I:(bit_size(B))>> = B,
  I.

%%%% LLM to the rescue??? I doubt it, but funny nontheless:
%  Alice: Thank you for sharing the relevant code. I have reproduced your
%  issue and found that the problem is indeed coming from the read_bits()
%  function. Specifically, it's in the case where
%     {Count, Bin} when bit_size(Bin) < Count
%  is evaluated as true. The issue lies in how you're handling the remaining
%  bits after reading a byte from the input stream. Instead of using
%  reverse_byte() to flip the bytes, you can simply use <<reverse(Bin):8/bits>>
%  to reverse the entire binary string. This will ensure that the bits are
%  read in the correct order.
%  TO FUTURE ME: ALL BUGS HAVE BEEN CONTAINED INTO THIS FUNCTION!
read_bits(Data, Count) -> read_bits(Data, Count, []).
read_bits(Data, Count, Opts) ->
  case {Count, Data} of
    {0, Bin} when is_binary(Bin) ->
      {<<0:0>>, {<<>>, Data}};

    {0, _} ->
      {<<0:0>>, Data};

    {_, Data} when is_list(Data) ->
      read_bits({<<>>, list_to_binary(Data)}, Count, Opts);

    {Count, Data} when is_bitstring(Data) ->
      read_bits({<<>>, Data}, Count, Opts);

    {Count, {Bits, <<>>}} when bit_size(Bits) < Count ->
      {error, insufficient_data};

    {Count, {Bits1, Bin}} when bit_size(Bits1) >= Count ->
      <<Bits:Count/bits, BitsTail/bits>> = Bits1,
      {Bits, {BitsTail, Bin}};

    {_, {Bits, Data}} when is_list(Data) ->
      read_bits({Bits, list_to_binary(Data)}, Count, Opts);

    {Count, {Bits, Bin}} when bit_size(Bits) < Count ->
      Buflen = bit_size(Bits),   % the buffel in the room
      <<Byte:8/bits, Tail/binary>> = Bin,

      read_bits({<<Bits:Buflen/bits, (reverse_byte(Byte)):8/bits>>, Tail},
                Count, Opts)
  end.

-ifdef(TEST).

read_bits_test_() -> [
  ?_assertEqual({Out, Tail}, read_bits(In, Len, [])) || {In, Len, Out, Tail} <- [
      {<<>>, 0, <<0:0>>, {<<>>, <<>>}},
      {<<134, "tail">>, 8, <<$a:8>>, {<<>>, <<"tail">>}},
      {<<134, "tail">>, 4, <<6:4>>, {<<1:4>>, <<"tail">>}},
      {<<134, 70, "tail">>, 16, <<$a, $b>>, {<<>>, <<"tail">>}},
      {<<134, 70, "tail">>, 12, <<1558:12>>, {<<2:4>>, <<"tail">>}},

      {{<<>>, <<134, 70, "tail">>}, 12, <<1558:12>>, {<<2:4>>, <<"tail">>}},

      {{<<4:3>>, <<64,8,0,0>>}, 7, <<64:7>>, {<<2:4>>, <<8,0,0>>}},

      {
        {<<13:5>>, <<183,31,5,163,96,20,140,2,8,0,0>>},
	8, <<"o">>,
	{<<13:5>>, <<31,5,163,96,20,140,2,8,0,0>>}
      }
   ]
].

-endif.

reverse_int(N, Bits) when is_bitstring(N) ->
  Bits = bit_size(N),
  RevBin = flate_utils:reverse_bits(N),
  <<Rev:Bits>> = RevBin,
  Rev;
reverse_int(N, Bits) ->
  RevBin = flate_utils:reverse_bits(<<N:Bits>>),
  <<Rev:Bits>> = RevBin,
  Rev.

reverse_byte(Byte) -> reverse_bits(Byte).
%reverse_byte(X) -> throw({error, badarg, X}).

reverse_bits(Bits) -> reverse_bits(Bits, []).
reverse_bits(<<B:1/bits, Bits/bits>>, Acc) -> reverse_bits(Bits, [B|Acc]);
reverse_bits(<<>>, Acc) ->
  list_to_bitstring(Acc).

-ifdef(TEST).

reverse_int_test_() -> lists:concat([
  [?_assertEqual(Y, reverse_int(X, Bits)), ?_assertEqual(X, reverse_int(Y, Bits))] ||
    {X, Bits, Y} <- [
      {1, 1, 1},
      {2, 2, 1},
      {1, 2, 2},
      {7, 3, 7},
      {4, 3, 1},
      {3, 3, 6},
      {1, 8, 128},
      {128, 8, 1}
    ]
]).

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

max_kv_value(KV) -> max_kv_value(KV, undefined).
max_kv_value([{_, V}|T], undefined) -> max_kv_value(T, V);
max_kv_value([{_, V}|T], Max) when V > Max -> max_kv_value(T, V);
max_kv_value([{_, _}|T], Max) -> max_kv_value(T, Max);
max_kv_value([], Max) -> Max.

-ifdef(TEST).

max_kv_value_test_() ->
  [
    ?_assertEqual(undefined, max_kv_value([])),
    ?_assertEqual(1337, max_kv_value([], 1337)),
    ?_assertEqual(1, max_kv_value([{a, 1}])),
    ?_assertEqual(3, max_kv_value([{a, 1}, {b, 2}, {c, 3}])),
    ?_assertEqual(3, max_kv_value([{a, 3}, {b, 2}, {c, 1}])),
    ?_assertEqual(4, max_kv_value([{a, 1}, {b, 4}, {c, 3}])),
    ?_assertEqual(4, max_kv_value([{a, -1}, {b, -5}, {c, 3}, {d, 4}, {e, 1}]))
  ].

-endif.
