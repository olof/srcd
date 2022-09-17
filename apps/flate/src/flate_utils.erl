-module(flate_utils).
-export([read_bits/2, read_bits/3, read_hook/2, reverse_int/2, reverse_byte/1, reverse_bits/1]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

read_hook(Opts, Data) ->
  Hook = proplists:get_value(read_hook, Opts, fun (_) -> ok end),
  Hook(Data).

read_bits(Data, Count) ->
  read_bits(Data, Count, []).
read_bits(Data, Count, Opts) ->
  case {Count, Data} of
    {0, Bin} when is_binary(Bin) -> {0, {<<>>, Data}};
    {0, _} -> {0, Data};

    {Count, Data} when is_bitstring(Data) ->
      read_bits({<<>>, Data}, Count, Opts);

    {Count, {Bits, <<>>}} when bit_size(Bits) < Count ->
      {error, insufficient_data};

    {Count, {Bits1, Bin}} when bit_size(Bits1) >= Count ->
      <<Bits:Count, BitsTail/bits>> = Bits1,
      {Bits, {BitsTail, Bin}};

    {Count, {Bits, Bin}} when bit_size(Bits) < Count ->
      read_bits(fill_bits(Bits, Bin, Opts), Count, Opts)
  end.

fill_bits(Bits, Bytes, Opts) when is_binary(Bytes) ->
   <<Byte:8/bits, Tail/binary>> = Bytes,
   read_hook(Opts, Byte),
   Rev = case proplists:get_bool(reverse_input_byte_order, Opts) of
     true -> reverse_byte(Byte);
     false -> Byte
   end,
   {<<Bits:(bit_size(Bits))/bits, Rev:8/bits>>, Tail};
fill_bits(Bits, Stream, Opts) when is_port(Stream) ->
  [Byte] = io:get_chars(Stream, "", 1),
  read_hook(Opts, <<Byte:8>>),
  Rev = case proplists:get_bool(reverse_input_byte_order, Opts) of
    true -> reverse_byte(Byte);
    false -> Byte
  end,
  {<<Bits:(bit_size(Bits))/bits, Rev:8>>, Stream}.

-ifdef(TEST).

read_bits_test_() -> [
  ?_assertEqual({Out, Tail}, read_bits(In, Len, Opts)) || {In, Len, Opts, Out, Tail} <- [
      {<<>>, 0, [], 0, {<<>>, <<>>}},
      {<<"abc">>, 8, [], $a, {<<>>, <<"bc">>}},
      {<<"abc">>, 4, [], 6, {<<1:4>>, <<"bc">>}},
      {<<"abc">>, 12, [], 1558, {<<2:4>>, <<"c">>}},

      {{<<>>, <<"abc">>}, 12, [], 1558, {<<2:4>>, <<"c">>}},
      {{<<4:3>>, <<2,8,0,0>>}, 7, [], 64, {<<2:4>>, <<8,0,0>>}},
      {
        {<<13:5>>, <<183,31,5,163,96,20,140,2,8,0,0>>},
        8, [reverse_input_byte_order], 111,
        {<<13:5>>, <<31,5,163,96,20,140,2,8,0,0>>}
      }
   ]
].

-endif.

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
