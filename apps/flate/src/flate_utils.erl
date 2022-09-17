-module(flate_utils).
-export([read_bits/2, read_bits/3, reverse_int/2, reverse_byte/1, reverse_bits/1]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

b2i(B) ->
  <<I:(bit_size(B))>> = B,
  I.

read_bits(Data, Count) ->
  read_bits(Data, Count, []).
read_bits(Data, Count, Opts) ->
  ?LOG_NOTICE("X?"),
  case {Count, Data} of
    {0, Bin} when is_binary(Bin) ->
      ?LOG_NOTICE("A!"),
      {0, {<<>>, Data}};
    {0, _} ->
      ?LOG_NOTICE("B!"),
      {0, Data};

    {Count, Data} when is_bitstring(Data) ->
      ?LOG_NOTICE("C"),
      read_bits({<<>>, Data}, Count, Opts);

    {Count, {Bits, <<>>}} when bit_size(Bits) < Count ->
      ?LOG_NOTICE("D!"),
      {error, insufficient_data};

    {Count, {Bits1, Bin}} when bit_size(Bits1) >= Count ->
      <<Bits:Count, BitsTail/bits>> = Bits1,
      ?LOG_NOTICE("E! with Bits = ~p (from ~p), count: ~p", [Bits, Bits1, Count]),
      {Bits, {BitsTail, Bin}};

    {Count, {Bits, Bin}} when bit_size(Bits) < Count ->
      ?LOG_NOTICE("F"),
      Buflen = bit_size(Bits),   % the buffel in the room
      <<Byte:8/bits, Tail/binary>> = Bin,
      DoRev = proplists:get_bool(reverse_input_byte_order, Opts),
      Rev = case DoRev of
        true -> reverse_byte(Byte);
        false -> Byte
      end,
      <<RevI:(bit_size(Rev))>> = Rev,

      N = <<Bits:Buflen/bits, Byte:8/bits>>,
      N2 = <<Byte:8/bits, Bits:Buflen/bits>>,
      N3 = <<Bits:Buflen/bits, Rev:8/bits>>,
      N4 = <<Rev:8/bits, Bits:Buflen/bits>>,
      NSize = bit_size(N),
      case 8 - NSize rem 8 of
        8       ->
          I1 = b2i(N),
          IW1 = I1 bsr (NSize - Count),
          I2 = b2i(N2),
          IW2 = I2 bsr (NSize - Count),
          I3 = b2i(N3),
          IW3 = I3 bsr (NSize - Count),
          I4 = b2i(N4),
          IW4 = I4 bsr (NSize - Count),
          ?LOG_NOTICE("N1 = ~p ~"++integer_to_list(NSize)++".2.0B~n"
                      "N2 = ~p ~"++integer_to_list(NSize)++".2.0B~n"
                      "N3 = ~p ~"++integer_to_list(NSize)++".2.0B~n"
                      "N4 = ~p ~"++integer_to_list(NSize)++".2.0B~n"
                      "n1 want: ~p~n"
                      "n2 want: ~p~n"
                      "n3 want: ~p~n"
                      "n4 want: ~p~n"
                      "rev: ~p (~p), ~p~n",
          [I1, I1, I2, I2, I3, I3, I4, I4, IW1, IW2, IW3, IW4, Rev, RevI, DoRev]);
        PadSize ->
          I1 = b2i(<<0:PadSize, N:NSize/bits>>),
          IW1 = I1 bsr (NSize - Count),
          I2 = b2i(<<0:PadSize, N2:NSize/bits>>),
          IW2 = I2 bsr (NSize - Count),
          I3 = b2i(<<0:PadSize, N3:NSize/bits>>),
          IW3 = I3 bsr (NSize - Count),
          I4 = b2i(<<0:PadSize, N4:NSize/bits>>),
          IW4 = I4 bsr (NSize - Count),
          ?LOG_NOTICE("N1 = ~p ~"++integer_to_list(NSize)++".2.0B (padsize: ~p)~n"
                      "N2 = ~p ~"++integer_to_list(NSize)++".2.0B (padsize: ~p)~n"
                      "N3 = ~p ~"++integer_to_list(NSize)++".2.0B (padsize: ~p)~n"
                      "N4 = ~p ~"++integer_to_list(NSize)++".2.0B (padsize: ~p)~n"
                      "n1 want: ~p~n"
                      "n2 want: ~p~n"
                      "n3 want: ~p~n"
                      "n4 want: ~p~n"
                      "rev: ~p (~p), ~p~n",
          [I1, I1, PadSize, I2, I2, PadSize, I3, I3, PadSize, I4, I4, PadSize, IW1, IW2, IW3, IW4, Rev, RevI, DoRev])
      end,

      <<OldInt:Buflen>> = Bits,
      <<NewInt:8>> = Byte,

      ?LOG_NOTICE("F: Old=~p (~"++integer_to_list(Buflen)++".2.0B), New=~p (~8.2.0B) ~nBin: ~p", [OldInt, OldInt, NewInt, NewInt, Bin]),

      %read_bits({<<Rev:8/bits, Bits:Buflen/bits>>, Tail}, Count)
      read_bits({<<Bits:Buflen/bits, Rev:8/bits>>, Tail}, Count, Opts)

      %read_bits({<<Bits:Buflen/bits, Byte:8/bits>>, Tail}, Count)
      %read_bits({<<Byte:8/bits, Bits:Buflen/bits>>, Tail}, Count)

      %read_bits({reverse_byte(<<Bits:Buflen/bits, Rev:8/bits>>), Tail}, Count)
      %read_bits({reverse_byte(<<Rev:8/bits, Bits:Buflen/bits>>), Tail}, Count)

      %read_bits({reverse_byte(<<Bits:Buflen/bits, Byte:8/bits>>), Tail}, Count)
      %read_bits({reverse_byte(<<Byte:8/bits, Bits:Buflen/bits>>), Tail}, Count)

%     BufSize = bit_size(Bits1),
%     ExtraSize = Count-bit_size(Bits1),
%     PadSize = 8 - ExtraSize,
%     <<Byte:1/binary, Tail/binary>> = Bin,
%     ?LOG_NOTICE("~p eating a byte from Bin to Bits: Bits: ~p, Bin: ~p", [Count, Bits1, Bin]),
%     <<BitsTail:PadSize/bits, MoreBits:ExtraSize/bits>> = <<Byte/binary>>,
%     <<Result:Count>> = <<MoreBits:ExtraSize/bits, Bits1:BufSize/bits>>,
%     {Result, {BitsTail, Tail}}
  end.

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
