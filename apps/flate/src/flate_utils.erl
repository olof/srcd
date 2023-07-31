-module(flate_utils).
-export([b2i/1, read_bits/2, read_bits/3, read_hook/2, reverse_int/2, reverse_byte/1, reverse_bits/1]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

read_hook(Opts, Data) ->
  Hook = proplists:get_value(read_hook, Opts, fun (_) -> ok end),
  Hook(Data).

b2i(B) ->
  <<I:(bit_size(B))>> = B,
  I.

read_bits(Data, Count) ->
  read_bits(Data, Count, []).
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
          IW4 = I4 bsr (NSize - Count);
        PadSize ->
          I1 = b2i(<<0:PadSize, N:NSize/bits>>),
          IW1 = I1 bsr (NSize - Count),
          I2 = b2i(<<0:PadSize, N2:NSize/bits>>),
          IW2 = I2 bsr (NSize - Count),
          I3 = b2i(<<0:PadSize, N3:NSize/bits>>),
          IW3 = I3 bsr (NSize - Count),
          I4 = b2i(<<0:PadSize, N4:NSize/bits>>),
          IW4 = I4 bsr (NSize - Count)
      end,

      <<OldInt:Buflen>> = Bits,
      <<NewInt:8>> = Byte,

      read_bits({<<Bits:Buflen/bits, Rev:8/bits>>, Tail}, Count, Opts)
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
      {<<>>, 0, [], <<0:0>>, {<<>>, <<>>}},
      {<<"abc">>, 8, [], <<$a:8>>, {<<>>, <<"bc">>}},
      {<<"abc">>, 4, [], <<6:4>>, {<<1:4>>, <<"bc">>}},
      {<<"abc">>, 12, [], <<1558:12>>, {<<2:4>>, <<"c">>}},

      {{<<>>, <<"abc">>}, 12, [], <<1558:12>>, {<<2:4>>, <<"c">>}},
      {{<<4:3>>, <<2,8,0,0>>}, 7, [], <<64:7>>, {<<2:4>>, <<8,0,0>>}},
      {
        {<<13:5>>, <<183,31,5,163,96,20,140,2,8,0,0>>},
        8, [reverse_input_byte_order], <<"o">>,
        {<<13:5>>, <<31,5,163,96,20,140,2,8,0,0>>}
      }
   ]
].

-endif.

reverse_int(N, Bits) when is_bitstring(N) ->
  ?LOG_NOTICE("REVERSE INT: ~p ~p", [N, Bits]),
  Bits = bit_size(N),
  RevBin = flate_utils:reverse_bits(N),
  <<Rev:Bits>> = RevBin,
  Rev;
reverse_int(N, Bits) ->
  ?LOG_NOTICE("REVERSE INT: ~p ~p", [N, Bits]),
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
