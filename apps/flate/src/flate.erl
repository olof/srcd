% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-

-module(flate).
% This module tries to implement RFC 1951, to be able to support
% inflating compressed objects.

-export([in/1, in/2, de/1, tail/1, stats/1]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-export([fixed/0]).
-endif.

%%%% Inflating a compressed blob:
% {more, Context2} = flate:in(Part1),
%
% % We got back a 'more' atom, let's feed it more data!
% {more, Context1} = flate:in(Context2, Part2),
% {more, Context0} = flate:in(Context1, Part3),
%
% % When it has had enough, it says 'ok', and returns
% % the decoded content, and a final context object.
% {ok, Decoded, Context} = flate:in(Context0, Part4),
%
% % Anything left at the end?
% Rem = flate:tail(Context),
%
%%%% Deflating an uncompressed blob:
% % This is just a wrapper around OTP's zlib deflate.
% {ok, Encoded, Context} = flate:de(Data)
%
%%%% How much was read/written?
% % Works for both inflate and deflate; sizes in bytes.
% [
%   {read, Read},
%   {written, Written}
% ] = flate:stats(Context).

-include("record.hrl").
-include("check.hrl").

-define(op(Name),
Name(#zlib{op=Name} = State) -> route(Name, State);
Name(#zlib{op=Op}) -> {badarg, op, Op};
Name(Data) -> Name(#zlib{op=Name, input=Data})).

?op(de).
in(State = #zlib{input=De}, Data) when is_list(De) ->
  deflate(State#zlib{input=lists:reverse([Data | De])});
in(State = #zlib{input=De}, Data) ->
  deflate(State#zlib{input=[De, Data]}).
?op(in).

% route() is a hack, to workaround a limitation with erlang preprocessor; i
% couldn't call a function called Name?MODULE when Name was a macro parameter.
% This saddened me.
route(in, State) -> inflate(State);
route(de, State) -> deflate(State).

inflate(#zlib{input= <<>>, state=data} = Ctx) -> {more, Ctx};
inflate(#zlib{input=Enc, output=Dec, state=data, read_count=Rc, write_count=Wc} = Ctx) ->
  % parse code tree, parse compressed bytes
  <<Head:8, _/binary>> = Enc,
  <<Btail:5/bits, Btype:2, Bfinal:1, Tail/binary>> = Enc,
  {ok, This, NewTail, ReadLen} = inflate_block(int_to_btype(Btype),
                                               flate_utils:reverse_byte(Btail),
                                               Tail),

  NewCtx = Ctx#zlib{
    input=NewTail,
    output=case Dec of
      undefined -> This;
      _ -> [This | Dec]
    end,
    write_count=(Wc + size(This)),
    read_count=(Rc + ReadLen + 1)
  },

  case Bfinal of
    0 -> inflate(NewCtx);
    1 -> finalize(NewCtx)
  end.

deflate(#zlib{input=Input} = Ctx) ->
  Z = zlib:open(),
  ok = zlib:'deflateInit'(Z, default),
  [Output] = zlib:deflate(Z, Input, finish),
  ok = zlib:'deflateEnd'(Z),
  {ok, Output,
       finalize(Ctx#zlib{read_count=size(Input), write_count=size(Output)})}.

stats(#zlib{read_count=R, write_count=W}) -> {ok, [{read, R}, {written, W}]}.

tail(#zlib{op=in, input=Tail}) -> Tail;
tail(#zlib{op=de, output=Tail}) -> Tail.  % This shouldn't currently happen

finalize(#zlib{input={_, Data}} = Ctx) ->
  % TODO If we have an incomplete byte, we just throw it away now. That
  %      may, or may not, be an ok thing to do.
  finalize(Ctx#zlib{input=Data});
finalize(#zlib{output=Out} = Ctx) ->
  {ok, iolist_to_binary(lists:reverse(Out)),
   Ctx#zlib{state=finalized, output=undefined}}.

inflate_block(no_compression, _, Data) ->
  % NOTE: Uncompressed blocks, RFC 1951 section 3.2.1:
  % > Any bits of input up to the next byte boundary are ignored.
  <<Len:16, Nlen:16, Payload/binary>> = Data,
  % > LEN is the number of data bytes in the block.  NLEN is the
  % > one's complement of LEN.
  Nlen = 16#FFFF - Len,
  <<Decoded:Len/bytes, Tail/binary>> = Payload,
  {ok, Decoded, Tail, Len + 4};
inflate_block(huffman_fixed, InitialBits, Data) ->
  inflate_symbols(flate_huffman:init(fixed()), {InitialBits, Data});
inflate_block(huffman_dyn, InitialBits, Data) ->
  % TODO: maybe i forgot to do byte accounting on this?
  % TODO: codetree doesn't exist. So there's that.
  {ok, Codes, D} = flate_huffman:codetree(dynamic, {InitialBits, Data}),
  inflate_symbols(Codes, D).

inflate_symbols(Huffman, Data) -> inflate_symbols(Huffman, Data, [], 0).
inflate_symbols(Huffman, Data, Symbols, Bits) ->
  case flate_huffman:get_symbol(Huffman, Data) of
    {ok, {Len, _, 256}, Tail} ->
      {ok, list_to_binary(lists:reverse(Symbols)), Tail,
	   (Bits + Len) div 8 + case Bits + Len rem 8 of 0 -> 0; _ -> 1 end};
    {ok, {Len, _, Symbol}, Tail} when Symbol < 256 ->
      inflate_symbols(Huffman, Tail, [Symbol | Symbols], Bits + Len);
    {ok, {_, _, Code}, Tail1} ->
      {Length, Dist, Tail, Read} = decode_distance_pair(Huffman, Code, Tail1),
      inflate_symbols(Huffman, Tail, [clone_output(lists:flatten(Symbols), Dist, Length) | Symbols],
                      Bits + Read)
  end.

-ifdef(TEST).

inflate_symbols_test_() -> lists:concat([
  [
    ?_assertEqual(Expected,
                  inflate_symbols(flate_huffman:init(flate:fixed()), In)) ||
      {In, Expected} <- [
        {{<<0:7>>, <<>>}, {ok, <<>>, end_of_stream, 1}},
        {{<<>>, <<0>>},   {ok, <<>>, {<<0:1>>, <<>>}, 1}},
        {<<0>>,           {ok, <<>>, {<<0:1>>, <<>>}, 1}}
	% TODO missing tests for non-empty blobs
      ]
  ]
]).

-endif.

clone_output(Symbols, Dist, Length) ->
  {_, Buf} = lists:split(length(Symbols)-Dist, Symbols),
  clone_output(Buf, Length, [], []).
clone_output(_, 0, Cur, Acc) ->
  lists:concat(lists:reverse([lists:reverse(Cur)|Acc]));
clone_output([], Len, Cur, Acc) ->
  Buf = lists:concat(lists:reverse([lists:reverse(Cur)|Acc])),
  clone_output(Buf, Len, [], [Buf]);
clone_output([], N, _, _) ->
  {error, distance_too_far_back, N};
clone_output([S|Buf], Length, Cur, Acc) ->
  clone_output(Buf, Length-1, [S|Cur], Acc).

-ifdef(TEST).

clone_output_test_() -> [
  ?_assertEqual("d",    clone_output("abcd", 1, 1)),
  ?_assertEqual("dd",   clone_output("abcd", 1, 2)),
  ?_assertEqual("dddd", clone_output("abcd", 1, 4)),

  ?_assertEqual("c",    clone_output("abcd", 2, 1)),
  ?_assertEqual("cd",   clone_output("abcd", 2, 2)),
  ?_assertEqual("cdcd", clone_output("abcd", 2, 4)),

  ?_assertEqual("abcd", clone_output("abcd", 4, 4)),
  ?_assertEqual("abcda", clone_output("abcd", 4, 5)),
  ?_assertEqual("abcdabcd", clone_output("abcd", 4, 8)),
  ?_assertEqual("abcdabcda", clone_output("abcd", 4, 9))
].

-endif.

decode_distance_pair(Huffman, Code, Data) ->
  %% We got our length, now we only need our distance:
  % we should be able to construct a code table for the distances,
  % and pass it instead of Huffman as the first parameter:
  %{ok, {Read, _, Sym}, Tail} = flate_huffman:get_symbol(Huffman, Data),

  {Length, LengthTail, BitsRead1} = decode_distance_len(Code, Data),
  {Distance, DistanceTail, BitsRead2} = decode_distance(Code, LengthTail),

  % TODO: Don't forget to do accounting on ExtraBits!
  {Length, Distance, DistanceTail, BitsRead1 + BitsRead2}.

decode_distance_len(Code, Data) ->
  {ExtraBits, Len} = distance_code_length(Code),
  {Extra, Tail} = read_bits(Data, ExtraBits),
  {(Len bsl ExtraBits) + Extra, Tail, ExtraBits}.

decode_distance(Code, Data) ->
  {DistanceCode, Extras} = read_bits(Data, 5),
  {DistanceCode, ExtraBits} = distance_extra_bits(DistanceCode),
  {Extra, Tail} = read_bits(Extras, ExtraBits),
  {(DistanceCode bsl ExtraBits) + Extra + 1, Tail, 5 + ExtraBits}.

read_bits(Data, Count) ->
  case {Count, Data} of
    {0, _} -> {0, Data};

    {Count, Data} when is_binary(Data) andalso Count rem 8 == 0 ->
      <<Bits:Count, Tail/binary>> = Data,
      {Bits, Tail};

    {Count, Data} when is_binary(Data) andalso Count rem 8 > 0 ->
      Pad = 8 - Count rem 8,
      <<Bits:Count, TailBits:Pad/bits, Tail/binary>> = Data,
      {Bits, {TailBits, Tail}};

    {Count, {Bits, Bin}} when bit_size(Bits) == Count ->
      <<Int:Count>> = Bits,
      {Int, Bin};

    {Count, {Bits1, Bin}} when bit_size(Bits1) > Count ->
      <<Bits:Count, BitsTail/bits>> = Bits1,
      {Bits, {BitsTail, Bin}};

    {Count, {Bits1, Bin}} when bit_size(Bits1) < Count ->
      ?LOG_NOTICE("Count: ~p, Bits: ~p, Bin: ~p", [Count, Bits1, Bin]),
      PadSize = 8 - Count rem 8,
      <<Byte:8/bits, Tail/binary>> = Bin,
      <<Bits:Count, BitsTail/bits>> = <<Bits1/bits, Byte/bits>>,
      {Bits, {BitsTail, Tail}}
  end.


fixed() ->
  % Literal value    Bits                 Codes
  % -------------------------------------------------------
  %       0 - 143     8         00110000 through  10111111
  %     144 - 255     9        110010000 through 111111111
  %     256 - 279     7          0000000 through   0010111
  %     280 - 287     8         11000000 through  11000111
  lists:concat([
    [{X, 8} || X <- lists:seq(0, 143)],
    [{X, 9} || X <- lists:seq(144, 255)],
    [{X, 7} || X <- lists:seq(256, 279)],
    [{X, 8} || X <- lists:seq(280, 287)]
  ]).

distance_extra_bits() ->
  % Code table for distance alphabet from RFC 1951 section 3.2.5
  lists:concat([
    [{X,  0} || X <- [ 0,  1, 2, 3]],
    [{X,  1} || X <- [ 4,  5]],
    [{X,  2} || X <- [ 6,  7]],
    [{X,  3} || X <- [ 8,  9]],
    [{X,  4} || X <- [10, 11]],
    [{X,  5} || X <- [12, 13]],
    [{X,  6} || X <- [14, 15]],
    [{X,  7} || X <- [16, 17]],
    [{X,  8} || X <- [18, 19]],
    [{X,  9} || X <- [20, 21]],
    [{X, 10} || X <- [22, 23]],
    [{X, 11} || X <- [24, 25]],
    [{X, 12} || X <- [26, 27]],
    [{X, 13} || X <- [28, 29]]
  ]).
distance_extra_bits(Code) ->
  lists:keyfind(Code, 1, distance_extra_bits()).

distance_code_length(Code) ->
  case Code of
    257 -> {0,   3}; 258 -> {0,   4}; 259 -> {0,   5}; 260 -> {0,   6};
    261 -> {0,   7}; 262 -> {0,   8}; 263 -> {0,   9}; 264 -> {0,  10};
    265 -> {1,  11}; 266 -> {1,  13}; 267 -> {1,  15}; 268 -> {1,  17};
    269 -> {2,  19}; 270 -> {2,  23}; 271 -> {2,  27}; 272 -> {2,  31};
    273 -> {3,  35}; 274 -> {3,  43}; 275 -> {3,  51}; 276 -> {3,  59};
    277 -> {4,  67}; 278 -> {4,  83}; 279 -> {4,  99}; 280 -> {4, 115};
    281 -> {5, 131}; 282 -> {5, 163}; 283 -> {5, 195}; 284 -> {5, 227};
    285 -> {0, 258}
  end.

int_to_btype(0) -> no_compression;
int_to_btype(1) -> huffman_fixed;
int_to_btype(2) -> huffman_dyn;
int_to_btype(N) -> {invalid_zlib_btype, N}.

-ifdef(TEST).
?check_full_inflate(inflate_empty_uncompressed_test_,
                    <<1, 0, 0, 255, 255>>, <<>>).
?check_full_inflate(inflate_a_uncompressed_test_,
                    <<1, 0, 1, 255, 254, "a">>, <<"a">>).
?check_full_inflate(inflate_aaa_uncompressed_test_,
                    <<1, 0, 3, 255, 252, "aaa">>, <<"aaa">>).
?check_full_inflate(inflate_aaa_uncompressed_tail_test_,
                    <<1, 0, 3, 255, 252, "aaaAAA">>, <<"aaa">>, <<"AAA">>).

?check_full_inflate(inflate_empty_huffman_fixed_test_,
                    <<3, 0>>, <<>>).

%% perl -e '
%%   print map { chr } 75, 4, 0
%% ' | github/madler/infgen/infgen -dd -r
%% ! infgen 3.0 output
%% !
%% last            ! 1
%% fixed           ! 01
%% literal 'a      ! 10001001
%% end             ! 0000000
%%                 ! 000000
?check_full_inflate(inflate_a_huffman_fixed_test_,
                    <<75, 4, 0>>, <<"a">>).

-endif.
