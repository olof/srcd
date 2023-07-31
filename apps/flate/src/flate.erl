% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-

-module(flate).
% This module tries to implement RFC 1951, to be able to support
% inflating compressed objects.

-export([in/1, in/2, in/3, de/1, tail/1, stats/1]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-export([fixed/0]).
-endif.

%%%% Inflating a compressed blob:
% {more, Len0, Context2} = flate:in(Part1),
%
% % We got back a 'more' atom, let's feed it Len0 more units of data!
% {more, Len1, Context1} = flate:in(Context2, Part2),
% {more, Len2, Context0} = flate:in(Context1, Part3),
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
Name(#zlib{op=Name} = State, Opts) -> route(Name, State, Opts);
Name(#zlib{op=Op}, Opts) -> {badarg, op, Op};
Name(Data, Opts) -> Name(#zlib{op=Name, input=Data}, Opts)).

de(Data) -> de(Data, []).
?op(de).

in(State = #zlib{input=De}, Data, Opts) when is_list(Data) ->
  Chunk = list_to_binary(Data),
  inflate(State#zlib{input= <<De/binary, Chunk/binary>>}, Opts);
in(State = #zlib{input=De}, Data, Opts) ->
  inflate(State#zlib{input= <<De/binary, Data/binary>>}, Opts).
in(Data) when is_list(Data) -> in(list_to_binary(Data), []);
in(Data) -> in(Data, []).
?op(in).

% route() is a hack, to workaround a limitation with erlang preprocessor; i
% couldn't call a function called Name?MODULE when Name was a macro parameter.
% This saddened me.
route(in, State, Opts) -> inflate(State, Opts);
route(de, State, Opts) -> deflate(State, Opts).

inflate(#zlib{input= <<>>, state=data} = Ctx, Opts) -> {more, 1, Ctx};
inflate(#zlib{input=Enc, output=Dec, state=data, read_count=Rc, write_count=Wc} = Ctx, Opts) ->
  % parse code tree, parse compressed bytes
  %<<Btail:5/bits, Btype:2, Bfinal:1, Tail/binary>> = Enc,
  {Bfinal, Tail2} = read_bits(Enc, 1, [reverse_input_byte_order]),
  {BtypeR, Tail1} = read_bits(Tail2, 2, [reverse_input_byte_order]),
  Btype = flate_utils:reverse_int(BtypeR, 2),

  case inflate_block(int_to_btype(Btype), Tail1, Opts) of
    {ok, This, NewTail, ReadLen} ->
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
        0 -> inflate(NewCtx, Opts);
        1 -> finalize(NewCtx, Opts)
      end;
    {more, Missing} ->
      {more, Missing, Ctx}
  end.

deflate(#zlib{input=Input} = Ctx, Opts) ->
  Z = zlib:open(),
  ok = zlib:'deflateInit'(Z, default),
  [Output] = zlib:deflate(Z, Input, finish),
  ok = zlib:'deflateEnd'(Z),
  {ok, Output,
       finalize(Ctx#zlib{read_count=size(Input), write_count=size(Output)}, Opts)}.

stats(#zlib{read_count=R, write_count=W}) -> {ok, [{read, R}, {written, W}]}.

tail(#zlib{op=in, input=Tail}) -> Tail;
tail(#zlib{op=de, output=Tail}) -> Tail.  % This shouldn't currently happen

finalize(#zlib{input={_, Data}} = Ctx, Opts) ->
  % TODO If we have an incomplete byte, we just throw it away now. That
  %      may, or may not, be an ok thing to do.
  finalize(Ctx#zlib{input=Data}, Opts);
finalize(#zlib{output=Out} = Ctx, Opts) ->
  {ok, iolist_to_binary(lists:reverse(Out)),
   Ctx#zlib{state=finalized, output=undefined}}.

inflate_block(no_compression, {_, Data}, Opts) when is_binary(Data) andalso size(Data) < 4 ->
  {more, 4-size(Data)};
inflate_block(no_compression, {_, Data}, Opts) when is_binary(Data) ->
  % NOTE: Uncompressed blocks, RFC 1951 section 3.2.1:
  % > Any bits of input up to the next byte boundary are ignored.
  <<Len:16, Nlen:16, Payload/binary>> = Data,
  read_hook(Opts, <<Len:16, Nlen:16>>),
  % > LEN is the number of data bytes in the block.  NLEN is the
  % > one's complement of LEN.
  Nlen = 16#FFFF - Len,
  <<Decoded:Len/bytes, Tail/binary>> = Payload,
  read_hook(Opts, Decoded),
  {ok, Decoded, Tail, Len + 4};
inflate_block(huffman_fixed, {InitialBits, Data}, _Opts) ->
  inflate_symbols(flate_huffman:init(fixed()), {InitialBits, Data});
inflate_block(huffman_dyn, <<>>, _Opts) ->
  {more, 2};
inflate_block(huffman_dyn, {InitialBits, <<>>}, _Opts) when bit_size(InitialBits) < 9 ->
  {more, 1};
inflate_block(huffman_dyn, Bin, _Opts) when is_binary(Bin) andalso size(Bin) < 2 ->
  {more, 1};
inflate_block(huffman_dyn, Data, _Opts) ->
  {Bits, Tail1} = read_bits(Data, 9, [reverse_input_byte_order]),
  <<HDIST:5, HCLEN:4>> = Bits,
  CodeLen = (HCLEN + 4) * 3,
  TrailBitLen = abs(8 - CodeLen) rem 8,
  <<CodeAlphabet:CodeLen/bits, Tail/bits>> = Tail1,
  <<InitialBits:TrailBitLen/bits, BinTail/binary>> = Tail,
  % HLIT + 257 code lengths for the literal/length alphabet,
  %  encoded using the code length Huffman code

  % HDIST + 1 code lengths for the distance alphabet,
  %    encoded using the code length Huffman code

  % The actual compressed data of the block,
  %    encoded using the literal/length and distance Huffman
  %    codes

  % The literal/length symbol 256 (end of data),
  %    encoded using the literal/length Huffman code

  % TODO: maybe i forgot to do byte accounting on this?
  % TODO: codetree doesn't exist. So there's that.
  {ok, Codes, D} = flate_huffman:codetree(dynamic, {InitialBits, BinTail}),
  inflate_symbols(Codes, D).

inflate_symbols(Huffman, Data) -> inflate_symbols(Huffman, Data, [], 0).
inflate_symbols(Huffman, Data, Symbols, BitCount) ->
  case flate_huffman:get_symbol(Huffman, Data) of
    {error, insufficient_data} -> {more, 1};
    {ok, {Len, _, 256}, Tail} ->
      {ok, list_to_binary(lists:reverse(Symbols)), Tail,
	   (BitCount + Len) div 8 + case BitCount + Len rem 8 of 0 -> 0; _ -> 1 end};
    {ok, {Len, Code, Symbol}, Tail} when Symbol < 256 ->
      inflate_symbols(Huffman, Tail, [Symbol | Symbols], BitCount + Len);
    {ok, {Len, _, Code}, Tail1} ->
      case decode_distance_pair(Huffman, Code, Tail1) of
        {more, N} -> {more, N};
        {Length, Dist, Tail, Read} ->
          case clone_output(lists:flatten(Symbols), Dist, Length) of
            {error, Reason} -> error({error, Reason});
            Output -> inflate_symbols(Huffman, Tail, [Output | Symbols],
                                      BitCount + Len + Read)
          end
      end
  end.

-ifdef(TEST).

inflate_symbols_test_() -> lists:concat([
  [
    ?_assertEqual(Expected,
                  inflate_symbols(flate_huffman:init(flate:fixed()), In)) ||
      {In, Expected} <- [
        {{<<0:7>>, <<>>}, {ok, <<>>, {<<>>, <<>>}, 1}},
        {{<<>>, <<0>>},   {ok, <<>>, {<<0:1>>, <<>>}, 1}},
        {<<0>>,           {ok, <<>>, {<<0:1>>, <<>>}, 1}}
	% TODO missing tests for non-empty blobs
      ]
  ]
]).

-endif.

clone_output(Symbols, Dist, _) when Dist > length(Symbols) ->
  {error, {deflate_distance_too_far_back, Dist}};
clone_output(Symbols, Dist, Length) ->
  {_, Buf} = lists:split(length(Symbols)-Dist, Symbols),
  clone_output(Buf, Length, [], []).

clone_output(_, 0, Cur, Acc) ->
  lists:concat(lists:reverse([lists:reverse(Cur)|Acc]));
clone_output([], Len, Cur, Acc) ->
  Buf = lists:concat(lists:reverse([lists:reverse(Cur)|Acc])),
  clone_output(Buf, Len, [], [Buf]);
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
  ?_assertEqual("abcdabcda", clone_output("abcd", 4, 9)),

  ?_assertEqual(lists:duplicate(258, $?), clone_output("?", 1, 258)),
  ?_assertEqual(lists:flatten(lists:duplicate(129, "c?")),
                clone_output("abc?", 2, 258))
].

-endif.

decode_distance_pair(Huffman, Code, Data) ->
  case decode_distance_len(Code, Data) of
    {more, N} -> {more, N};
    {Length, LengthTail, BitsRead1} ->
      case decode_distance(LengthTail) of
        {more, N} -> {more, N};
        {Distance, DistanceTail, BitsRead2} ->
          {Length, Distance, DistanceTail, BitsRead1 + BitsRead2}
      end
  end.

decode_distance_len(Code, Data) ->
  {ExtraBits, Len} = distance_code_length(Code),
  case read_bits(Data, ExtraBits) of
    % TODO: improve {more, 1} to calculate how many bytes we should read
    {error, insufficient_data} -> {more, 1};
    %                 {((Len bsl ExtraBits) + Extra, Tail, ExtraBits}.
    {Extra, Tail} -> {Len + flate_utils:reverse_int(Extra, ExtraBits),
                      Tail, ExtraBits}
  end.

decode_distance(Data) ->
  case read_bits(Data, 5, []) of
    {error, insufficient_data} -> {more, 1};
    {RevCode, Extras} ->
      Code = flate_utils:reverse_int(RevCode, 5),
      Base = distance_base(Code),
      {Code, ExtraBits} = distance_extra_bits(Code),
      case read_bits(Extras, ExtraBits) of
        {error, insufficient_data} -> {more, 1};
        {Extra, Tail} ->
          {Base + Extra + 1, Tail, 5 + ExtraBits}
      end
  end.

fixed() ->
  % Literal value    Bits                 Codes
  % -------------------------------------------------------
  %       0 - 143     8         00110000 through  10111111   (48-191)
  %     144 - 255     9        110010000 through 111111111  (400-511)
  %     256 - 279     7          0000000 through   0010111    (0- 23)
  %     280 - 287     8         11000000 through  11000111  (192-199)
  lists:concat([
    [{X, 8} || X <- lists:seq(0, 143)],
    [{X, 9} || X <- lists:seq(144, 255)],
    [{X, 7} || X <- lists:seq(256, 279)],
    [{X, 8} || X <- lists:seq(280, 287)]
  ]).

dynamic() ->
  % Literal value    Bits                 Codes
  % -------------------------------------------------------
  %       0 - 15     8         00110000 through  10111111
  %           16     9        110010000 through 111111111
  %           17     7          0000000 through   0010111
  %     280 - 28     7         11000000 through  11000111
  lists:concat([
    [{X, 3} || X <- lists:seq(0, 15)],
    [{X, 9} || X <- lists:seq(144, 255)],
    [{X, 7} || X <- lists:seq(256, 279)],
    [{X, 8} || X <- lists:seq(280, 287)]
  ]).

-define(distance_code(Lower, Upper, Extra),
        distance_code(N) when N >= Lower andalso N =< Upper div 4 ->
	  {N, {5, Extra * 2 + 3}};
        distance_code(N) when N > Upper div 4 andalso (N =< Upper) ->
	  {N, {5, Extra * 2 + 3}}).

distance_codes() -> [distance_code(N) || N <- lists:seq(1, 32768) ].
distance_code(1) -> {1, {5, 0}};
distance_code(2) -> {2, {5, 1}};
distance_code(3) -> {3, {5, 2}};
distance_code(4) -> {4, {5, 3}};
?distance_code(      5,     8,   1);
?distance_code(      9,    16,   2);
?distance_code(     17,    32,   3);
?distance_code(     33,    64,   4);
?distance_code(     65,   128,   5);
?distance_code(    129,   256,   6);
?distance_code(    257,   512,   7);
?distance_code(    513,  1024,   8);
?distance_code(   1025,  2048,   9);
?distance_code(   2049,  4096,  10);
?distance_code(   4097,  8192,  11);
?distance_code(   8193, 16384,  12);
?distance_code(  16385, 32768,  13).

distance_base() -> [
  { 0,     1},
  { 1,     2},
  { 2,     3},
  { 3,     4},
  { 4,     5},
  { 5,     7},
  { 6,     9},
  { 7,    13},
  { 8,    17},
  { 9,    25},
  {10,    33},
  {11,    49},
  {12,    65},
  {13,    97},
  {14,   129},
  {15,   193},
  {16,   257},
  {17,   385},
  {18,   513},
  {19,   769},
  {20,  1025},
  {21,  1537},
  {22,  2049},
  {23,  3073},
  {24,  4097},
  {25,  6145},
  {26,  8193},
  {27, 12289},
  {28, 16385},
  {29, 24577}
].
distance_base(Code) ->
  {Code, Value} = lists:keyfind(Code, 1, distance_base()),
  Value.

distance_length_base() -> [
  { 0,   3}, { 1,   4}, { 2,   5}, { 3,   6}, { 4,   7}, { 5,   8}, { 6,   9},
  { 7,  10}, { 8,  11}, { 9,  13}, {10,  15}, {11,  17}, {12,  19}, {13,  23},
  {14,  27}, {15,  31}, {16,  35}, {17,  43}, {18,  51}, {19,  59}, {20,  67},
  {21,  83}, {22,  99}, {23, 115}, {24, 131}, {25, 163}, {26, 195}, {27, 227},
  {28, 258}
].
distance_length_base(Code) ->
  {Code, Value} = lists:keyfind(Code, 1, distance_length_base()),
  Value.

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
  {L, _} = case Code of
    257 -> {0,   3}; 258 -> {0,   4}; 259 -> {0,   5}; 260 -> {0,   6};
    261 -> {0,   7}; 262 -> {0,   8}; 263 -> {0,   9}; 264 -> {0,  10};
    265 -> {1,  11}; 266 -> {1,  13}; 267 -> {1,  15}; 268 -> {1,  17};
    269 -> {2,  19}; 270 -> {2,  23}; 271 -> {2,  27}; 272 -> {2,  31};
    273 -> {3,  35}; 274 -> {3,  43}; 275 -> {3,  51}; 276 -> {3,  59};
    277 -> {4,  67}; 278 -> {4,  83}; 279 -> {4,  99}; 280 -> {4, 115};
    281 -> {5, 131}; 282 -> {5, 163}; 283 -> {5, 195}; 284 -> {5, 227};
    285 -> {0, 258}
  end.
  %{L, Code}.

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
?check_full_inflate(inflate_a_huffman_fixed_test_, <<75, 4, 0>>, <<"a">>).

%% $ perl -e '
%%   print map { chr } 179, 183, 31, 5, 163, 96, 20, 140, 2, 8, 0, 0
%% ' | github/madler/infgen/infgen -dd -r
%% last                    ! 1                    1:1
%% fixed                   ! 01                   1:2
%% literal '?              ! 11110110           246:8
%% literal '?              ! 11110110           246:8
%% match 258 1             ! 00000 0:5 10100011 163:8
%% match 258 1             ! 00000 0:5 10100011 163:8
%% match 258 1             ! 00000 0:5 10100011 163:8
%% match 258 1             ! 00000 0:5 10100011 163:8
%% match 6 1               ! 00000 0:5 0010000   16:7
%% end                     ! 0000000              0:7
%% (padding to whole byte) ! 000000               0:6
%%                                                ---
%%                                                 96 (12 bytes)
?check_full_inflate('inflate_?x1040_fixed_pigz_test_',
		    <<179, 183, 31, 5, 163, 96, 20, 140, 2, 8, 0, 0>>,
                    list_to_binary(lists:duplicate(1040, "?"))).


test_inflate_steps() ->
  In = <<179, 183, 31, 5, 163, 96, 20, 140, 2, 8, 0, 0>>,
  Expect = list_to_binary(lists:duplicate(1040, "?")),
  Fixed = flate_huffman:init(fixed()),

  % parse code tree, parse compressed bytes
  <<Btail:5/bits, Btype:2, Bfinal:1, Tail0/binary>> = In,

  ?assertEqual(1, Bfinal),
  ?assertEqual(huffman_fixed, int_to_btype(Btype)),
  ?assertEqual(<<22:5>>, Btail),
  ?assertEqual(<<13:5>>, flate_utils:reverse_byte(Btail)),
  ?assertEqual(<<183, 31, 5, 163, 96, 20, 140, 2, 8, 0, 0>>, Tail0),

  %{ok, CodeMatch1, Tail1} = flate_huffman:get_symbol(Fixed, {flate_utils:reverse_bits(Btail), Tail0}),
  {ok, CodeMatch1, Tail1} = flate_huffman:get_symbol(Fixed, {<<13:5>>, Tail0}),
  ?assertEqual({8, 246, $?}, CodeMatch1),
  ?assertEqual({<<13:5>>, <<31, 5, 163, 96, 20, 140, 2, 8, 0, 0>>}, Tail1),

  {ok, {8, 246, $?}, Tail2} = flate_huffman:get_symbol(Fixed, Tail1),

  % ??? 24, i got it to 31
  ?assertEqual({<<24:5>>, <<5, 163, 96, 20, 140, 2, 8, 0, 0>>}, Tail2),

  MaxSubstr = lists:duplicate(258, $?),
  ShortSubstr = lists:duplicate(6, $?),

  % Repeat 1
  {ok, {8, 163, 285}, Tail3} = flate_huffman:get_symbol(Fixed, Tail2),
  ?assertEqual({<<0:5>>, Tail4 = <<163, 96, 20, 140, 2, 8, 0, 0>>}, Tail3),
  % head of the bin list is: 2#00000101
  % we steal the bits 101 (use them as lsb of code 197), and left is 00000.
  {258, 2, Tail4, 5} = decode_distance_pair(Fixed, 285, Tail3),
  MaxSubstr = clone_output([63, 63], 2, 258),

  % Repeat 2
  {ok, {8, 163, 285}, Tail5 = {<<>>, <<96, 20, 140, 2, 8, 0, 0>>}} = flate_huffman:get_symbol(Fixed, Tail4),
  {258, 2, Tail6, 5} = decode_distance_pair(Fixed, 285, Tail5),
  MaxSubstr = clone_output([63, 63] ++ MaxSubstr, 2, 258),

  % Repeat 3
  {ok, {8, 163, 285}, Tail7} = flate_huffman:get_symbol(Fixed, Tail6),
  {258, 2, Tail8, 5} = decode_distance_pair(Fixed, 285, Tail7),
  MaxSubstr = clone_output([63, 63] ++ MaxSubstr ++ MaxSubstr, 2, 258),

  % Repeat 4
  {ok, {8, 163, 285}, Tail9} = flate_huffman:get_symbol(Fixed, Tail8),
  {258, 2, Tail10, 5} = decode_distance_pair(Fixed, 285, Tail9),
  MaxSubstr = clone_output([63, 63] ++ MaxSubstr ++ MaxSubstr ++ MaxSubstr, 2, 258),

  % Repeat 5, shorter length
  {ok, {7, 16, 260}, Tail11} = flate_huffman:get_symbol(Fixed, Tail10),
  {6, 2, Tail12, 5} = decode_distance_pair(Fixed, 260, Tail11),
  ShortSubstr = clone_output([63, 63] ++ MaxSubstr ++ MaxSubstr ++ MaxSubstr ++ MaxSubstr, 2, 6),

  % end marker (7 bits) and padding (6 bits)
  ?assertEqual({<<0:5>>, <<0>>}, Tail12).

inflate_steps_test_() -> [fun () -> test_inflate_steps() end].

-endif.

read_bits(Data, Count) -> flate_utils:read_bits(Data, Count).
read_bits(Data, Count, Opts) -> flate_utils:read_bits(Data, Count, Opts).
read_hook(Opts, Data) -> flate_utils:read_hook(Opts, Data).
