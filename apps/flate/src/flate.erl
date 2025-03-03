% ex:ts=2:sw=2:sts=2:et:foldmarker=-ifdef(TEST).,-endif.:foldmethod=marker
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-

-module(flate).
% This module tries to implement RFC 1951, to be able to
% support inflating compressed objects. Details of specific
% algorithms are in other modules, like flate_huffman,
% flate_lz77 and flate_adler32. This module instead deals
% mostly with the block structure and details specific
% to the DEFLATE format. It is the main entrypoint for
% uncompressing such blobs. (See the flatez module for
% instead dealing with zlib compressed blobs.)

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

-export([in/1, in/2, in/3, de/1, tail/1, stats/1]).

-ifdef(TEST).
-export([fixed/0]).
-endif.

-include("record.hrl").
-include("check.hrl").

-define(op(Name),
Name(#zlib{op=Name} = State, Opts) -> route(Name, State, Opts);
Name(#zlib{op=Op}, _Opts) -> {badarg, op, Op};
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

inflate(#zlib{state=data, input= <<>>} = Ctx, _Opts) -> {more, 1, Ctx};
inflate(#zlib{state=finalize} = Ctx, Opts) -> finalize(Ctx, Opts);
inflate(#zlib{state=data, input=Enc, output=Dec, read_count=Rc, write_count=Wc} = Ctx, Opts) ->
  % parse code tree, parse compressed bytes
  {<<Bfinal:1, BtypeR:2>>, Tail} = flate_utils:read_bits(Enc, 3, []),
  Btype = flate_utils:reverse_int(BtypeR, 2),

  case inflate_block(int_to_block_type(Btype), Tail, Opts) of
    {ok, This, NewTail, ReadLen} ->
      inflate(Ctx#zlib{
        input=NewTail,
        state=case Bfinal of
          0 -> data;
          1 -> finalize
        end,
        output=case Dec of
          undefined -> This;
          _ -> [This | Dec]
        end,
        write_count=(Wc + size(This)),
        read_count=(Rc + ReadLen + 1)
      }, Opts);

    {more, Missing} ->
      {more, Missing, Ctx}
  end.

deflate(#zlib{input=Input} = Ctx, Opts) ->
  Z = zlib:open(),
  ok = zlib:'deflateInit'(Z, default),
  [Output] = zlib:deflate(Z, Input, finish),
  ok = zlib:'deflateEnd'(Z),
  {ok, Output, finalize(Ctx#zlib{read_count=size(Input),
                                 write_count=size(Output)},
                        Opts)}.

stats(#zlib{read_count=R, write_count=W}) -> {ok, [{read, R}, {written, W}]}.

tail(#zlib{op=in, input=Tail}) -> Tail;
tail(#zlib{op=de, output=Tail}) -> Tail.  % This shouldn't currently happen

finalize(#zlib{input={_, Data}} = Ctx, Opts) ->
  % TODO If we have an incomplete byte, we just throw it away now. That
  %      may, or may not, be an ok thing to do.
  finalize(Ctx#zlib{input=Data}, Opts);
finalize(#zlib{output=Out} = Ctx, _Opts) ->
  {ok, iolist_to_binary(lists:reverse(Out)),
   Ctx#zlib{state=finalized, output=undefined}}.

inflate_block(no_compression, {_, Data}, _Opts) when is_binary(Data)
                                                andalso size(Data) < 4 ->
  {more, 4-size(Data)};
inflate_block(no_compression, {_, Data}, Opts) when is_binary(Data) ->
  % NOTE: Uncompressed blocks, RFC 1951 section 3.2.1:
  % > Any bits of input up to the next byte boundary are ignored.
  <<Len:16, Nlen:16, Payload/binary>> = Data,
  flate_utils:read_hook(Opts, <<Len:16, Nlen:16>>),

  % > LEN is the number of data bytes in the block.  NLEN is the
  % > one's complement of LEN.
  Nlen = 16#FFFF - Len,

  <<Decoded:Len/bytes, Tail/binary>> = Payload,
  flate_utils:read_hook(Opts, Decoded),

  {ok, Decoded, Tail, Len + 4};

inflate_block(huffman_fixed, {InitialBits, Data}, _Opts) ->
  inflate_symbols(flate_huffman:init(fixed()), {InitialBits, Data});

inflate_block(huffman_dyn, Bin, _Opts)
  when is_binary(Bin)
  andalso size(Bin) < 2 ->
    {more, 2-size(Bin)};
inflate_block(huffman_dyn, {InitialBits, Bin}, _Opts)
  when bit_size(InitialBits) < 6
  andalso size(Bin) < 2 ->
    {more, 2-size(Bin)};
inflate_block(huffman_dyn, {InitialBits, <<>>}, _Opts)
  when bit_size(InitialBits) < 14 ->
    {more, 1};
inflate_block(huffman_dyn, {Bits, Tail}, Opts) when bit_size(Bits) < 14 ->
  Missing = ((14-bit_size(Bits)-1) div 8 + 1) * 8,
  <<AddBits:Missing/bits, Tail1/binary>> = Tail,
  NewBits = <<Bits/bits, AddBits/bits>>,
  inflate_block(huffman_dyn, {NewBits, Tail1}, Opts);
inflate_block(huffman_dyn, {Bits, Tail1}, _Opts) ->
  <<HLIT:5, HDIST:5, HCLEN:4, InitialBits/bits>> = Bits,

  % Yes, the dynamic Huffman codes and extra bits are stored in the same order
  % as the fixed Huffman codes. The tricky part is understanding how the
  % Huffman codes are transmitted in the deflate stream header for each block.
  % -- madler @ https://stackoverflow.com/a/10472789

  % Calmarius wrote:
  % > Actually the RFC is wrong with that statement. And I got bitten by this. The
  % > extra bits must be read LSB first, like any other bit fields in the archive.
  %
  % If you pull the bits out one at a time, then yes, the LSB is the first
  % bit-sized value pulled (e.g. as above, for 14 you will get 0,1,1,1) - but
  % if you pull the needed bits all at once (e.g. 14 is 1110), then only one
  % value is pulled, and the LSB will be the last (i.e. least significant) bit
  % in that value. With that understanding the RFC is correct. –
  % mwfearnley (2017-2018)

  case read_codes(HCLEN, {InitialBits, Tail1}) of
    {error, insufficient_data} -> {more, 1};
    {CodeAlphabet, Tail} ->
      % HLIT + 257 code lengths for the literal/length alphabet,
      %  encoded using the code length Huffman code

      % HDIST + 1 code lengths for the distance alphabet,
      %    encoded using the code length Huffman code

      % The actual compressed data of the block,
      %    encoded using the literal/length and distance Huffman
      %    codes

      % The literal/length symbol 256 (end of data),
      %    encoded using the literal/length Huffman code

      % Turns out, i can't find implementation for flate_huffman:codetree... Fun!
      {ok, Dynamic} = flate_huffman:init(dynamic(CodeAlphabet)),
      case inflate_symbols(Dynamic, Tail) of
        {error, insufficient_data} -> {more, 1};
        {ok, Codes, D} -> inflate_symbols(Codes, D)
      end
  end.

read_codes(HCLEN, Data) ->
  CodeLen = (HCLEN + 4) * 3,
  case flate_utils:read_bits(Data, CodeLen) of
    {error, insufficient_data} -> {more, 1};
    {CodeBlob, Tail} -> sort_code_len({[ Code || << Code:3/integer >> <= CodeBlob ], Tail})
  end.

inflate_symbols(Huffman, Data) -> inflate_symbols(Huffman, Data, [], 0).
inflate_symbols(Huffman, Data, Symbols, BitCount) ->
  case flate_huffman:get_symbol(Huffman, Data) of
    {error, insufficient_data} -> {more, 1};

    {ok, {Len, _, 256}, Tail} ->
      {ok, list_to_binary(lists:reverse(Symbols)), Tail,
	     (BitCount + Len) div 8 + ceil((BitCount + Len) rem 8 / 8)};

    {ok, {Len, _Code, Symbol}, Tail} when Symbol < 256 ->
      inflate_symbols(Huffman, Tail, [Symbol | Symbols], BitCount + Len);

    {ok, {Len, _, Code}, Tail1} ->
      % FIXME: we can refer to distances going back across blocks,
      %        which we don't pass as it is right now. What if we
      %        defer expansion to later, and just leave an lz
      %        instruction in its place? Or just pass the extra
      %        context (i.e. results from previous blocks).
      case flate_lz77:decode(Symbols, Code, Tail1) of
        {more, N} -> {more, N};
        {error, Reason} -> error({error, Reason});
        {ok, Output, Tail, Read} ->
          inflate_symbols(Huffman, Tail, [Output | Symbols],
                          BitCount + Len + Read)
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
      ]
  ]
]).

-endif.

dynamic(Data) ->
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

fixed() ->
% 0 - 15: Represent code lengths of 0 - 15
%     16: Copy the previous code length 3 - 6 times.
%         The next 2 bits indicate repeat length
%               (0 = 3, ... , 3 = 6)
%            Example:  Codes 8, 16 (+2 bits 11),
%                      16 (+2 bits 10) will expand to
%                      12 code lengths of 8 (1 + 6 + 5)
%     17: Repeat a code length of 0 for 3 - 10 times.
%         (3 bits of length)
%     18: Repeat a code length of 0 for 11 - 138 times
%         (7 bits of length)
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

int_to_block_type(0) -> no_compression;
int_to_block_type(1) -> huffman_fixed;
int_to_block_type(2) -> huffman_dyn;
int_to_block_type(N) -> {invalid_zlib_block_type, N}.

code_len_orders() -> [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15].

sort_code_len(Codes) when length(Codes) < 19 ->
  sort_code_len(flate_utils:right_pad_list(Codes, 19, 0));
sort_code_len(Codes) when length(Codes) < 20 ->
  [V || {_, V} <- lists:sort(lists:zip(code_len_orders(), Codes))].

-ifdef(TEST).
sort_code_len_test_() -> [
  ?_assertEqual([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], sort_code_len([])),
  ?_assertEqual([4, 18, 16, 14, 12, 10, 8, 6, 5, 7, 9, 11, 13, 15, 17, 19, 1, 2, 3], sort_code_len(lists:seq(1, 19)))
].
-endif.

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
  _Expect = list_to_binary(lists:duplicate(1040, "?")),
  Fixed = flate_huffman:init(fixed()),

  % parse code tree, parse compressed bytes
  <<Btail:5/bits, Btype:2, Bfinal:1, Tail0/binary>> = In,

  ?assertEqual(1, Bfinal),
  ?assertEqual(huffman_fixed, int_to_block_type(Btype)),
  ?assertEqual(<<22:5>>, Btail),
  ?assertEqual(<<13:5>>, flate_utils:reverse_byte(Btail)),
  ?assertEqual(<<183, 31, 5, 163, 96, 20, 140, 2, 8, 0, 0>>, Tail0),

  %{ok, CodeMatch1, Tail1} = flate_huffman:get_symbol(Fixed, {flate_utils:reverse_bits(Btail), Tail0}),
  {ok, CodeMatch1, Tail1} = flate_huffman:get_symbol(Fixed, {<<13:5>>, Tail0}),

  ?assertEqual({8, 111, $?}, CodeMatch1),
  ?assertEqual({<<13:5>>, <<31, 5, 163, 96, 20, 140, 2, 8, 0, 0>>}, Tail1),

  {ok, {8, 111, $?}, Tail2} = flate_huffman:get_symbol(Fixed, Tail1),

  % ??? 24, i got it to 31
  ?assertEqual({<<24:5>>, <<5, 163, 96, 20, 140, 2, 8, 0, 0>>}, Tail2),

  MaxSubstr = lists:duplicate(258, $?),
  ShortSubstr = lists:duplicate(6, $?),

  % Repeat 1
  {ok, CodeMatch2, Tail3} = flate_huffman:get_symbol(Fixed, Tail2),
  ?assertEqual({8, 197, 285}, CodeMatch2),
  ?assertEqual({<<0:5>>, Tail4 = <<163, 96, 20, 140, 2, 8, 0, 0>>}, Tail3),
  % head of the bin list is: 2#00000101
  % we steal the bits 101 (use them as lsb of code 197), and left is 00000.
  {258, 2, Tail4, 5} = flate_lz77:decode_distance_pair(285, Tail3),
  MaxSubstr = flate_lz77:clone_output([63, 63], 2, 258),

  % Repeat 2
  {ok, {8, 197, 285}, Tail5 = {<<>>, <<96, 20, 140, 2, 8, 0, 0>>}} = flate_huffman:get_symbol(Fixed, Tail4),
  {258, 2, Tail6 = {<<6:3>>, <<20, 140, 2, 8, 0, 0>>}, 5} = flate_lz77:decode_distance_pair(285, Tail5),
  MaxSubstr = flate_lz77:clone_output([63, 63] ++ MaxSubstr, 2, 258),

  % Repeat 3
  {ok, {8, 197, 285}, Tail7} = flate_huffman:get_symbol(Fixed, Tail6),
  {258, 2, Tail8, 5} = flate_lz77:decode_distance_pair(285, Tail7),
  MaxSubstr = flate_lz77:clone_output([63, 63] ++ MaxSubstr ++ MaxSubstr, 2, 258),

  % Repeat 4
  {ok, {8, 197, 285}, Tail9} = flate_huffman:get_symbol(Fixed, Tail8),
  {258, 2, Tail10, 5} = flate_lz77:decode_distance_pair(285, Tail9),
  MaxSubstr = flate_lz77:clone_output([63, 63] ++ MaxSubstr ++ MaxSubstr ++ MaxSubstr, 2, 258),

  % Repeat 5, shorter length
  {ok, {7, 4, 260}, Tail11} = flate_huffman:get_symbol(Fixed, Tail10),
  {6, 2, Tail12, 5} = flate_lz77:decode_distance_pair(260, Tail11),
  ShortSubstr = flate_lz77:clone_output([63, 63] ++ MaxSubstr ++ MaxSubstr ++ MaxSubstr ++ MaxSubstr, 2, 6),

  % end marker (7 bits) and padding (6 bits)
  ?assertEqual({<<0:5>>, <<0>>}, Tail12).

inflate_steps_test_() -> [fun () -> test_inflate_steps() end].

fixed_huffman_test_() ->
  Fixed = flate_huffman:init(fixed()),
  [
    ?_assertEqual({ok, {8, $@+$0, $@}, {<<>>, <<>>}}, flate_huffman:get_symbol(Fixed, <<(flate_utils:reverse_int($@+$0, 8)):8>>))
  ].

-endif.
