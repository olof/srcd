% ex:ts=2:sw=2:sts=2:et;foldmarker=-ifdef(TEST).,-endif.;foldmethod=marker
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-

-module(flate_huffman).
% This module implements huffman coding and related functions.

-export([decode/2]).

-define(SYMBOL_WIDTH, 2).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_ABCDEFGH_INPUT, [{$A, 3}, {$B, 3}, {$C, 3}, {$D, 3},
                              {$E, 3}, {$F, 2}, {$G, 4}, {$H, 4}]).
-define(TEST_ABCDEFGH_COUNT, #{2 => 1, 3 => 5, 4 => 2}).
-define(TEST_ABCDEFGH_OFS,   [{1, 0}, {2, 0}, {3, 2}, {4, 14}]).
-define(TEST_ABCDEFGH_CODES, [{$A, {3,  2}}, {$B, {3,  3}},
                              {$C, {3,  4}}, {$D, {3,  5}},
                              {$E, {3,  6}}, {$F, {2,  0}},
                              {$G, {4, 14}}, {$H, {4, 15}}]).
-endif.

maxv(KV) -> maxv(KV, 0).
maxv([{_, V}|T], Max) when V > Max -> maxv(T, V);
maxv([{_, _}|T], Max) -> maxv(T, Max);
maxv([], Max) -> Max.

decode(Lengths, Data) ->
  %?LOG_NOTICE("Lengths: ~p~nOffsets: ~p~nCodes: ~p", [Counts, Offsets, Codes]),
  %    loop until end of block:
  %      1. decode literal length value from input stream
  %      2a. if value < 256: copy literal byte to output
  %      2b. elif end of block (256)? break
  %      2c. elif 256 > value < 286:
  %             decode $distance from input stream
  %             move $distance bytes back in output
  %             copy LEN bytes from this pos to the output
  decode(Lengths, setup(Lengths), Data, [], 0).
decode(Lengths, Codes, Data, Symbols, Bits) ->
  case decode_symbol(Codes, Data) of
    {ok, {Len, _, 256}, Tail} ->
      {ok, list_to_binary(lists:reverse(Symbols)), Tail,
	   (Bits + Len) div 8 + case Bits + Len rem 8 of 0 -> 0; _ -> 1 end};
    {ok, {Len, _, Symbol}, Tail} when Symbol < 256 ->
      decode(Lengths, Codes, Tail, [Symbol | Symbols], Bits + Len);
    {ok, {Len, Code, Symbol}, _} ->
      {error, not_implemented, repetitions, {Len, Code, Symbol}}
  end.

-ifdef(TEST).

decode_test_() -> lists:concat([
  [
    ?_assertEqual(Expected, decode(flate:fixed(), In)) || {In, Expected} <- [
      {{<<0:7>>, <<>>}, {ok, <<>>, end_of_stream, 1}},
      {{<<>>, <<0>>},   {ok, <<>>, {<<0:1>>, <<>>}, 1}},
      {<<0>>,           {ok, <<>>, {<<0:1>>, <<>>}, 1}}
    ]
  ]
]).

-endif.

decode_symbol(Codes, Bin) when is_binary(Bin) ->
  decode_symbol(Codes, {<<>>, Bin});
decode_symbol(Codes, Bin) when is_bitstring(Bin) ->
  decode_symbol(Codes, {Bin, <<>>});
decode_symbol(Codes, {<<>>, <<C:1/binary, Tail/binary>>}) ->
  decode_symbol(Codes, {C, Tail});
decode_symbol(Codes, {C, Tail}) when is_integer(C) ->
  decode_symbol(Codes, {<<C:8/integer>>, Tail});
decode_symbol(Codes, {C, Tail}) ->
  decode_symbol(Codes, {0, 0}, {C, Tail}).

decode_symbol(_, {Len, _}, _) when Len > 15 ->
  {error, invalid_code};
decode_symbol(Codes, {Len, Cand}, {Bits, Data}) when bit_size(Bits) > 0 ->
  BitTailSize = bit_size(Bits) - 1,
  <<T:BitTailSize/bitstring, H:1>> = Bits,
  NewLen = Len + 1,
  NewCode = Cand bsl 1 + H,
  NewData = {T, Data},
  case lists:keyfind({NewLen, NewCode}, 2, Codes) of
    false ->
      decode_symbol(Codes, {NewLen, NewCode}, NewData);
    {Val, {NewLen, NewCode}} ->
      case NewData of
        {<<>>, <<>>} -> {ok, {NewLen, NewCode, Val}, end_of_stream};
        {<<>>, Bin} -> {ok, {NewLen, NewCode, Val}, Bin};
	{_, _} = Tail -> {ok, {NewLen, NewCode, Val}, Tail}
      end
  end;
decode_symbol(Codes, Cand, {<<>>, <<Byte:1/binary, Data/binary>>}) ->
  decode_symbol(Codes, Cand, {Byte, Data});
decode_symbol(_, _, {<<>>, <<>>}) ->
  {error, not_enough_data}.

-ifdef(TEST).

decode_test_symbols_test_() -> [
  ?_assertEqual(
    {ok, SymbolMatch, Tail},
    decode_symbol(?TEST_ABCDEFGH_CODES, Encoded)
  ) || {Encoded, SymbolMatch, Tail} <- [
    {<<2:3>>, {3, 2, $A}, end_of_stream}
  ]
].

decode_abcdefgh_symbols_test_() -> [
  ?_assertEqual(
    {ok, {Len, Code, Val}, end_of_stream},
    decode_symbol(?TEST_ABCDEFGH_CODES, <<Code:Len>>)) ||
      {Val, {Len, Code}} <- ?TEST_ABCDEFGH_CODES
].

decode_fixed_symbols_test() ->
  % This assumes a working setup(), otherwise we won't know what
  % we are looking up codes in. Also: make it a static test, not
  % a generator, because we don't want each symbol to count as a
  % specific test (or else about 90% of our test case count would
  % be this, already described as not so useful, test. It does
  % hold some value, since we get to see that it works nicely even
  % for real world code trees.
  Codes = setup(flate:fixed()),
  [
    ?assertEqual(
      {ok, {Len, Code, Val}, end_of_stream},
      decode_symbol(Codes, <<Code:Len>>)) ||
      {Val, {Len, Code}} <- Codes
  ].

-endif.

setup(Lengths) ->
  Counts = counts(Lengths),
  ok = check(Counts),
  {ok, Offsets} = offsets(lists:seq(1, maxv(Lengths)), Counts),
  codes(lists:seq(0, maxv(Offsets)), Lengths, Offsets).

-ifdef(TEST).

setup_test_() -> [
  ?_assertEqual(?TEST_ABCDEFGH_CODES, setup(?TEST_ABCDEFGH_INPUT))
].

-endif.

counts(L) -> counts(L, #{}).
counts([], Counts) -> Counts;
counts([{_, L}|Lens], Counts) ->
  counts(Lens, Counts#{L => maps:get(L, Counts, 0) + 1}).

-ifdef(TEST).

counts_test_() -> [
  ?_assertEqual(Expected, counts(Input)) || {Input, Expected} <- [
    {[], #{}},
    {flate:fixed(), #{7 => 24, 8 => 152, 9 => 112}},
    {?TEST_ABCDEFGH_INPUT, ?TEST_ABCDEFGH_COUNT}
  ]
].

-endif.

%   /* check for an over-subscribed or incomplete set of counts */
%   left = 1;                           /* one possible code of zero length */
%   for (len = 1; len <= MAXBITS; len++) {
%       left <<= 1;                     /* one more bit, double codes left */
%       left -= h->count[len];          /* deduct count from possible codes */
%       if (left < 0)
%           return left;                /* over-subscribed--return negative */
%   }                                   /* left > 0 means incomplete */
%
%huff_check(_Counts) ->
%  maps:to_list(Counts)
%
% https://github.com/madler/zlib/blob/21767c6/contrib/puff/puff.c#L355
% FIXME: stub, should work for fixed code tree :)
check(_Counts) -> ok.

-ifdef(TEST).

check_test_() -> [
  ?_assertEqual(Expected, check(Input)) || {Input, Expected} <- [
    {flate:fixed(), ok}
  ]
].

-endif.

%   offs[1] = 0;
%   for (len = 1; len < MAXBITS; len++)
%       offs[len + 1] = offs[len] + h->count[len];

offsets(Lengths, Counts) ->
  offsets(Lengths, [], Counts).
offsets([], Offsets, _) ->
  {ok, lists:reverse(Offsets)};
offsets([Len | Lengths], Offsets, Counts) ->
  offsets(Lengths, [{Len, offset_pos(Len, Counts)} | Offsets], Counts).

-ifdef(TEST).

offsets_test_() -> [
  ?_assertEqual(
    Expected,
    offsets(lists:seq(1, maxv(Lengths)), counts(Lengths))
  ) || {Lengths, Expected} <- [
    {[], {ok, []}},
    %{flate:fixed(), {ok, this_is_wrong}},
    {?TEST_ABCDEFGH_INPUT, {ok, ?TEST_ABCDEFGH_OFS}}
  ]
].

-endif.

offset_pos(0, _) -> 0;
offset_pos(Len, Counts) ->
  2 * (offset_pos(Len-1, Counts) + length_width(Len-1, Counts)).

-ifdef(TEST).

offset_pos_test_() -> [
  ?_assertEqual(0, offset_pos(1, counts(?TEST_ABCDEFGH_INPUT))),
  ?_assertEqual(0, offset_pos(2, counts(?TEST_ABCDEFGH_INPUT))),
  ?_assertEqual(2, offset_pos(3, counts(?TEST_ABCDEFGH_INPUT))),
  ?_assertEqual(14, offset_pos(4, counts(?TEST_ABCDEFGH_INPUT)))
].

-endif.

length_width(C, Counts) -> maps:get(C, Counts, 0).

-ifdef(TEST).

length_width_test_() -> [
  ?_assertEqual(0, length_width(1, counts(?TEST_ABCDEFGH_INPUT))),
  ?_assertEqual(1, length_width(2, counts(?TEST_ABCDEFGH_INPUT))),
  ?_assertEqual(5, length_width(3, counts(?TEST_ABCDEFGH_INPUT))),
  ?_assertEqual(2, length_width(4, counts(?TEST_ABCDEFGH_INPUT)))
].

-endif.

codes(Codes, Lengths, Offsets) ->
  codes(Codes, Lengths, Offsets, []).
codes([], _, _, Codes) -> lists:sort(Codes);
codes([N|Tail], Lengths, Offsets, Codes) ->
  case lists:keytake(N, 2, Lengths) of
    false ->
      %?LOG_NOTICE("[UNDEFINED]~nLengths: ~p~nCode: ~p", [Lengths, N]),
      codes(Tail, Lengths, Offsets, Codes);
    {value, {Value, N}, Rest} ->
      %?LOG_NOTICE("[DEFINED]~nLengths: ~p~nOffsets: ~p~nCode ~p = ~p", [Lengths, Offsets, N, Value]),
      {Next, RestOffsets} = reserve_code(Offsets, N),
      codes([N|Tail], Rest, RestOffsets, [{Value, {N, Next}} | Codes])
  end.

-ifdef(TEST).

code_test_() -> [
  ?_assertEqual(
    ?TEST_ABCDEFGH_CODES,
    codes(lists:seq(0, maxv(?TEST_ABCDEFGH_OFS)),
          ?TEST_ABCDEFGH_INPUT,
          ?TEST_ABCDEFGH_OFS)
  )
].

-endif.

% reserve_code/2: Takes a list, Offsets parameter, of shape
%   [{Length, Code},...],
% returns a Code and an updated Offsets list as a tuple,
%   {Code, [{Length, Code+1},...]},
% where Length = parameter Len, other elements are left as is.
reserve_code(Offsets, Len) ->
  Code = proplists:get_value(Len, Offsets),
  {Code, lists:keyreplace(Len, 1, Offsets, {Len, Code + 1})}.

-ifdef(TEST).

-define(reserve_code_test(Ofs, Len, New, Code),
        ?_assertEqual({Code, New}, reserve_code(Ofs, Len))).

reserve_code_test_() -> [
  ?reserve_code_test(Ofs, Len, New, Code) || {Ofs, Len, New, Code} <- [
    {[{1, 0}, {2, 0}, {3, 1}, {4, 7}], 2,
     [{1, 0}, {2, 1}, {3, 1}, {4, 7}], 0},
    {[{1, 0}, {2, 1}, {3, 2}, {4, 7}], 3,
     [{1, 0}, {2, 1}, {3, 3}, {4, 7}], 2},
    {[{1, 0}, {2, 1}, {3, 2}, {4, 7}], 4,
     [{1, 0}, {2, 1}, {3, 2}, {4, 8}], 7}
  ]
].

-endif.
