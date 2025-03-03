% ex:ts=2:sw=2:sts=2:et:foldmarker=-ifdef(TEST).,-endif.:foldmethod=marker
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-

-module(flate_lz77).
-export([decode/3, lazy_decode/2, resolve/2, resolve_all/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([clone_output/3, decode_distance_pair/2]).
-endif.

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

decode_distance_pair(Code, Data) ->
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
  case flate_utils:read_bits(Data, ExtraBits) of
    {error, insufficient_data} ->
      {more, flate_utils:reverse_int(ExtraBits) div 8 + 1};
    {Extra, Tail} ->
      {Len + flate_utils:reverse_int(Extra, ExtraBits), Tail, ExtraBits}
  end.

decode_distance(Data) ->
  case flate_utils:read_bits(Data, 5, []) of
    {error, insufficient_data} -> {more, 1};
    {RevCode, Extras} ->
      Code = flate_utils:reverse_int(RevCode, 5),
      Base = distance_base(Code),
      {Code, ExtraBits} = distance_extra_bits(Code),
      case flate_utils:read_bits(Extras, ExtraBits) of
        {error, insufficient_data} -> {more, 1};
        {Extra, Tail} ->
          {Base + flate_utils:b2i(Extra) + 1, Tail, 5 + ExtraBits}
      end
  end.

decode(Symbols, Code, Data) ->
  case lazy_decode(Code, Data) of
    {more, N} -> {more, N};
    {ok, Instr, Tail, Read} ->
      case resolve(Symbols, Instr) of
        {error, Reason} -> {error, Reason};
        Output -> {ok, Output, Tail, Read}
      end
  end.

lazy_decode(Code, Data) ->
  case decode_distance_pair(Code, Data) of
    {more, N} -> {more, N};
    {Length, Dist, Tail, Read} -> {ok, {lz77, Dist, Length}, Tail, Read}
  end.

resolve_all(Syms) -> resolve_all(Syms, []).
resolve_all([], Output) -> Output;
resolve_all([Sym|Symbols], Output) ->
  case resolve(Output, Sym) of
    {error, Reason} -> {error, Reason};
    Resolved -> resolve_all(Symbols, Output ++ Resolved)
  end.

-ifdef(TEST).
resolve_all_test_() -> [
  ?_assertEqual([], resolve_all([])),
  ?_assertEqual([1], resolve_all([1])),
  ?_assertEqual([1, 2, 3, 2], resolve_all([1, 2, 3, {lz77, 2, 1}])),
  ?_assertEqual([1, 2, 3, 2, 3], resolve_all([1, 2, 3, {lz77, 2, 2}])),
  ?_assertEqual([1, 2, 3, 2, 3, 2], resolve_all([1, 2, 3, {lz77, 2, 3}]))
].
-endif.

resolve(Symbols, {lz77, Dist, Length}) ->
  clone_output(lists:flatten(Symbols), Dist, Length);
resolve(_, Sym) -> [Sym].

clone_output(Symbols, Dist, _) when Dist > length(Symbols) ->
  {error, {lz77_distance_too_far_back, Dist, length(Symbols)}};
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

-ifdef(DEAD_CODE).
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
-endif.
