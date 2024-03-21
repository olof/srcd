% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-

-module(flate_adler32).
% This module implements the adler32 checksum function.

-export([checksum/1, check/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MOD, 65521).

checksum(Data) when is_binary(Data) ->
  checksum(binary_to_list(Data));
checksum([]) -> 1;
checksum([H|Data]) ->
  [S|_] = L = lists:foldl(fun(N, [X|_] = Acc) -> [(N+X) rem ?MOD|Acc] end,
                          [0], [H+1 | Data]),
  (lists:sum(L) rem ?MOD) bsl 16 + S.

check(Data, <<Checksum:32/integer>>) -> check(Data, Checksum);
check(Data, Checksum) -> case checksum(Data) of
  Checksum -> ok;
  _ -> {error, checksum_mismatch}
end.

-ifdef(TEST).

int(N) when is_integer(N) -> N;
int(<<N:32/integer>>) -> N.

known_checksum_test_() -> lists:concat([
  [
    ?_assertEqual(int(Checksum), checksum(Input)),
    ?_assertEqual(int(Checksum), checksum(Input)),
    ?_assertEqual(int(Checksum), checksum(list_to_binary(Input))),
    ?_assertEqual(int(Checksum), checksum(list_to_binary(Input))),
    ?_assertEqual(ok, check(Input, Checksum))
  ] || {Input, Checksum} <- [
    {"", 1},
    {"W", 5767256},
    {"Wikipedia", 300286872},           % well documented encyclopedic fact!
    {"test", 73204161},
    {lists:duplicate(1040, $?), 2181038080} % not as well known, but SO true
  ]
]).

equivalent_test_() -> [
  ?_assertEqual(A, B) || {A, B} <- [
    {checksum(<<"A">>), checksum("A")},
    {check(<<"A">>, checksum(<<"A">>)), check("A", checksum("A"))}
  ]
].

check_pass_test_() -> [
  ?_assertEqual(ok, check("A", checksum(<<"A">>))),
  ?_assertEqual(ok, check(<<"A">>, checksum("A")))
].

check_fail_test_() -> [
  ?_assertEqual({error, checksum_mismatch}, check(<<"B">>, checksum(<<"A">>)))
].

-endif.
