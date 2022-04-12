-module(srcd_utils).

-export([cmd_split/1]).

cmd_split(Cmd) ->
  cmd_split(Cmd, "", false, false, []).

cmd_split([], This, false, false, Res) ->
  {ok, lists:reverse([lists:reverse(This)|Res])};
cmd_split([], _, T, _, _) when T =/= false ->
  error;
cmd_split([], _, _, true, _) ->
  error;
cmd_split([$\ |Cmd], This, false, false, Res) ->
  cmd_split(Cmd, "", false, false, [lists:reverse(This)|Res]);
cmd_split([$\n|Cmd], This, false, false, Res) ->
  cmd_split(Cmd, "", false, false, [lists:reverse(This)|Res]);
cmd_split([$'|Cmd], This, false, false, Res) ->
  cmd_split(Cmd, This, $', false, Res);
cmd_split([$'|Cmd], This, $', false, Res) ->
  cmd_split(Cmd, This, false, false, Res);
cmd_split([$"|Cmd], This, false, false, Res) ->
  cmd_split(Cmd, This, $", false, Res);
cmd_split([$"|Cmd], This, $", false, Res) ->
  cmd_split(Cmd, This, false, false, Res);
cmd_split([$\\|Cmd], This, Q, false, Res) ->
  cmd_split(Cmd, This, Q, true, Res);
cmd_split([Ch|Cmd], This, Q, true, Res) ->
  cmd_split(Cmd, [Ch|This], Q, false, Res);
cmd_split([Ch|Cmd], This, Q, false, Res) ->
  cmd_split(Cmd, [Ch|This], Q, false, Res).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(_ok(V, In), ?_assertEqual({ok, V}, cmd_split(In))).

cmd_split_test_() ->
  [
    ?_ok([[]], ""),
    ?_ok(["true"], "true"),
    ?_ok(["echo", "hej"], "echo hej"),
    ?_ok(["echo", "hej\\"], "echo hej\\\\"),
    ?_ok(["echo", "foo bar"], "echo foo\\ bar"),
    ?_ok(["echo", "foo bar"], "echo \"foo bar\""),
    ?_ok(["echo", "foo bar"], "echo 'foo bar'"),
    ?_ok(["echo", "foobar baz"], "echo foo'bar baz'"),
    ?_ok(["echo", "foo bar baz"], "echo foo' bar baz'"),
    ?_ok(["echo", "foo", "bar", "baz"], "echo foo bar baz"),
    ?_assertEqual(error, cmd_split("echo 'foo bar")),
    ?_assertEqual(error, cmd_split("echo \"foo bar"))
  ].
-endif.
