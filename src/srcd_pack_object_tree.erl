-module(srcd_pack_object_tree).
-export([deps/1, build/1]).

-include_lib("kernel/include/logger.hrl").
-include("srcd_object.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% TODO: support non-empty tree objects
deps(#tree{items=Items}) -> [Oid || #tree_node{object=Oid} <- Items].
build(#tree{items=Items}) ->
  lists:concat([format_entry(Node) || Node <- Items]).

format_entry(#tree_node{mode=Mode, name=Name, object=Oid}) ->
  lists:concat([Mode, " ", Name, "\0", srcd_utils:hex_to_bin_sha1(Oid)]).

-ifdef(TEST).

empty_test_() ->
  [
    ?_assertEqual([], deps(#tree{items=[]})),
    ?_assertEqual([], build(#tree{items=[]}))
  ].

single_blob_test_() ->
  Entries = [
    #tree_node{
      mode="100644",
      name="test",
      object="da39a3ee5e6b4b0d3255bfef95601890afd80709"
    }
  ],
  BinHex = srcd_utils:hex_to_bin_sha1("da39a3ee5e6b4b0d3255bfef95601890afd80709"),

  [
    ?_assertEqual(["da39a3ee5e6b4b0d3255bfef95601890afd80709"],
                  deps(#tree{items=Entries})),
    ?_assertEqual("100644 test\0" ++ BinHex, build(#tree{items=Entries}))
  ].

-endif.
