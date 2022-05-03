-module(srcd_pack_object_commit).
-export([deps/1, build/1]).

-include_lib("kernel/include/logger.hrl").
-include("srcd_object.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

deps(#commit{tree=Tree, parents=Parents}) -> [Tree|Parents].

build(C = #commit{msg=Msg}) ->
  Header = string:join(lists:concat([
    tree_line(C),
    parent_lines(C),
    author_line(C),
    committer_line(C)
  ]), "\n"),
  string:join([Header, Msg], "\n\n").

tree_line(#commit{tree=Tree}) ->
  ["tree " ++ Tree].

parent_lines(#commit{parents=Parents}) ->
  ["parent " ++ P || P <- Parents].

author_line(#commit{author=Author}) ->
  ["author " ++ format_author(Author)].

committer_line(#commit{committer=Committer}) ->
  ["committer " ++ format_author(Committer)].

format_author(Stamp = #stamp{name=Name, email=Email, time=T, tz=Tz}) ->
  lists:flatten(io_lib:format("~s <~s> ~b ~s", [Name, Email, T, Tz])).

-ifdef(TEST).

-define(STAMP, #stamp{
  name="Unit Test",
  email="ceo@example.com",
  time=1651498600,
  tz="+0200"
}).

no_parents_test_() ->
  Commit = #commit{
    tree="4b825dc642cb6eb9a060e54bf8d69288fbee4904",
    author=?STAMP,
    committer=?STAMP,
    msg="test\n"
  },

  [
    ?_assertEqual(
      string:join([
        "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904",
        "author Unit Test <ceo@example.com> 1651498600 +0200",
        "committer Unit Test <ceo@example.com> 1651498600 +0200",
        "",
        "test\n"
      ], "\n"),
      build(Commit)
    ),
    ?_assertEqual(["4b825dc642cb6eb9a060e54bf8d69288fbee4904"], deps(Commit))
  ].

single_parent_test_() ->
  Commit = #commit{
    tree="4b825dc642cb6eb9a060e54bf8d69288fbee4904",
    author=?STAMP,
    committer=?STAMP,
    parents=["4bd8bdbdc6661187350f6e6141577c3d7cda1ac6"],
    msg="test\n"
  },

  [
    ?_assertEqual(
      string:join([
        "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904",
        "parent 4bd8bdbdc6661187350f6e6141577c3d7cda1ac6",
        "author Unit Test <ceo@example.com> 1651498600 +0200",
        "committer Unit Test <ceo@example.com> 1651498600 +0200",
        "",
        "test\n"
      ], "\n"),
      build(Commit)
    ),
    ?_assertEqual(
      [
        "4b825dc642cb6eb9a060e54bf8d69288fbee4904",
        "4bd8bdbdc6661187350f6e6141577c3d7cda1ac6"
      ],
      deps(Commit)
    )
  ].

two_parents_test_() ->
  Commit = #commit{
    tree="4b825dc642cb6eb9a060e54bf8d69288fbee4904",
    author=?STAMP,
    committer=?STAMP,
    parents=["4bd8bdbdc6661187350f6e6141577c3d7cda1ac6",
             "c64283c48a44496dd0a52fa2850728efefd81df8"],
    msg="test\n"
  },

  [
    ?_assertEqual(
      string:join([
        "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904",
        "parent 4bd8bdbdc6661187350f6e6141577c3d7cda1ac6",
        "parent c64283c48a44496dd0a52fa2850728efefd81df8",
        "author Unit Test <ceo@example.com> 1651498600 +0200",
        "committer Unit Test <ceo@example.com> 1651498600 +0200",
        "",
        "test\n"
      ], "\n"),
      build(Commit)
    ),
    ?_assertEqual(
      [
        "4b825dc642cb6eb9a060e54bf8d69288fbee4904",
        "4bd8bdbdc6661187350f6e6141577c3d7cda1ac6",
        "c64283c48a44496dd0a52fa2850728efefd81df8"
      ],
      deps(Commit)
    )
  ].

build_three_parents_test_() ->
  Commit = #commit{
    tree="4b825dc642cb6eb9a060e54bf8d69288fbee4904",
    author=?STAMP,
    committer=?STAMP,
    parents=["4bd8bdbdc6661187350f6e6141577c3d7cda1ac6",
             "c64283c48a44496dd0a52fa2850728efefd81df8",
             "c64283c48a44496dd0a52fa2850728efefd81df9"],
    msg="test\n"
  },

  [
    ?_assertEqual(
      string:join([
        "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904",
        "parent 4bd8bdbdc6661187350f6e6141577c3d7cda1ac6",
        "parent c64283c48a44496dd0a52fa2850728efefd81df8",
        "parent c64283c48a44496dd0a52fa2850728efefd81df9",
        "author Unit Test <ceo@example.com> 1651498600 +0200",
        "committer Unit Test <ceo@example.com> 1651498600 +0200",
        "",
        "test\n"
      ], "\n"),
      build(Commit)
    ),
    ?_assertEqual(
      [
        "4b825dc642cb6eb9a060e54bf8d69288fbee4904",
        "4bd8bdbdc6661187350f6e6141577c3d7cda1ac6",
        "c64283c48a44496dd0a52fa2850728efefd81df8",
        "c64283c48a44496dd0a52fa2850728efefd81df9"
      ],
      deps(Commit)
    )
  ].

-endif.
