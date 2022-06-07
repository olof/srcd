% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_zlib).
-export([deflate/1, inflate/1]).

-elvis([{elvis_style, no_catch_expressions, disable}]).

% I don't like git anymore ♥
% Reads one byte at a time, tries to inflateEnd, sees if it throws
% an exception, and if not... maybe we're good? or if we get an exception
% restart the stream (refeeding the compressed data we've seen so far).
% please hire me to do your netcodes!

-include_lib("kernel/include/logger.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Note: deflate takes a full data blob and compresses, unlike inflate that
%       takes an iodevice and uncompresses the stream. This is not a public
%       api so that's fine :)

deflate(Data) ->
  Z = zlib:open(),
  ok = zlib:'deflateInit'(Z, default),
  [X] = zlib:deflate(Z, Data, finish),
  ok = zlib:'deflateEnd'(Z),
  binary_to_list(X).

inflate(IoDevice) ->
  ?LOG_NOTICE("Inflate object"),
  inflate_reader({fun (_, Len) -> io:get_chars(IoDevice, "", Len) end, 0}).

inflate_reader({Reader, Pos}) ->
  % a zlib header is two bytes; by reading one byte here, we know that the zlib
  % header has been read in full as inflate_reader/7 will also read one byte.
  inflate_reader({Reader, Pos+1}, Reader(Pos, 1)).
inflate_reader(Reader, PreBuf) ->
  %?LOG_NOTICE("Inflate with prebuf: ~p", [PreBuf]),
  Z = zlib:open(),
  ok = zlib:'inflateInit'(Z),
  {ok, Len, Data, Compressed} = inflate_reader(Z, Reader, [], PreBuf,
                                               [], length(PreBuf), 1),
  case lists:split(2, Compressed) of
    {[120, 156], _} ->
      CLen = length(Compressed),
      {CLen, Len} = {Len, CLen},
      ok = zlib:close(Z),
      {ok, Len, Data, Compressed};
    {Prefix, Tail} ->
      ?LOG_NOTICE("Unexpected header ~p~nBefore: ~p~nAfter: ~p~nData: ~p",
                  [Prefix, Compressed, Tail, Data]),
      {error, bad_zlib}
  end.
inflate_reader(Z, {Reader, Pos}, InProc, InUnproc, Out, ReadCount0, N) ->
  %?LOG_NOTICE("Round ~p, processed: ~p, unprocessed: ~p",
  %            [N, length(InProc), length(InUnproc)]),
  NewBuf = InUnproc ++ Reader(Pos, 1),
  %?LOG_NOTICE("Inflate round with buf: ~p", [NewBuf]),
  ReadCount = ReadCount0 + 1,
  NewPos = Pos + 1,
  case catch zlib:inflate(Z, NewBuf) of
    {'EXIT', {data_error, _}} ->
      inflate_reader(Z, {Reader, NewPos}, InProc, NewBuf, Out, ReadCount, N+1);
    {'EXIT', Err} -> ?LOG_NOTICE("unexpected exit inflate: ~p", [Err]),
                     {ok, ReadCount0, Out, InProc ++ NewBuf};
    [] ->
      %?LOG_NOTICE("Got nothing back! Maybe I'm done?"),
      case catch zlib:'inflateEnd'(Z) of
        {'EXIT', {data_error, _}} ->
          inflate_reader({Reader, NewPos}, InProc ++ NewBuf);
        ok -> {ok, ReadCount, Out, InProc ++ NewBuf}
      end;
    [New] ->
      inflate_reader(Z, {Reader, NewPos}, InProc ++ NewBuf, [],
                     Out ++ binary_to_list(New), ReadCount, N+1)
  end.

-ifdef(TEST).

% string "test", deflated
-define(TEST_OBJ, [16#78, 16#9c, 16#2b, 16#49, 16#2d, 16#2e,
                   16#01, 16#00, 16#04, 16#5d, 16#01, 16#c1]).

% a git packfile, consisting of a commit, a tree and a blob object + headers.
% used to make sure our zlib wrapper doesn't get confused and respects its
% boundaries (no over/under reads). Additional variants of this may become
% necessary.
-define(TEST_PACK_HDR, [$P,  $A,  $C,  $K,
                         0,   0,   0,   2,
                         0,   0,   0,   3]).
-define(TEST_PACK_COMMIT_HDR, [148, 13]).
-define(TEST_PACK_COMMIT, [
  120, 156, 149, 203,  59,  14, 194,  48,  12,   0, 208,  61, 167, 200, 142,
  132,  98, 231, 215,  72,   8,  49, 179, 112,   6,  39, 118,  40,  18, 109,
  170,  38, 189,  63, 189,   2, 227,  27, 222, 216,  69, 116,  77,  57, 122,
   14, 181, 112, 100,  74, 214,  36, 143,  19, 178,  55,  82,  75,  76,   1,
   38, 176, 236,  81,  44, 171, 141, 118,  89, 135, 118, 228, 153, 179,  69,
    7,   5, 240,  44, 132, 177,  84, 200,  20,  29, 113,  38, 168,  56, 185,
   26,  66, 178, 138, 142,  49, 183,  93, 191, 190, 173, 234, 103, 155, 105,
  237, 189, 173, 250, 214,  78,  63, 100, 204, 199, 118, 237, 114, 215,  16,
   60,  34, 122,  23, 157, 190,  24,  52,  70, 149, 182,  44, 159,  49, 228,
  255, 169, 150, 254,  86,  63, 175,  54,  63, 177
]).
-define(TEST_PACK_COMMIT_OBJ, ?TEST_PACK_COMMIT_HDR ++ ?TEST_PACK_COMMIT).
-define(TEST_PACK_COMMIT_START, length(?TEST_PACK_HDR) +
                                length(?TEST_PACK_COMMIT_HDR)).

-define(TEST_PACK_TREE_HDR, [160, 2]).
-define(TEST_PACK_TREE, [
  120, 156,  51,  52,  48,  48,  51,  49,  81,  72, 203, 204,  73, 101,  48,
  184, 238, 171, 218, 226, 116, 252,  88, 168, 208, 171,  95,  91,  74, 194,
  122, 175, 179,  29,  49,   0,   0, 178, 115,  12, 227
]).
-define(TEST_PACK_TREE_OBJ, ?TEST_PACK_TREE_HDR ++ ?TEST_PACK_TREE).
-define(TEST_PACK_TREE_START, ?TEST_PACK_COMMIT_START +
                              length(?TEST_PACK_COMMIT) +
                              length(?TEST_PACK_TREE_HDR)).

-define(TEST_PACK_BLOB_HDR, [52]).
-define(TEST_PACK_BLOB, [120, 156, 43, 73, 45, 46, 1, 0, 4, 93, 1, 193]).
-define(TEST_PACK_BLOB_OBJ, ?TEST_PACK_BLOB_HDR ++ ?TEST_PACK_BLOB).
-define(TEST_PACK_BLOB_START, ?TEST_PACK_TREE_START +
                              length(?TEST_PACK_TREE) +
                              length(?TEST_PACK_BLOB_HDR)).

-define(TEST_PACK, lists:concat([
   ?TEST_PACK_HDR,
   ?TEST_PACK_COMMIT_OBJ,
   ?TEST_PACK_TREE_OBJ,
   ?TEST_PACK_BLOB_OBJ
])).

-record(test, {buf, plaintext, compressed, line, pos=0, result=ok}).
-define(test(Plaintext, Buf, Compressed),
        #test{buf=Buf,
              plaintext=Plaintext,
              compressed=Compressed,
              line=?LINE}).
-define(test(Plaintext, Buf, Compressed, Pos),
        #test{buf=Buf,
              plaintext=Plaintext,
              compressed=Compressed,
              pos=Pos,
              line=?LINE}).

zlib_test_() -> [
  {Line, fun () -> ?assertEqual({ok, Expect}, test_blob(Buf, Pos, Obj)) end} ||
  #test{buf=Buf, plaintext=Expect, compressed=Obj, pos=Pos, line=Line} <- [
    ?test("test", ?TEST_OBJ, ?TEST_OBJ),

    ?test("test", ?TEST_OBJ ++ ?TEST_OBJ, ?TEST_OBJ),
    ?test("test", ?TEST_OBJ ++ ?TEST_OBJ, ?TEST_OBJ, length(?TEST_OBJ)),

    ?test("test", ?TEST_OBJ ++ "x" ++ ?TEST_OBJ, ?TEST_OBJ),
    ?test("test", ?TEST_OBJ ++ "x" ++ ?TEST_OBJ, ?TEST_OBJ,
          length(?TEST_OBJ) + 1),

    ?test("test", ?TEST_OBJ ++ "x" ++ ?TEST_OBJ ++ "tail", ?TEST_OBJ),
    ?test("test", ?TEST_OBJ ++ "x" ++ ?TEST_OBJ ++ "tail", ?TEST_OBJ,
          length(?TEST_OBJ) + 1),

    ?test(string:join(
      [
        "tree f9b75d6fcd7da93095282d50efc7961813d52e3d",
        "parent 4a5ddb3241c127daa27cf1ba74adba1f284f6693",
        "author Olof Johansson <olof@ethup.se> 1652225474 +0200",
        "committer Olof Johansson <olof@ethup.se> 1652225474 +0200",
        "",
        "msg",
        ""
      ], "\n"),
      ?TEST_PACK, ?TEST_PACK_COMMIT, ?TEST_PACK_COMMIT_START
    ),
    ?test("100644 file" ++ [
            0, 48, 215, 77, 37, 132, 66, 199, 198, 85, 18,
            234, 250, 180, 116, 86, 141, 215, 6, 196, 48],
          ?TEST_PACK,
          ?TEST_PACK_TREE,
          ?TEST_PACK_TREE_START),
    ?test("test", ?TEST_PACK, ?TEST_PACK_BLOB, ?TEST_PACK_BLOB_START)
  ]].

test_reader(Buf, Pos, Len) ->
  case lists:split(Pos, Buf) of
    {_, []} -> eof;
    {_, Bytes} ->
      {Read, _} = lists:split(Len, Bytes),
      Read
  end.

test_blob(Buf, InitPos, Expect) ->
  Reader = fun (Pos, Len) -> test_reader(Buf, Pos, Len) end,
  TestLen = length(Expect),
  case inflate_reader({Reader, InitPos}) of
    {ok, TestLen, Blob, Expect} -> {ok, Blob};
    {ok, GotLen, GotBlob, GotCompressed} -> {
      not_ok, {unexpected_result, [
        {blob, GotBlob},
        {compressed_len, {GotLen, GotLen-TestLen}},
        {compressed, GotCompressed}
      ]}};
    X -> {not_ok, X}
  end.

-endif.
