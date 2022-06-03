% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_zlib).
-export([inflate/1, test/0]).

% I don't like git anymore ♥
% Reads one byte at a time, tries to inflateEnd, sees if it throws
% an exception, and if not... maybe we're good? or if we get an exception
% restart the stream (refeeding the compressed data we've seen so far).
% please hire me to do your netcodes!

-include_lib("kernel/include/logger.hrl").

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
  ok = zlib:inflateInit(Z),
  {ok, Len, Data, Compressed} = inflate_reader(Z, Reader, [], PreBuf, [], length(PreBuf), 1),
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
      case catch zlib:inflateEnd(Z) of
        {'EXIT', {data_error, _}} ->
          inflate_reader({Reader, NewPos}, InProc ++ NewBuf);
        ok -> {ok, ReadCount, Out, InProc ++ NewBuf}
      end;
    [New] ->
      inflate_reader(Z, {Reader, NewPos}, InProc ++ NewBuf, [],
                     Out ++ binary_to_list(New), ReadCount, N+1)
  end.

-define(TEST_Z, [16#78, 16#9c, 16#2b, 16#49, 16#2d, 16#2e,
                 16#01, 16#00, 16#04, 16#5d, 16#01, 16#c1]).
-define(TEST_Z2, ?TEST_Z ++ ?TEST_Z).
-define(TEST_Z2S, ?TEST_Z ++ " " ++ ?TEST_Z).

test() ->
  ok = test_1(),
  ?LOG_NOTICE("test 1 ok"),
  ok = test_2(),
  ?LOG_NOTICE("test 2 ok"),
  ok = test_3(),
  ?LOG_NOTICE("test 3 ok"),
  ok = test_4(),
  ?LOG_NOTICE("test 4 ok"),
  ?LOG_NOTICE("all tests ok"),
  ok.

test_reader(Buf, Pos, Len) ->
  case lists:split(Pos, Buf) of
    {_, []} -> eof;
    {_, Bytes} ->
      {Read, _} = lists:split(Len, Bytes),
      Read
  end.

test_blob(Blob, Test, InitPos, Expect) ->
  Reader = fun (Pos, Len) -> test_reader(Test, Pos, Len) end,
  TestLen = length(Expect),
  case inflate_reader({Reader, InitPos}) of
    {ok, TestLen, Blob, Expect} -> ok;
    X -> not_ok
  end.

test_1() -> test_blob("test", ?TEST_Z, 0, ?TEST_Z).
test_2() ->
  test_blob("test", ?TEST_Z2, 0, ?TEST_Z),
  test_blob("test", ?TEST_Z2, length(?TEST_Z), ?TEST_Z).
test_3() ->
  test_blob("test", ?TEST_Z2S, 0, ?TEST_Z),
  test_blob("test", ?TEST_Z2S, length(?TEST_Z) + 1, ?TEST_Z).

test_4() ->
  {ok, Fh0} = file:open("/home/olof/packfile.wireshark.z", [read]),
  {ok, Size, Object, _} = inflate(Fh0),
  Fh1 = Fh0,
  Header = io:get_chars(Fh1, "", 2),
  {ok, Len, Tree0, _} = inflate(Fh1),
  Tree = string:split(Tree0, "\0"),
  Len = 41,
  Tree = ["100644 file", [48, 215, 77, 37, 132, 66, 199, 198, 85, 18,
                          234, 250, 180, 116, 86, 141, 215, 6, 196, 48]],

  Header3 = io:get_chars(Fh1, "", 1),
  {ok, 12, "test", [16#78, 16#9c, 16#2b, 16#49, 16#2d, 16#2e,
                    16#01, 16#00, 16#04, 16#5d, 16#01, 16#c1]} = inflate(Fh1),
  file:close(Fh1).
