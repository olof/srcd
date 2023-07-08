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
-include("tests/zlib.trl").
-endif.

% Note: deflate takes a full data blob and compresses, unlike inflate that
%       takes an iodevice and uncompresses the stream. This is not a public
%       api so that's fine :)

deflate(Data) ->
  Z = zlib:open(),
  ok = zlib:'deflateInit'(Z, default),
  IoList = zlib:deflate(Z, Data, finish),
  ok = zlib:'deflateEnd'(Z),
  srcd_utils:iolist_to_list(IoList).

inflate(IoDevice) ->
  ?LOG_NOTICE("Inflate object"),
  inflate_reader({fun (_, Len) -> io:get_chars(IoDevice, "", Len) end, 0}).

inflate_reader({Reader, Pos}) ->
  % a zlib header is two bytes; by reading one byte here, we know that the zlib
  % header has been read in full as inflate_reader/7 will also read one byte.
  inflate_reader({Reader, Pos + 1}, Reader(Pos, 1)).
inflate_reader(Reader, PreBuf) ->
  %?LOG_NOTICE("Inflate with prebuf: ~p", [PreBuf]),
  Z = zlib:open(),
  ok = zlib:'inflateInit'(Z),
  {ok, Len, Data, Compressed} = inflate_reader(Z, Reader, [], PreBuf,
                                               [], length(PreBuf), 1),
  zlib:close(Z),
  {ok, Len, Data, Compressed}.
inflate_reader(Z, {Reader, Pos}, InProc, InUnproc, Out, ReadCount0, N) ->
  %?LOG_NOTICE("Round ~p, processed: ~p, unprocessed: ~p",
  %            [N, length(InProc), length(InUnproc)]),
  NewBuf = InUnproc ++ Reader(Pos, 1),
  %?LOG_NOTICE("Inflate round with buf: ~p", [NewBuf]),
  ReadCount = ReadCount0 + 1,
  NewPos = Pos + 1,
  case catch zlib:inflate(Z, NewBuf) of
    {'EXIT', {data_error, _}} ->
      inflate_reader(Z, {Reader, NewPos}, InProc,
                     NewBuf, Out, ReadCount, N + 1);
    {'EXIT', Err} -> ?LOG_NOTICE("unexpected exit inflate: ~p", [Err]),
                     {ok, ReadCount0, Out, InProc ++ NewBuf};
    [] ->
      case catch zlib:'inflateEnd'(Z) of
        {'EXIT', {data_error, _}} ->
          inflate_reader({Reader, NewPos}, InProc ++ NewBuf);
        ok ->
          {ok, ReadCount, Out, InProc ++ NewBuf}
      end;
    New when is_list(New) ->
      case catch zlib:'inflateEnd'(Z) of
        {'EXIT', {data_error, _}} ->
          inflate_reader({Reader, NewPos}, InProc ++ NewBuf);
        ok -> {ok, ReadCount,
               Out ++ srcd_utils:iolist_to_list(New), InProc ++ NewBuf}
      end
  end.
