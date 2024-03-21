% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-

-module(flatez).
% This module tries to implement RFC 1950, to be able to support
% inflating compressed objects.

-export([in/1, in/2, in/3, de/1, tail/1, stats/1]).

%%%% Inflating a compressed blob:
% {more, Context2} = flate:in(Part1),
%
% % We got back a 'more' atom, let's feed it more data!
% {more, Context1} = flate:in(Context2, Part2),
% {more, Context0} = flate:in(Context1, Part3),
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

-include("record.hrl").
-include("check.hrl").

in(Data)       -> in(Data, []).
in(Data, Opts) ->
  ReadHook = proplists:get_value(read_hook, Opts, fun (_) -> ok end),
  <<HDR:2/binary, Tail/binary>> = Data,
  0 = flate_utils:b2i(HDR) rem 31,

  % deflate is 8, and only thing we support.
  <<CMF:1/binary, FLG:1/binary>> = HDR,
  <<FLGVal:4, CMFVal:4>> = CMF,
  <<CINFO:4, CM:4>> = CMF,
  <<_FLEVEL:2, FDICT:1, _FCHECK:5>> = FLG,
  8 = CM,

  Win = crypto:mod_pow(2, CINFO + 8, 16#7fff),
  % TODO: need to pass Win to flate:in
  %io:format(standard_error, "zlib tail: ~.2B~n", [Tail]),
  case flate:in(#zlib{op=in, read_count=2}, Tail, Opts) of
    {more, Bytes, Ctx} -> {more, Bytes, Ctx};
    {ok, Res, Ctx} -> finalize(Res, Ctx)
  end.
in(#zlib{state=finalized, output=Res} = Ctx, Data, Opts) ->
  finalize(Res, Ctx#zlib{input=Data, output=undefined});
in(Ctx, Data, Opts) ->
  case flate:in(Ctx, Data, Opts) of
    {more, Bytes, NewCtx} ->
      {more, Bytes, NewCtx};
    {ok, Res, NewCtx} ->
      finalize(Res, NewCtx)
  end.
de(Data) -> flate:de(Data).
tail(Ctx) -> flate:tail(Ctx).
stats(Ctx) -> flate:stats(Ctx).

finalize(Res, #zlib{input=Input} = Ctx) when size(Input) < 4 ->
  {more, 4-size(Input), Ctx#zlib{output=Res}};
finalize(Res, #zlib{input= <<Checksum:32/bits, Tail/binary>>,
                    read_count=Rc} = Ctx) ->
  {flate_adler32:check(Res, Checksum), Res,
   Ctx#zlib{read_count=Rc+4, input=Tail}}.

-ifdef(TEST).
?check_full_inflate(zopfli_empty_inflation_test_,
                    <<2#01111000, 2#11011010, 3, 0, 0, 0, 0, 1>>,
                    <<>>).
?check_full_inflate(pigz_empty_inflation_test_,
                    <<2#01111000, 2#01011110, 3, 0, 0, 0, 0, 1>>,
                    <<>>).
?check_full_inflate(zopfli_a_inflation_test_,
                    <<2#01111000, 2#11011010, 115, 4, 0, 0, 66, 0, 66>>,
                    <<"A">>).
?check_full_inflate(pigz_a_inflation_test_,
                    <<2#01111000, 2#01011110, 115, 4, 0, 0, 66, 0, 66>>,
                    <<"A">>).
?check_full_inflate(pigz_aaa_inflation_test_,
                    <<2#01111000, 2#01011110, 115, 116, 116, 4, 0, 1, 137, 0, 196>>,
                    <<"AAA">>).
?check_full_inflate('pigz_?x1040_inflation_test_',
                    <<2#01111000, 2#01011110, 179, 183, 31, 5, 163, 96, 20, 140, 2, 8, 0, 0, 130, 0, 0, 0>>,
                    list_to_binary(lists:duplicate(1040, "?"))).
-endif.
