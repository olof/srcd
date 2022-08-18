% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(flate).

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

-export([in/1, in/2, de/1, tail/1, stats/1]).
-record(zlib, {op, in, de, state, read_count=0, write_count=0}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(op(Name),
Name(#zlib{op=Name} = State) -> route(Name, State);
Name(#zlib{op=Op}) -> {badarg, op, Op};
Name(Data) -> Name(#zlib{op=?FUNCTION_NAME, Name=Data, state=done})).

?op(de).
in(State = #zlib{de=De}, Data) when is_list(De) ->
  deflate(State#zlib{de=lists:reverse([Data | De])});
in(State = #zlib{de=De}, Data) ->
  deflate(State#zlib{de=[De, Data]}).
?op(in).

% route() is a hack, to workaround a limitation with erlang preprocessor; i
% couldn't call a function called Name?MODULE when Name was a macro parameter.
% This saddened me.
route(in, State) -> inflate(State);
route(de, State) -> deflate(State).

inflate(#zlib{in= <<>>, state=data} = Ctx) -> {more, Ctx};
inflate(#zlib{in= <<>>, de=Dec} = Ctx) -> {ok, Dec, finalize(Ctx, 0, 0)};
inflate(#zlib{in=Enc, de=Dec} = Ctx) ->
  % parse code tree, parse compressed bytes
  % ONE BIT: start of block, BFINAL
  % TWO BITS: type of block, BTYPE
  % case int_to_btype(Btype) of
  %   no_compression ->
  %      % 1. skip to byte boundary
  %      % 2. read LEN and NLEN
  %      % 3. copy LEN bytes to output;
  %   huffman_* ->
  %      % 0. if huffman_dyn: read code trees
  %      % loop until end of block:
  %      %   1. decode literal length value from input stream
  %      %   2a. if value < 256: copy literal byte to output
  %      %   2b. elif end of block (256)? break
  %      %   2c. elif 256 > value < 286:
  %      %          decode $distance from input stream
  %      %          move $distance bytes back in output
  %      %          copy LEN bytes from this pos to the output
  %
  % Do this until last block (BFINAL) is found
  %
  {ok, <<>>, finalize(Ctx, size(Enc), 0)}.

deflate(#zlib{de=Dec} = Ctx) ->
  Z = zlib:open(),
  ok = zlib:'deflateInit'(Z, default),
  [Enc] = zlib:deflate(Z, Dec, finish),
  ok = zlib:'deflateEnd'(Z),
  {ok, Enc, finalize(Ctx, size(Dec), size(Enc))}.

int_to_btype(0) -> no_compression.
int_to_btype(1) -> huffman_fixed.
int_to_btype(2) -> huffman_dyn.

finalize(#zlib{op=Op, state=final, read_count=R, write_count=W}, Read, Written) ->
  #zlib{op=Op,
        state=finalized,
        in=undefined,
        de=undefined,
        read_count=R+Read,
        write_count=W+Written}.

stats(#zlib{read_count=R, write_count=W}) -> {ok, [{read, R}, {written, W}]}.

tail(#zlib{op=in, in=Tail}) -> Tail;
tail(#zlib{op=de, de=Tail}) -> Tail.  % This shouldn't currently happen

-ifdef(TEST).

-define(check_inflate(Name, Input, Output, Tail),
Name() ->
  {ok, Result, Ctx} = in(Input),
  Read = size(Input) - case Tail of
    undefined -> 0;
    _ -> size(Tail)
  end,
  Written = size(Output),

  [
    ?_assertEqual(Output, Result),
    ?_assertEqual(Tail, tail(Ctx)),
    ?_assertEqual({ok, [
      {read, Read},
      {written, Written}
    ]}, stats(Ctx))
  ]
).
-define(check_inflate(Name, Input, Output),
       ?check_inflate(Name, Input, Output, undefined)).

?check_inflate(empty_inflation_test_,
               <<120, 94, 3, 0, 0, 0, 0, 1>>, <<>>).
?check_inflate(a_inflation_test_,
               <<120, 94, 115, 4, 0, 0, 66, 0, 66>>, <<"A">>).
?check_inflate(aaa_inflation_test_,
               <<120, 94, 115, 116, 116, 4, 0, 1, 137, 0, 196>>, <<"AAA">>).

-endif.
