% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-

-module(flate).
% This module tries to implement RFC 1951, to be able to support
% inflating compressed objects.

-export([in/1, in/2, de/1, tail/1, stats/1]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-export([fixed/0]).
-endif.

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

-record(zlib, {op, input, state=data, output=[], read_count=0, write_count=0}).

-include("check.hrl").

-define(op(Name),
Name(#zlib{op=Name} = State) -> route(Name, State);
Name(#zlib{op=Op}) -> {badarg, op, Op};
Name(Data) -> Name(#zlib{op=Name, input=Data})).

?op(de).
in(State = #zlib{input=De}, Data) when is_list(De) ->
  deflate(State#zlib{input=lists:reverse([Data | De])});
in(State = #zlib{input=De}, Data) ->
  deflate(State#zlib{input=[De, Data]}).
?op(in).

% route() is a hack, to workaround a limitation with erlang preprocessor; i
% couldn't call a function called Name?MODULE when Name was a macro parameter.
% This saddened me.
route(in, State) -> inflate(State);
route(de, State) -> deflate(State).

inflate(#zlib{input= <<>>, state=data} = Ctx) -> {more, Ctx};
inflate(#zlib{input=Enc, output=Dec, state=data, read_count=Rc, write_count=Wc} = Ctx) ->
  % parse code tree, parse compressed bytes
  <<Head:8, _/binary>> = Enc,
  <<ByteTail:5/bits, Btype:2, Bfinal:1, Tail/binary>> = Enc,
  {ok, DecBlock, NewTail, ReadLen} = inflate_block(int_to_btype(Btype), ByteTail, Tail),

  NewCtx = Ctx#zlib{
    input=NewTail,
    output=case Dec of
      undefined -> DecBlock;
      _ -> [DecBlock | Dec]
    end,
    write_count=(Wc + size(DecBlock)),
    read_count=(Rc + ReadLen + 1)
  },

  case Bfinal of
    0 -> inflate(NewCtx);
    1 -> finalize(NewCtx)
  end.

deflate(#zlib{input=Input} = Ctx) ->
  Z = zlib:open(),
  ok = zlib:'deflateInit'(Z, default),
  [Output] = zlib:deflate(Z, Input, finish),
  ok = zlib:'deflateEnd'(Z),
  {ok, Output,
       finalize(Ctx#zlib{read_count=size(Input), write_count=size(Output)})}.

stats(#zlib{read_count=R, write_count=W}) -> {ok, [{read, R}, {written, W}]}.

tail(#zlib{op=in, input=Tail}) -> Tail;
tail(#zlib{op=de, output=Tail}) -> Tail.  % This shouldn't currently happen

finalize(#zlib{input={_, Data}} = Ctx) ->
  % TODO If we have an incomplete byte, we just throw it away now. That
  %      may, or may not, be an ok thing to do.
  finalize(Ctx#zlib{input=Data});
finalize(#zlib{output=Out} = Ctx) ->
  {ok, iolist_to_binary(lists:reverse(Out)),
   Ctx#zlib{state=finalized, output=undefined}}.

inflate_block(no_compression, _, Data) ->
  <<Len:16, Nlen:16, Payload/binary>> = Data,
  Nlen = 16#FFFF - Len,
  <<Decoded:Len/bytes, Tail/binary>> = Payload,
  {ok, Decoded, Tail, Len + 4};
inflate_block(huffman_fixed, InitialBits, Data) ->
  flate_huffman:decode(fixed(), {InitialBits, Data});
inflate_block(huffman_dyn, InitialBits, Data) ->
  % TODO: maybe i forgot to do byte accounting on this?
  % TODO: codetree doesn't exist. So there's that.
  {ok, Codes, D} = flate_huffman:codetree(dynamic, {InitialBits, Data}),
  flate_huffman:decode(Codes, D).

fixed() ->
  % Literal value    Bits                 Codes
  % -------------------------------------------------------
  %       0 - 143     8         00110000 through  10111111
  %     144 - 255     9        110010000 through 111111111
  %     256 - 279     7          0000000 through   0010111
  %     280 - 287     8         11000000 through  11000111
  lists:concat([
    [{X, 8} || X <- lists:seq(0, 143)],
    [{X, 9} || X <- lists:seq(144, 255)],
    [{X, 7} || X <- lists:seq(256, 279)],
    [{X, 8} || X <- lists:seq(280, 287)]
  ]).

int_to_btype(0) -> no_compression;
int_to_btype(1) -> huffman_fixed;
int_to_btype(2) -> huffman_dyn;
int_to_btype(N) -> {invalid_zlib_btype, N}.

-ifdef(TEST).
?check_full_inflate(inflate_empty_uncompressed_test_,
                    <<1, 0, 0, 255, 255>>, <<>>).
?check_full_inflate(inflate_a_uncompressed_test_,
                    <<1, 0, 1, 255, 254, "a">>, <<"a">>).
?check_full_inflate(inflate_aaa_uncompressed_test_,
                    <<1, 0, 3, 255, 252, "aaa">>, <<"aaa">>).
?check_full_inflate(inflate_aaa_uncompressed_tail_test_,
                    <<1, 0, 3, 255, 252, "aaaAAA">>, <<"aaa">>, <<"AAA">>).

?check_full_inflate(inflate_empty_huffman_fixed_test_,
                    <<3, 0>>, <<>>).

%% perl -e 'print map { chr } 75, 4, 0' | ~/src/github/madler/infgen/infgen -bb
%% ! infgen 3.0 output
%% !
%% last			! 1
%% fixed		! 01
%% literal 'a		! 10001001
%% end			! 0000000
%% 			! 000000
?check_full_inflate(inflate_a_huffman_fixed_test_,
                    <<75, 4, 0>>, <<"a">>).

-endif.
