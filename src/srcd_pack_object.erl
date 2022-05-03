-module(srcd_pack_object).
-export([deps/1, type_id/1, build/1, header/2]).

-include_lib("kernel/include/logger.hrl").
-include("srcd_object.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

compress(Data) ->
  Z = zlib:open(),
  ok = zlib:deflateInit(Z, default),
  [X] = zlib:deflate(Z, Data, finish),
  ok = zlib:deflateEnd(Z),
  binary_to_list(X).

build(Obj) ->
  ?LOG_NOTICE("Building obj from ~p", [Obj]),
  {Type, Payload} = format(Obj),
  Header = header(Type, length(Payload)),
  Compressed = compress(Payload),
  Res = Header ++ Compressed,
  Res.

% TODO: load(Data) -> {Type, Obj}

route_to_type(Obj, Func, Args) ->
  Mod = type_module(Obj),
  apply(Mod, Func, Args).

type_module(#commit{}) -> srcd_pack_object_commit;
type_module(#tree{}) -> srcd_pack_object_tree.

type_id(commit) -> 1;
type_id(tree) -> 2;
type_id(blob) -> 3;
type_id(tag) -> 4;
type_id(ofs_delta) -> 6;
type_id(ref_delta) -> 7.

deps(#blob{}) -> [];
deps(Obj) -> route_to_type(Obj, deps, [Obj]).

header(Type, Len) when is_atom(Type) ->
  header(type_id(Type), Len);
header(Type, Len) when Len < 16 -> [Type bsl 4 + Len];
header(Type, Len) ->
  header_tail([128 + (Type bsl 4) + Len band 15], Len bsr 4).
header_tail(Prefix, Rem) when Rem < 128 -> Prefix ++ [Rem];
header_tail(Prefix, Rem) ->
  header_tail(Prefix ++ [1 bsl 7 + Rem band 127], Rem bsr 7).

%payload(Type, Data) ->
%  lists:flatten(io_lib:format("~s ~b\0~s",
%                              [Type, length(Data), format(Type, Data)])).

format(#blob{data=Data}) -> {blob, Data};
format(#commit{} = Data) -> {commit, srcd_pack_object_commit:build(Data)};
format(#tree{} = Data)   -> {tree, srcd_pack_object_tree:build(Data)}.

-ifdef(TEST).
-define(_test_header(Expect, Type, Len),
        ?_assertEqual(Expect, header(Type, Len))).
-define(_test_header_commit(Expect, Len),
        ?_test_header(Expect, commit, Len)).
-define(_test_header_ref_delta(Expect, Len),
        ?_test_header(Expect, ref_delta, Len)).

header_test_() ->
  [
    ?_test_header_commit([16], 0),
    ?_test_header_commit([17], 1),
    ?_test_header_commit([31], 15),
    ?_test_header_commit([144, 1], 16),
    ?_test_header_commit([145, 1], 17),
    ?_test_header_commit([159, 1], 31),
    ?_test_header_commit([144, 2], 32),
    ?_test_header_commit([159, 2], 47),
    ?_test_header_commit([144, 3], 48),
    ?_test_header_commit([159, 3], 63),
    ?_test_header_commit([144, 4], 64),
    ?_test_header_commit([159, 7], 127),
    ?_test_header_commit([144, 8], 128),
    ?_test_header_commit([159, 15], 255),
    ?_test_header_commit([144, 16], 256),
    ?_test_header_commit([159, 127], 2047),
    ?_test_header_commit([144, 128, 1], 2048),

    ?_test_header_ref_delta([112], 0),
    ?_test_header_ref_delta([113], 1),
    ?_test_header_ref_delta([127], 15),
    ?_test_header_ref_delta([240, 1], 16),
    ?_test_header_ref_delta([255, 127], 2047),
    ?_test_header_ref_delta([240, 128, 1], 2048),

    ?_test_header([160, 8], tree, 128),
    ?_test_header([176, 8], blob, 128),
    ?_test_header("3", blob, 3),
    ?_test_header([192, 8], tag, 128),
    ?_test_header([224, 8], ofs_delta, 128)
  ].
-endif.
