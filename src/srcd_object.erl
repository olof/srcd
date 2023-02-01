% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_object).
-export([read/1, read/2, deps/1, pack/1, encode/1, canon/1,
         parse/1, parse/2, type/1, type_id/1, type_name/1]).

-include_lib("kernel/include/logger.hrl").
-include("srcd_object.hrl").

-ifdef(TEST).
-include("tests/object.trl").
-endif.

type(#object{data=Data}) -> type(Data);
type(#blob{}) -> blob;
type(#tree{}) -> tree;
type(#commit{}) -> commit.

type_id(#object{data=Data}) -> type_id(element(1, Data));
type_id(Atom) ->
  case Atom of
    commit -> 1;
    tree -> 2;
    blob -> 3;
    tag -> 4;
    ofs_delta -> 6;
    ref_delta -> 7
  end.
type_name(#object{data=Data}) -> element(1, Data);
type_name(Id) ->
  case Id of
    1 -> commit;
    2 -> tree;
    3 -> blob;
    4 -> tag;
    6 -> ofs_delta;
    7 -> ref_delta
  end.

read(Digest) -> read(standard_io, Digest).
read(Fh, Digest1) ->
  {Type, Length, Digest0} = read_object_header(Fh, Digest1),
  ObjDigest1 = crypto:hash_update(crypto:hash_init(sha), lists:concat([
    atom_to_list(Type), " ", integer_to_list(Length), "\0"
  ])),
  % So, we can't do read(Length) because Length refers to uncompressed
  % data size and we are reading compressed data. We really have no good
  % way of knowing how much to read.
  %   We could read a byte at a time until zlib is happy, then stop, and
  % read another object.
  %   We could try to make zlib let us know how long it has read and glue
  % the remaining bytes to the head of stdin somehow.
  %   We could a lot and see how many objects we get. (But when do we
  %   stop reading?)
  % What does git do? It relies on memory pointers to buffers where both
  % it and zlib can do its work.
  case Type of
    ref_delta ->
      % * base object name
      % * compressed delta data
      {ok, BaseObjId, D} = read_object_id(Fh, Digest0),
      ?LOG_NOTICE("ref_delta base obj: ~p", [
        srcd_utils:bytes_to_hex(BaseObjId)
      ]),
      {ok, {SizeBase, SizeTarget, Instructions}, D2} = read_delta_data(Fh, D),
      {ok, #ref_delta{
        ref=srcd_utils:bytes_to_hex(BaseObjId),
        size_base=SizeBase,
        size_target=SizeTarget,
        instructions=Instructions
      }, D2};
    ofs_delta ->
      % * a negative relative offset from the delta object's position in the
      %   pack
      % * compressed delta data
      {error, ofs_delta_not_implemented};
    _ ->
      {ok, _, Object, Compressed} = srcd_zlib:inflate(Fh),
      Length = length(Object),
      Digest = crypto:hash_update(Digest0, Compressed),
      ObjDigest = crypto:hash_update(ObjDigest1, Object),
      {ok, Parsed} = parse(Type, Object),
      H = crypto:hash_final(ObjDigest),
      ?LOG_NOTICE("object parsed: hash: ~p", [srcd_utils:bin_to_hex(H)]),
      {ok, #object{data=Parsed, id=srcd_utils:bin_to_hex(H)}, Digest}
  end.

read_delta_data(Fh, Digest) ->
  {ok, _, Object, Compressed} = srcd_zlib:inflate(Fh),
  Digest0 = crypto:hash_update(Digest, Compressed),

  ?LOG_NOTICE("REF DELTA BYTES: ~p", [Object]),

  {SizeBase, Tail1} = parse_delta_size(Object),
  {SizeTarget, Tail} = parse_delta_size(Tail1),

  ?LOG_NOTICE("Base size: ~p; Target size: ~p", [SizeBase, SizeTarget]),
  {ok, Instructions} = parse_delta_data(Tail, SizeTarget, []),
  {ok, {SizeBase, SizeTarget, Instructions}, Digest0}.

parse_delta_data(_Tail, 0, Instructions) ->
  % TODO: _Tail should be empty - what if it isn't?
  {ok, lists:reverse(Instructions)};
parse_delta_data(Data, BytesLeft, Instructions) when BytesLeft > 0 ->
  {Instruction, Size, Tail} = parse_delta_instruction(Data),
  ?LOG_NOTICE("delta copy remaining bytes: ~p", [BytesLeft - Size]),
  parse_delta_data(Tail, BytesLeft - Size, [Instruction | Instructions]).

parse_delta_instruction([Byte | Object]) ->
  case Byte band 128 of
    128 ->  % copy from base object
            % (Byte band 127) contains a seven bit mask;
            % offset1 offset2 offset3 offset4 size1 size2 size3
            {{Offset, Size}, Tail} = parse_base_copy_instr(Object,
                                                           Byte band 127),
            ?LOG_NOTICE("delta copy command, ~p", [{Offset, Size}]),
            {{copy, {Offset, Size}}, Size, Tail};

    0 ->    % add verbatim data
            % (Byte contains size in bytes)
            % TODO: Byte == 0 is reserved, and should be rejected
            {Data, Tail} = lists:split(Byte, Object),
            ?LOG_NOTICE("delta add command (size ~p), ~p", [Byte, Data]),
            {{add, Data}, Byte, Tail}
  end.

combine_intlist(Ints) ->
  Multipliers = lists:reverse(lists:seq(0, length(Ints) - 1)),
  Sum = lists:sum([X bsl N * 8 || {X, N} <- lists:zip(Ints, Multipliers)]),
  ?LOG_NOTICE("INTS = ~p * ~p -> ~p", [Ints, Multipliers, Sum]),
  Sum.

parse_base_copy_instr(Object, 0) ->
  {{0, 0}, Object};
parse_base_copy_instr(Object, Mask) ->
  read_base_copy_instr(Object, Mask, 1, {[], []}).

read_base_copy_instr(Object, Mask, 128, {Offsets, Sizes}) ->
  ?LOG_NOTICE("OFS/SIZ LISTS: ~p    ~p", [Offsets, Sizes]),
  {{combine_intlist(Offsets), combine_intlist(Sizes)}, Object};
read_base_copy_instr(Object, Mask, N, {Offsets, Sizes}) ->
  {Byte, Tail} = case Mask band N of
    0 -> {0, Object};   % Field omitted - defaults to 0
    _ -> [B | T] = Object, {B, T}
  end,
  read_base_copy_instr(Tail, Mask, N * 2, case N > 8 of
    false -> {[Byte | Offsets], Sizes};
    true -> {Offsets, [Byte | Sizes]}
  end).

parse_delta_size(Object) -> parse_delta_size(Object, 0, 0).
parse_delta_size([Byte | Tail], Sum, Exp) ->
  ?LOG_NOTICE("PARSE_DELTA_SIZE: ~p", [Byte]),
  case Byte band 128 of
    128 -> parse_delta_size(Tail, (Byte band 127 bsl Exp) + Sum, Exp + 7);
    _ -> {(Byte bsl Exp) + Sum, Tail}
  end.

read_object_id(Fh, Digest) ->
  % FIXME: assumes sha1
  % FIXME: handle unexpected eof
  {ObjId, D} = srcd_utils:read(Fh, 20, Digest),
  {ok, ObjId, D}.

read_object_header(Fh, Digest) ->
  {[Byte], D} = srcd_utils:read(Fh, 1, Digest),
  ?LOG_NOTICE("initial object header byte ~p", [Byte]),
  Type = srcd_object:type_name(Byte band 112 bsr 4),
  ?LOG_NOTICE("initial object header type ~p", [Type]),
  N = Byte band 15,
  case Byte band 128 of
    0 -> {Type, N, D};
    128 -> read_object_header(Fh, D, N, 4, Type)
  end.
read_object_header(Fh, Digest, N0, Bits, Type) ->
  {[Byte], D} = srcd_utils:read(Fh, 1, Digest),
  N = N0 + (Byte band 127 bsl Bits),
  case Byte band 128 of
    0 -> {Type, N, D};
    128 -> read_object_header(Fh, D, N, Bits + 7, Type)
  end.

read_packfile_signature(#pack{hash=D} = State) ->
  Hash = crypto:hash_final(D),
  case list_to_binary(srcd_utils:read(20)) of
    Hash -> {ok, State#pack{hash=srcd_utils:bin_to_hex(Hash)}};
    ShouldB -> ?LOG_NOTICE("Hash mismatch: got vs should be ~p",
                           [{Hash, ShouldB}]),
               {error, packfile_hash_fail}
  end.

encode(#object{id=Id, data=Data}) -> encode(Data);
encode(#blob{data=Data}) -> {blob, Data};
encode(#commit{msg=Msg} = Commit) ->
  CommitHeader = string:join(lists:concat([
    commit_tree_line(Commit),
    commit_parent_lines(Commit),
    commit_author_line(Commit),
    commit_committer_line(Commit)
  ]), "\n"),
  {commit, string:join([CommitHeader, Msg], "\n\n")};
encode(#tree{items=Items}) ->
  {tree, lists:concat([encode(Node) || Node = #tree_node{} <- Items])};
encode(#tree_node{mode=Mode, name=Name, object=Oid}) ->
  lists:concat([Mode, " ", Name, "\0", srcd_utils:hex_to_bin_sha1(Oid)]).

pack(Object) ->
  {Type, Payload} = encode(Object),
  EncHeader = header(Type, length(Payload)),
  Compressed = srcd_zlib:deflate(Payload),
  lists:concat([EncHeader, Compressed]).

header(Type, Len) when is_atom(Type) ->
  header(type_id(Type), Len);
header(Type, Len) when Len < 16 -> [Type bsl 4 + Len];
header(Type, Len) ->
  header_tail([128 + (Type bsl 4) + Len band 15], Len bsr 4).
header_tail(Prefix, Rem) when Rem < 128 -> Prefix ++ [Rem];
header_tail(Prefix, Rem) ->
  header_tail(Prefix ++ [1 bsl 7 + Rem band 127], Rem bsr 7).

canon(#object{data=D}) -> canon(D);
canon(#blob{} = D) -> canon(blob, D);
canon(#tree{} = D) -> canon(tree, D);
canon(#commit{} = D) -> canon(commit, D).
canon(Type, D) ->
  {Type, Payload} = encode(D),
  Len = length(Payload),
  lists:flatten(io_lib:format("~s ~b\0~s", [Type, Len, Payload])).

parse_tree_nodes(Object) ->
  parse_tree_nodes(Object, []).
parse_tree_nodes("", Res) -> lists:reverse(Res);
parse_tree_nodes(Object, Res) ->
  [Mode, Tail1] = string:split(Object, " "),
  [Name, Tail0] = string:split(Tail1, "\0"),
  {Hash, Tail} = lists:split(20, Tail0),
  parse_tree_nodes(Tail, [#tree_node{
    mode=Mode,
    name=Name,
    object=srcd_utils:bytes_to_hex(Hash)
  } | Res]).

parse(Object) ->
  [Head, Payload] = string:split(Object, "\0"),
  [Type, Len] = string:split(Head, " "),
  parse(Type, Object).
parse(blob, Object) ->
  {ok, #blob{data=Object}};
parse(tree, Object) ->
  {ok, #tree{items=parse_tree_nodes(Object)}};
parse(commit, Object) ->
  {Head, Msg} = parse_commit(Object),

  ?LOG_NOTICE("Commit header: ~p", [Head]),

  Tree = proplists:get_value(tree, Head),
  Author = proplists:get_value(author, Head),
  Committer  = proplists:get_value(committer, Head),
  Parents = proplists:get_all_values(parent, Head),

  {ok, #commit{
    tree=Tree,
    author=Author,
    committer=Committer,
    parents=Parents,
    msg=Msg
  }};
parse(TypeId, Object) ->
  parse(type_name(TypeId), Object).

parse_commit(Object) ->
  [Head0, Msg] = string:split(Object, "\n\n"),
  Head = parse_commit_head(Head0),
  {Head, Msg}.

deps(#object{data=D}) -> deps(D);
deps(#blob{}) -> [];
deps(#commit{tree=Tree, parents=Parents}) -> [Tree | Parents];
deps(#tree{items=Items}) -> [Oid || #tree_node{object=Oid} <- Items].

parse_commit_head(Head) ->
  Lines = string:split(Head, "\n", all),
  [parse_commit_head_line(Line) || Line <- Lines].

parse_commit_head_line(Line) ->
  [Key, Val] = string:split(Line, " "),
  {parse_commit_head_key(Key), parse_commit_head_val(Key, Val)}.

parse_commit_head_key("tree") -> tree;
parse_commit_head_key("parent") -> parent;
parse_commit_head_key("author") -> author;
parse_commit_head_key("committer") -> committer.

parse_commit_head_val("tree", Val) -> Val;
parse_commit_head_val("parent", Val) -> Val;
parse_commit_head_val("author", Val) -> parse_author_stamp(Val);
parse_commit_head_val("committer", Val) -> parse_author_stamp(Val).

parse_author_stamp(Val) ->
  [Head, Tz] = string:split(Val, " ", trailing),
  [User, Time] = string:split(Head, " ", trailing),

  % TODO: parse User into Name and Email.
  %       Currently, User usually looks like "Olof J <test@example.com>"
  %       We want: Name = "Olof J" and Email = "test@example.com"
  %       We can't assume User to always look like that.
  %       Using the whole User as name works because we don't change
  %       it at all, so the sha1 still matches.
  #stamp{
    name=User,
    %email="olof@example.com",
    time=list_to_integer(Time),
    tz=Tz
  }.

commit_tree_line(#commit{tree=Tree}) ->
  ["tree " ++ Tree].

commit_parent_lines(#commit{parents=Parents}) ->
  ["parent " ++ P || P <- Parents].

commit_author_line(#commit{author=Author}) ->
  ["author " ++ format_author(Author)].

commit_committer_line(#commit{committer=Committer}) ->
  ["committer " ++ format_author(Committer)].

format_author(Stamp = #stamp{time=T}) when is_list(T) ->
  format_author(Stamp#stamp{time=list_to_integer(T)});
format_author(Stamp = #stamp{name=Name, email=undefined, time=T, tz=Tz}) ->
  lists:flatten(io_lib:format("~s ~b ~s", [Name, T, Tz]));
format_author(Stamp = #stamp{name=Name, email=Email, time=T, tz=Tz}) ->
  lists:flatten(io_lib:format("~s <~s> ~b ~s", [Name, Email, T, Tz])).
