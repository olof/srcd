% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_sqlite).
-behavior(srcd_persistence).

% This is very experimental; esqlite3 does not seem a good match.

-include("srcd_object.hrl").
-include_lib("kernel/include/logger.hrl").

-export([init/2, load/1, dump/3, list/0]).

open(Name) -> esqlite3:open(Name ++ ".db").

init(Db, Name) ->
  {ok, Conn} = open(Db),
  ok = esqlite3:exec(<<"
    BEGIN;
    CREATE TABLE meta (attr TEXT UNIQUE, value TEXT);
    CREATE TABLE objects (id TEXT UNIQUE, type INT, data);
    CREATE TABLE refs (ref TEXT UNIQUE, oid TEXT);
  ">>, Conn),
  '$done' = esqlite3:exec(<<"INSERT INTO meta VALUES (?, ?);">>,
                          ["name", Name], Conn),
  ok = esqlite3:exec(<<"COMMIT;">>, Conn),
  ok = esqlite3:close(Conn).

load(Db) ->
  {ok, Conn} = open(Db),
  Meta = read_meta(Conn),
  Refs = read_refs(Db, Conn),
  Objects = read_objects(Conn),
  ok = esqlite3:close(Conn),
  {ok, Meta, Refs, Objects}.

dump(Db, Refs, Objects) ->
  {ok, Conn} = open(Db),
  ok = esqlite3:exec(<<"BEGIN;">>, Conn),
  ok = write_objects(Conn, Objects),
  ok = write_refs(Conn, Refs),
  ok = esqlite3:exec(<<"COMMIT;">>, Conn),
  ok = esqlite3:close(Conn).

list() ->
  {ok, Dir} =  application:get_env(srcd, data_dir),
  {ok, Dbs} = file:list_dir(Dir),
  {ok, [filename:absname_join(Dir, Db) || Db <- Dbs,
                                          filename:extension(Db) =:= ".db"]}.

read_meta(Conn) ->
  esqlite3:map(
    fun ({Attr, Value}) ->
      {binary_to_existing_atom(Attr), binary_to_list(Value)}
    end,
    <<"SELECT attr, value FROM meta;">>,
    Conn
  ).

read_refs(Db) ->
  {ok, Conn} = open(Db),
  Res = read_refs(Db, Conn),
  {ok, Conn} = esqlite3:close(Conn),
  Res.
read_refs(_Db, Conn) ->
  lists:keysort(1, esqlite3:map(
    fun ({Ref, Oid}) -> {binary_to_list(Ref), binary_to_list(Oid)} end,
    <<"SELECT ref, oid FROM refs;">>,
    Conn
  )).

write_refs(Conn, []) -> ok;
write_refs(Conn, [Cmd | Cmds]) ->
  '$done' = apply_ref_cmd(Conn, Cmd),
  write_refs(Conn, Cmds).

apply_ref_cmd(Conn, {create, Ref, Id}) ->
  sql_exec(Conn, "INSERT INTO refs (ref, oid) VALUES (?, ?);", [Ref, Id]);
apply_ref_cmd(Conn, {update, Ref, {Old, New}}) ->
  sql_exec(Conn, "UPDATE refs SET oid=? WHERE ref=? AND oid=?;",
           [New, Ref, Old]);
apply_ref_cmd(Conn, {delete, Ref, Id}) ->
  sql_exec(Conn, "DELETE FROM refs WHERE ref=? AND oid=?;", [Ref, Id]).

sql_exec(Conn, Query) -> sql_exec(Conn, Query, []).
sql_exec(Conn, Query, Args) ->
  ?LOG_DEBUG("SQL: ~s ~p", [Query, Args]),
  esqlite3:exec(list_to_binary(Query), Args, Conn).

write_objects(Conn, []) -> ok;
write_objects(Conn, [Obj | Objects]) ->
  ok = write_object(Conn, Obj),
  write_objects(Conn, Objects).

write_object(Conn, Obj) ->
  Type = srcd_object:type_id(Obj),
  {_, Encoded} = srcd_object:encode(Obj),
  '$done' = esqlite3:exec(<<"INSERT INTO objects VALUES (?, ?, ?);">>,
                          [Obj#object.id, Type, Encoded], Conn),
  ok.

read_objects(Conn) ->
  esqlite3:map(
    fun ({Oid, Type, Data}) ->
      #object{id=binary_to_list(Oid), data=parse_object(Type, Data)}
    end,
    <<"SELECT id, type, data FROM objects;">>,
    Conn
  ).

read_objects(Conn, Oids) ->
  [load_object(Conn, Oid) || Oid <- Oids].

read_object(Db, Oid) ->
  {ok, Conn} = open(Db),
  Res = load_object(Db, Oid),
  {ok, Conn} = esqlite3:close(Conn),
  {ok, Res}.

load_object(Conn, Oid) ->
  [Object] = esqlite3:map(
    fun ({Oid, Type, Data}) ->
      #object{id=binary_to_list(Oid), data=parse_object(Type, Data)}
    end,
    <<"SELECT id, type, data FROM objects WHERE id=?">>, [Oid],
    Conn),
  Object.

parse_object(Type, Data) ->
  {ok, Res} = srcd_object:parse(srcd_object:type_name(Type),
                                binary_to_list(Data)),
  Res.
