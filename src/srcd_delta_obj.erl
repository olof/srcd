% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_delta_obj).
-export([resolve/2, ref_delta/3]).

-include_lib("kernel/include/logger.hrl").
-include("srcd_object.hrl").
-ifdef(TEST).
-include("tests/delta_obj.trl").
-endif.

resolve(Repo, Pack = #pack{objects=Objects}) ->
  {ok, Resolved} = resolve(Repo, Objects),
  {ok, Pack#pack{objects=Resolved}};
resolve(Repo, Objects) when is_list(Objects) ->
  resolve(Repo, Objects, []).
resolve(Repo, [], Result) -> {ok, lists:reverse(Result)};
resolve(Repo, [Object = #object{} | Objects], Result) ->
  resolve(Repo, Objects, [Object | Result]);
resolve(Repo, [Object = #ref_delta{} | Objects], Result) ->
  ?LOG_NOTICE("ref_delta based on ref_delta, recursive resolve"),
  {ok, Resolved} = ref_delta(Repo, Result, Object),
  resolve(Repo, Objects, [Resolved | Result]).

ref_delta(Repo, Objects, #ref_delta{ref=BaseId, instructions=Instructions}) ->
  ?LOG_NOTICE("Processing ref delta with baseid ~p", [BaseId]),
  {ok, Obj} = case srcd_repo:object(Repo, BaseId) of
    {ok, O} -> {ok, O};
    {error, nomatch} -> find_object(Objects, BaseId)
  end,
  process_ref_delta(Repo, Objects, Obj, Instructions).

process_ref_delta(Repo, Objects, BaseObj = #ref_delta{}, Instructions) ->
  {ok, Resolved} =  ref_delta(Repo, Objects, BaseObj),
  process_ref_delta(Repo, Objects, Resolved, Instructions);
process_ref_delta(Repo, _, #object{id=BaseId, data=BaseObj}, Instructions) ->
  {Type, BaseData} = srcd_object:encode(BaseObj),
  % TODO: Check to see if base_size is as expected
  {ok, Data} = execute(BaseData, Instructions),
  {ok, Object} = srcd_object:parse(Type, Data),

  Digest = crypto:hash_init(sha),
  Digest2 = crypto:hash_update(Digest, srcd_object:canon(Object)),
  H = srcd_utils:bin_to_hex(crypto:hash_final(Digest2)),

  ?LOG_NOTICE("Resolved ref_delta to object ~p based on ~p of type ~p",
              [H, BaseId, Type]),

  {ok, #object{id=H, data=Object}}.
  % TODO: Check to see if target_size is as expected

execute(BaseObj, Instructions) ->
  execute("", BaseObj, Instructions).
execute(Obj, BaseObj, []) -> {ok, Obj};
execute(Obj, BaseObj, [Instruction | Instructions]) ->
  {ok, NewObj} = ref_delta_apply(Obj, BaseObj, Instruction),
  execute(NewObj, BaseObj, Instructions).

ref_delta_apply(Obj, BaseData, {copy, {Offset, Size}}) ->
  {ok, lists:concat([Obj, lists:sublist(BaseData, Offset + 1, Size)])};
ref_delta_apply(Obj, _, {add, Data}) ->
  {ok, lists:concat([Obj, Data])}.

find_object([], Id) -> {error, nomatch};
find_object([Object = #object{id=Id} | Objects], Id) -> {ok, Object};
find_object([_ | Objects], Id) -> find_object(Objects, Id).
