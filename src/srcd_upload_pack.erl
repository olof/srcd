-module(srcd_upload_pack).
-export([init/2]).
-export([caps/0, advertise/1]).

-include_lib("kernel/include/logger.hrl").
-record(?MODULE, {repo, opts=[], version=0}).

caps() -> srcd_ssh:caps() ++ [].

init(Args, #{version := Version}) ->
  ?LOG_NOTICE("init upload-pack"),
  {ok, {Repo, Opts}} = parse_args(Args),
  {ok, advertise, #?MODULE{repo=Repo, version=Version, opts=Opts}}.

advertise(#?MODULE{repo=Repo, version=Version, opts=Opts} = Data) ->
  ?LOG_NOTICE("Advertise upload-pack"),
  case srcd_repo:refs(Repo) of
    {ok, Refs} ->
      ?LOG_NOTICE("ogt some refs"),
      {ok, Adv} = srcd_pack:advertisement(Version, Refs, caps()),
      case proplists:get_value(advertise_refs, Opts) of
        true -> {ok, Adv};
        _ -> {next_state, wait_for_input, Adv, Data}
      end;
    {error, enoent} ->
      {error, "No such repo\n"}
  end.

wait_for_input(Type, Content, Data) -> not_implemented.
process_input(Type, Content, Data) -> not_implemented.

parse_args(Args) -> parse_args(Args, []).
parse_args([Repo], Res) -> {ok, {Repo, Res}};
parse_args(["--strict"|Args], Res) ->
  parse_args(Args, [strict | Res]);
parse_args(["--stateless-rpc"|Args], Res) ->
  parse_args(Args, [stateless_rpc | Res]);
parse_args(["--advertise-refs"|Args], Res) ->
  parse_args(Args, [advertise_refs | Res]).
