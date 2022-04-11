-module(gitd_cmd).
-export([parse/1]).

parse(Cmdline) ->
  case gitd_utils:cmd_split(Cmdline) of
    {ok, [Cmd | Args]} -> {ok, Cmd, parse_cmd_args(Cmd, Args)};
    invalid -> {error, invalid}
  end.

parse_cmd_args("git-receive-pack", [Repo]) -> [{repo, Repo}];
parse_cmd_args("git-upload-pack" = Cmd, Args) -> parse_cmd_args(Cmd, Args, []).
parse_cmd_args("git-upload-pack", [Repo], Res) -> [{repo, Repo} | Res];
parse_cmd_args("git-upload-pack" = Cmd, ["--strict"|Args], Res) ->
  parse_cmd_args(Cmd, Args, [strict | Res]);
parse_cmd_args("git-upload-pack" = Cmd, ["--stateless-rpc"|Args], Res) ->
  parse_cmd_args(Cmd, Args, [stateless_rpc | Res]);
parse_cmd_args("git-upload-pack" = Cmd, ["--advertise-refs"|Args], Res) ->
  parse_cmd_args(Cmd, Args, [advertise_refs | Res]).
