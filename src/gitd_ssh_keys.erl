-module(gitd_ssh_keys).
-behaviour(ssh_server_key_api).
-export([host_key/2, is_auth_key/3]).

host_key(Algo, Opts) -> ssh_file:host_key(Algo, Opts).
is_auth_key(Pubkey, "git", Opts) -> true; % :D
is_auth_key(_, _, _) -> false.
