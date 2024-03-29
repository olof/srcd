% ex:ts=2:sw=2:sts=2:et
% -*- tab-width: 2; c-basic-offset: 2; indent-tabs-mode: nil -*-
-module(srcd_ssh_ch).
-behavior(ssh_server_channel). % replaces ssh_daemon_channel
-record(state, {
          n,
          id,
          cm
         }).
-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

init([N]) ->
    {ok, #state{n = N}}.

handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    {ok, State#state{id = ChannelId,
                     cm = ConnectionManager}}.

handle_ssh_msg({ssh_cm, CM, {data, ChannelId, 0, Data}},
               #state{n = N} = State) ->
    M = N - size(Data),
    case M > 0 of
        true ->
           ssh_connection:send(CM, ChannelId, Data),
           {ok, State#state{n = M}};
        false ->
           <<SendData:N/binary, _/binary>> = Data,
           ssh_connection:send(CM, ChannelId, SendData),
           ssh_connection:send_eof(CM, ChannelId),
           {stop, ChannelId, State}
    end;
handle_ssh_msg({ssh_cm, CM, {env, ChannelId, WantReply, Var, Val}},
               #state{n = N} = State) ->
    ?LOG_NOTICE("Got env ~s=~p", [Var, Val]),
    ssh_connection:reply_request(CM, WantReply, failure, ChannelId),
    {ok, State};
handle_ssh_msg({ssh_cm, _ConnectionManager,
                {data, _ChannelId, 1, Data}}, State) ->
    error_logger:format(standard_error, " ~p~n", [binary_to_list(Data)]),
    {ok, State};

handle_ssh_msg({ssh_cm, _ConnectionManager, {eof, _ChannelId}}, State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, _Error, _}},
               State) ->
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, _Status}}, State) ->
    {stop, ChannelId, State}.

terminate(_Reason, _State) ->
    ok.
