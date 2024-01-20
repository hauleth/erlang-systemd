%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements.  See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership.  The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License.  You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @private

-module(systemd_socket).

-behaviour(gen_server).

-define(NAME, ?MODULE).

-include_lib("kernel/include/logger.hrl").

-export([send/3]).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

% # Internal interface

send(Data, Pid, Fds) when is_integer(Pid), is_list(Fds) ->
    Msg = systemd_protocol:encode(Data),
    gen_server:call(?NAME, {send, Msg, Pid, Fds}).

% # Behaviour implementation

-record(state, {socket, address, timer_ref}).

start_link(Address) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, Address, []).

init([]) ->
    {ok, []};
init(Address) ->
    {ok, Socket} = socket:open(local, dgram),
    TimerRef = set_timer(),
    State = #state{socket = Socket, address = Address, timer_ref = TimerRef},
    _ = send_msg(State, systemd_protocol:encode([{"MAINPID", os:getpid()}]), 0, []),
    {ok, State}.

handle_call({send, Message, Pid, Fds}, _Ref, State0) ->
    State = clean_timer(Message, State0),
    {reply, send_msg(State, iolist_to_binary(Message), Pid, Fds), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ready_info, _Time}, State) ->
    ?LOG_WARNING("systemd haven't received READY=1 message. "
                 "You may forget to add systemd:ready() to your "
                 "supervisor children list."),
    {noreply, State#state{timer_ref = undefined}}.

send_msg([], _Message, _Pid, _Fds) ->
    ok;
send_msg(#state{socket = Socket, address = Address}, Message, Pid, Fds) ->
    Addr = #{family => local, path => Address},
    MsgHdr = #{
        addr => Addr,
        iov => [Message],
        ctrl => encode_fds(Fds) ++ encode_auth(Pid)
    },
    case socket:sendmsg(Socket, MsgHdr) of
        ok -> ok;
        {error, ebadf} -> {error, bad_descriptor};
        {error, _} = Error -> Error
    end.

set_timer() ->
    case application:get_env(systemd, warn_about_readiness_message) of
        {ok, Time} when is_integer(Time), Time > 0 ->
            erlang:send_after(Time, self(), {ready_info, Time});
        {ok, false} ->
            undefined
    end.

clean_timer(Message, #state{timer_ref = Ref} = State) when is_reference(Ref) ->
    case string:find(Message, "READY=1\n") of
        nomatch ->
            State;

        _ ->
            _ = erlang:cancel_timer(Ref),
            State#state{timer_ref = undefined}
    end;
clean_timer(_, State) -> State.

%% @TODO Implement this when there will be a reliable way to encode
%% `cmsg_send()' with `credentials' type and there will be a way to get other
%% process information from the OS (`GID', `EGID', `UID', and `EUID').
encode_auth(0) -> [];
encode_auth(_) -> erlang:error(unimplemented).

encode_fds([]) ->
    [];
encode_fds(Fds) when is_list(Fds) ->
    Binary = <<<<Fd:32/native-integer>> || Fd <- Fds>>,
    [
     #{
       level => socket,
       type => rights,
       data => Binary
      }
    ].
