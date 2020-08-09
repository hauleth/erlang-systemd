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
%% @hidden

-module(systemd_socket).

-behaviour(gen_server).

-define(NAME, ?MODULE).

-include_lib("kernel/include/logger.hrl").

-export([send/3]).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

% # Internal interface

send(Data, Pid, Fds) when is_integer(Pid), is_list(Fds) ->
    Msg = systemd_protocol:encode(Data),
    gen_server:call(?NAME, {send, Msg, Pid, Fds}).

% # Behaviour implementation

start_link(Address) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, Address, []).

init([]) ->
    {ok, []};
init(Address) ->
    % We never receive on this socket, so we set is as {active, false}
    {ok, Socket} = socket:open(local, dgram),
    {ok, {Socket, Address}}.

handle_call({send, Message, Pid, Fds}, _Ref, {Socket, Address}=State) ->
    Addr = #{family => local, path => Address},
    MsgHdr = #{
      addr => Addr,
      iov => Message,
      ctrl => encode_fds(Fds) ++ encode_auth(Pid)
     },
    Resp = case socket:sendmsg(Socket, MsgHdr) of
               ok -> ok;
               {error, ebadf} -> {error, bad_descriptor};
               {error, _} = Error -> Error
           end,
    {reply, Resp, State};
handle_call(_Msg, _Ref, []) ->
    {reply, ok, []}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, {Socket, _Address}) ->
    ok = socket:close(Socket),
    ok.

encode_auth(0) -> [];
encode_auth(_) ->
    erlang:error(unimplemented).

encode_fds([]) -> [];
encode_fds(Fds) when is_list(Fds) ->
    Binary = << <<Fd:32/native-integer>> || Fd <- Fds>>,
    [#{level => socket,
       type => rights,
       data => Binary}].
