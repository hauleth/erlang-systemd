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

-export([send/1]).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

% # Internal interface

send({Field, Value}) ->
    Msg = systemd_protocol:encode_field(Field, Value),
    gen_server:call(?NAME, {send, Msg}).

% # Behaviour implementation

start_link(Address) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, Address, []).

init([]) ->
    {ok, []};
init(Address) ->
    % We never receive on this socket, so we set is as {active, false}
    {ok, Socket} = gen_udp:open(0, [binary, local, {active, false}]),
    {ok, {Socket, Address}}.

handle_call({send, Message}, _Ref, {Socket, Address}=State) ->
    ok = gen_udp:send(Socket, Address, 0, Message),
    {reply, ok, State};
handle_call(_Msg, _Ref, []) ->
    {reply, ok, []}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.
