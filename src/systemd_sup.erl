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

-module(systemd_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-include("systemd_internal.hrl").

-export([start_link/1, init/1]).

start_link(Opts) ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, Opts).

init(_Opts) ->
    case application:get_env(systemd, handle_event) of
        {ok, false} -> ok;
        {ok, Event} ->
            os:set_signal(Event, handle),
            gen_event:add_sup_handler(
              erl_signal_server,
              {systemd_signal_handler, systemd},
              Event
             )
    end,
    Env = systemd_env:get(),
    SupFlags = #{
        strategy => one_for_one
    },
    SocketServer = #{
        id => socket,
        start => {systemd_socket, start_link, [Env]}
    },

    Watchdog = #{
        id => watchdog,
        start => {systemd_watchdog, start_link, [Env]}
    },

    {ok, {SupFlags, [SocketServer, Watchdog]}}.
