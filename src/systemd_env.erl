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
-module(systemd_env).

-include("systemd_internal.hrl").

-export([get/0, reset/0, unset/0, clear/0]).

get() ->
    case persistent_term:get(?MODULE, undefined) of
        undefined ->
            Env = #{
                    watchdog_pid => os:getenv(?WATCHDOG_PID),
                    watchdog_timeout => os:getenv(?WATCHDOG_TIMEOUT),
                    notify_socket => os:getenv(?NOTIFY_SOCKET),
                    listen_pid => os:getenv(?LISTEN_PID),
                    listen_fds => os:getenv(?LISTEN_FDS),
                    listen_fdnames => os:getenv(?LISTEN_FDNAMES)
                   },
            persistent_term:put(?MODULE, Env),
            unset(),
            Env;
        Map when is_map(Map) -> Map
    end.

clear() ->
    persistent_term:erase(?MODULE).

reset() ->
    Env = persistent_term:get(?MODULE, #{}),
    _ = maps:map(fun do_set/2, Env),

    ok.

do_set(_Name, false) -> ok;
do_set(Name0, Value) ->
    Name1 = atom_to_list(Name0),
    Name = string:uppercase(Name1),
    os:putenv(Name, Value).

unset() ->
    os:unsetenv(?NOTIFY_SOCKET),
    os:unsetenv(?WATCHDOG_PID),
    os:unsetenv(?WATCHDOG_TIMEOUT),
    os:unsetenv(?LISTEN_PID),
    os:unsetenv(?LISTEN_FDS),
    os:unsetenv(?LISTEN_FDNAMES),
    ok.
