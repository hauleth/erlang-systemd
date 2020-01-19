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

-module(systemd_sup).

-behaviour(supervisor).

-include("systemd_internal.hrl").

-export([start_link/1, init/1]).

-include_lib("kernel/include/file.hrl").

start_link(Opts) ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, Opts).

init(_Opts) ->
    SupFlags = #{
      strategy => one_for_one
     },
    Pid = os:getpid(),

    SocketServer = #{id => socket,
                     start => {systemd_socket, start_link, [notify_socket()]}},
    Timeout = case watchdog() of
                   {Pid, TimeoutUS} when TimeoutUS > 0 ->
                       erlang:convert_time_unit(TimeoutUS, microsecond, millisecond);
                   _ -> infinity
               end,
    Watchdog = #{id => watchdog,
                 start => {systemd_watchdog, start_link, [Timeout]}},

    {ok, {SupFlags, [SocketServer, Watchdog]}}.

watchdog() ->
    Pid = case os:getenv(?WATCHDOG_PID) of
                 false -> os:getpid();
                 EnvPid -> EnvPid
             end,
    Time = case os:getenv(?WATCHDOG_TIMEOUT) of
               false -> -1;
               EnvTime ->
                   case string:to_integer(EnvTime) of
                       {Timeout, ""} -> Timeout;
                       _ -> -1
                   end
           end,
    unset(watchdog),
    {Pid, Time}.

notify_socket() ->
    State = case os:getenv(?NOTIFY_SOCKET) of
                false ->
                    [];
                [$@ | AbstractPath] ->
                    {local, [0 | AbstractPath]};
                Path ->
                    case file:read_file_info(Path) of
                        {error, _Error} ->
                            [];
                        {ok, #file_info{access=Access}}
                          when Access =:= write; Access =:= read_write ->
                            {local, Path}
                    end
            end,
    unset(notify),
    State.

unset(System) ->
    {ok, Unset} = application:get_env(systemd, unset_env),
    unset(System, Unset).

unset(System, true) -> systemd:unset_env(System);
unset(_System, _) -> ok.
