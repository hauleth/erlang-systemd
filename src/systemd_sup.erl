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
    SocketServer = #{id => socket,
                     start => {systemd_socket, start_link, [notify_socket()]}},

    WatchdogConfig = watchdog(),
    Watchdog = #{id => watchdog,
                 start => {systemd_watchdog, start_link, [WatchdogConfig]}},

    {ok, {SupFlags, [SocketServer, Watchdog]}}.

watchdog() ->
    Pid = os:getpid(),
    Enabled = case os:getenv(?WATCHDOG_PID) of
                 false -> true;
                 Pid -> true;
                 _ -> false
             end,
    Time = case os:getenv(?WATCHDOG_TIMEOUT) of
               false -> infinity;
               EnvTime ->
                   case string:to_integer(EnvTime) of
                       {Timeout, ""} when Timeout > 0 ->
                           erlang:convert_time_unit(Timeout,
                                                    microsecond,
                                                    millisecond);
                       _ ->
                           infinity
                   end
           end,
    unset(watchdog),
    case {Enabled, Time} of
        {_, infinity} -> {false, infinity};
        Other -> Other
    end.

notify_socket() ->
    State = case os:getenv(?NOTIFY_SOCKET) of
                false ->
                    [];
                [$@ | AbstractPath] ->
                    [0 | AbstractPath];
                Path ->
                    case file:read_file_info(Path) of
                        {error, _Error} ->
                            [];
                        {ok, #file_info{access=Access}}
                          when Access =:= write; Access =:= read_write ->
                            Path
                    end
            end,
    unset(notify),
    State.

unset(System) ->
    {ok, Unset} = application:get_env(systemd, unset_env),
    unset(System, Unset).

unset(System, true) -> systemd:unset_env(System);
unset(_System, _) -> ok.
