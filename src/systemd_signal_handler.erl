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
-module(systemd_signal_handler).

-behaviour(gen_event).

-include_lib("kernel/include/logger.hrl").

-export([init/1, handle_call/2, handle_event/2, handle_info/2]).

init(Event) ->
    ?LOG_NOTICE("Listening for ~p", [Event]),
    {ok, Event}.

handle_call(_, State) -> {ok, unsupported, State}.

handle_info(_, State) -> {ok, unsupported, State}.

handle_event(Event, Event) ->
    ?LOG_NOTICE("Received ~p signal: reloading", [Event]),
    systemd:reload(),
    {ok, Event};
handle_event(Other, State) ->
    ?LOG_NOTICE("Received ~p signal: ignoring", [Other]),
    {ok, State}.
