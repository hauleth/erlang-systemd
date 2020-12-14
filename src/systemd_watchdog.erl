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

-module(systemd_watchdog).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-include("systemd_internal.hrl").

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    report_cb/1
]).

-record(state, {timeout, enabled = true}).

start_link(Config) ->
    gen_server:start_link({local, ?WATCHDOG}, ?MODULE, Config, []).

init({Enabled, Timeout}) ->
    State = #state{enabled = Enabled, timeout = Timeout},
    notify(State),
    {ok, State}.

handle_call(enable, _Ref, #state{timeout = T} = State) when
    is_integer(T), T > 0
->
    NewState = State#state{enabled = true},
    notify(NewState),
    {reply, ok, NewState};
handle_call(enable, _Ref, State) ->
    {reply, ok, State};
handle_call(disable, _Ref, State) ->
    {reply, ok, State#state{enabled = false}};
handle_call(state, _Ref, State) ->
    case State of
        #state{enabled = true, timeout = Timeout} ->
            {reply, Timeout, State};
        _ ->
            {reply, false, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(keepalive, State) ->
    notify(State),
    {noreply, State}.

notify(#state{enabled = true, timeout = Timeout}) when
    is_integer(Timeout), Timeout > 2
->
    {ok, Scale} = application:get_env(systemd, watchdog_scale),
    Check =
        case application:get_env(systemd, watchdog_check) of
            {ok, {M, F, A}} -> fun() -> apply(M, F, A) end;
            {ok, F} when is_function(F, 0) -> F;
            undefined -> fun() -> true end
        end,
    try Check() of
        true ->
            ?LOG_DEBUG("Watchdog check is healthy"),
            systemd:notify({watchdog, "1"});
        false ->
            ?LOG_WARNING("Watchdog check is unhealthy");
        Other ->
            ?LOG_ERROR(
                #{
                    type => wrong_ret,
                    ret => Other
                },
                #{
                    report_cb => fun ?MODULE:report_cb/1,
                    watchdog_timeout => Timeout
                }
            )
    catch
        Class:Exception:Stacktrace ->
            ?LOG_ERROR(
                #{
                    type => healthcheck_failed,
                    class => Class,
                    exception => Exception,
                    stacktrace => Stacktrace
                },
                #{
                    report_cb => fun ?MODULE:report_cb/1,
                    watchdog_timeout => Timeout
                }
            )
    end,
    erlang:send_after(Timeout div Scale, self(), keepalive);
notify(_State) ->
    ok.

%% @hidden
report_cb(#{type := wrong_ret, ret := Ret}) ->
    {"Watchdog check returned ~p (boolean was expected)", [Ret]};
report_cb(#{
    type := healthcheck_failed,
    class := Class,
    exception := Exception,
    stacktrace := _Stacktrace
}) ->
    {"Watchdog healthcheck failed with ~p (~p)", [Class, Exception]}.
