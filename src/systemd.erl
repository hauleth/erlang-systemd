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
%% @doc
%% Functions for interacting with `systemd' features.
%%
%% @since 0.1.0
%% @end
-module(systemd).

-type state() ::
    ready
    | stopping
    | reloading
    | {status, unicode:chardata()}
    | {errno, non_neg_integer()}
    | {buserror, unicode:chardata()}
    | {extend_timeout, {non_neg_integer(), erlang:time_unit()}}
    | {unicode:chardata() | atom(), unicode:chardata()}.

-type sd_timeout() :: pos_integer().
-type fd() :: integer() | {integer(), unicode:chardata()}.

-export_type([
    state/0,
    fd/0,
    sd_timeout/0
]).

-include_lib("kernel/include/file.hrl").

-include("systemd_internal.hrl").

-export([
    unset_env/1,
    notify/1,
    ready/0,
    watchdog/1,
    listen_fds/0,
    store_fds/1,
    clear_fds/1,
    booted/0,
    is_journal/1
]).

-export([spawn_ready/0]).

%% @doc
%% Unset environment variables for given subsystem.
%%
%% Most environment variables will be cleaned on startup by default. To prevent
%% such behaviour set `unset_env' application variable to `false'.
%%
%% <dl>
%%      <dt>`unset_env(notify)'</dt>
%%      <dd>Unset variables used by {@link notify/1. `notify/1'}. This call will
%%      be done automatically when the `unset_env' application option is set
%%      (default). It is highly encouraged to unset these variables to prevent
%%      them from being passed to subprocesses.</dd>
%%      <dt>`unset_env(watchdog)'</dt>
%%      <dd>Unset variables used by {@link watchdog/1. `watchdog/1'}. This call
%%      will be done automatically when the `unset_env' application option is
%%      set (default). It is highly encouraged to unset these variables
%%      to prevent them from being passed to subprocesses.</dd>
%%      <dt>`unset_env(listen_fds)'</dt>
%%      <dd>Unset variables used by {@link listen_fds/0. `listen_fds/0'}. After
%%      that all subsequent calls to `listen_fds' will return empty list. It is
%%      highly encouraged to unset these variables to prevent them from being
%%      passed to the subprocesses.</dd>
%% </dl>
%%
%% @since 0.4.0
%% @end
-spec unset_env(Subsystem) -> ok when
    Subsystem :: notify | watchdog | listen_fds.
unset_env(notify) ->
    os:unsetenv(?NOTIFY_SOCKET),
    ok;
unset_env(watchdog) ->
    os:unsetenv(?WATCHDOG_PID),
    os:unsetenv(?WATCHDOG_TIMEOUT),
    ok;
unset_env(listen_fds) ->
    os:unsetenv(?LISTEN_PID),
    os:unsetenv(?LISTEN_FDS),
    os:unsetenv(?LISTEN_FDNAMES),
    ok.

%% ----------------------------------------------------------------------------

%% @doc
%% Send notification to the `systemd' socket.
%%
%% == Arguments ==
%%
%% <dl>
%%      <dt>`notify(ready)'</dt>
%%      <dd>Notify that application is ready for work. If used with
%%      `Type=notify' in `systemd.service(5)' file then it will block
%%      `systemctl start' until this is called.</dd>
%%      <dt>`notify(stopping)'</dt>
%%      <dd>Notify that application has already started shutting down, but is
%%      not yet ready to stop. For example when you want to do connection
%%      draining or you have to do some cleaning before fully stopping.
%%
%%      This will be automatically set for you in `systemd''s application
%%      `prep_stop/1' step, so user do not need to call it manually.</dd>
%%      <dt>`notify(reloading)'</dt>
%%      <dd>Notify that application is reloading. It is left up to user what
%%      is considered reloading and handle this call manually.</dd>
%%      <dt>`notify({errno, Errno :: integer()})'</dt>
%%      <dd>Notify that application encountered `Errno' in C's `errno'
%%      format.
%%
%%      Implemented only for feature parity.</dd>
%%      <dt>`notify({buserror, Error :: unicode:chardata()})'</dt>
%%      <dd>Notify about DBus error.</dd>
%%      <dt>`notify({extend_timeout, {integer(), erlang:time_unit()}})'</dt>
%%      <dd>Request extension of timeout for sending `notify(ready)'. This is
%%      useful in case of setups that are taking more time than originally
%%      expected, for example because of retries in connecting to external
%%      service.
%%
%%      This message must be sent within original timeout.</dd>
%% </dl>
%%
%% == Return value ==
%%
%% Returns `ok' on success or if the message was ignored because there is no
%% systemd notify socket. Only error that can be returned as `0.6.0' is when
%% the file descriptors are passed and at least one of them is not correct file
%% descriptor (for example it is closed), then this call will return `{error,
%% bad_descriptor}'.
%%
%% @since 0.1.0
%% @end
-spec notify(State :: state() | [state()]) -> ok.
notify(States) when is_list(States) ->
    systemd_socket:send([normalize_state(State) || State <- States], 0, []);
notify(State) ->
    notify([State]).

normalize_state(ready) ->
    {ready, "1"};
normalize_state(stopping) ->
    {stopping, "1"};
normalize_state(reloading) ->
    {reloading, "1"};
normalize_state({status, Status}) ->
    {"STATUS", Status};
normalize_state({errno, Errno}) when is_integer(Errno) ->
    {"ERRNO", integer_to_binary(Errno)};
normalize_state({buserror, Error}) ->
    {"BUSERROR", Error};
normalize_state({extend_timeout, {Time, Unit}}) when is_integer(Time) ->
    Microsecs = erlang:convert_time_unit(Time, Unit, microsecond),
    {"EXTEND_TIMEOUT_USEC", integer_to_binary(Microsecs)};
normalize_state({_, _} = Msg) ->
    Msg.

%% @doc
%% Returns child spec for task that will inform systemd that application is
%% ready.
%%
%% This is helper function that will return `supervisor:child_spec/0' map that
%% contains temporary job that will notify systemd about application readiness.
%% This is meant to be inserted into your supervison tree when application is
%% ready (usually at the end).
%% @end
ready() ->
    #{
        id => systemd_watchdog_ready,
        start => {?MODULE, spawn_ready, []},
        restart => temporary,
        shutdown => brutal_kill
    }.

%% @hidden helper for `ready/0' function that spawn process that notify about
%% application readiness.
spawn_ready() ->
    {ok, spawn_link(fun() -> systemd:notify(ready) end)}.

%% ----------------------------------------------------------------------------

%% @doc
%% Manage watchdog process.
%%
%% By default `systemd' will handle Watchdog process automatically for you.
%%
%% == Arguments ==
%%
%% <dl>
%%      <dt>`watchdog(state) -> sd_timeout()'</dt>
%%      <dd>Returns state of the Watchdog process. Which either be integer
%%      representing timeout in microseconds or `false' if Watchdog process is
%%      disabled.</dd>
%%      <dt>`watchdog(enable) -> ok'</dt>
%%      <dd>Enable Watchdog process. Watchdog process is automatically enabled
%%      when needed after startup, so this will be only needed if user manually
%%      called `watchdog(disable)'.</dd>
%%      <dt>`watchdog(disable) -> ok'</dt>
%%      <dd>Disable Watchdog process. This will cause no keep-alive messages to
%%      be sent.</dd>
%%      <dt>`watchdog(trigger) -> ok'</dt>
%%      <dd>Manually send keep-alive message to the Watchdog. It will not reset
%%      timer and will not disturb regular process pinging.</dd>
%% </dl>
%%
%% == Options ==
%%
%% <dl>
%%      <dt>`watchdog_scale'</dt>
%%      <dd>Divider of the timeout to send messages more often than this is
%%      required to prevent any jitter.
%%
%%      Defaults to `2' which will send messages twice as often as needed.</dd>
%%      <dt>`watchdog_check'</dt>
%%      <dd>Function (specified as 0-ary function or `mfa()') that will be ran
%%      before pinging watchdog process. Such function should return `true' if
%%      the application is in functioning state and `false' otherwise.
%%
%%      Defaults to function that always return `true'.</dd>
%% </dl>
%%
%% @since 0.1.0
%% @end
-spec watchdog
    (state) -> sd_timeout();
    (trigger) -> ok;
    (enable) -> ok;
    (disable) -> ok;
    (ping) -> ok.
watchdog(ping) ->
    systemd:notify({watchdog, "1"});
watchdog(trigger) ->
    systemd:notify({watchdog, "trigger"});
watchdog(state) ->
    gen_server:call(?WATCHDOG, state);
watchdog(enable) ->
    gen_server:call(?WATCHDOG, enable);
watchdog(disable) ->
    gen_server:call(?WATCHDOG, disable).

%% ----------------------------------------------------------------------------

%% @doc
%% Check if system was booted with systemd.
%%
%% Note that all other functions in this module are safe to call even on
%% non-systemd boots (even on non-systemd platforms). You should <b>NOT</b>
%% protect them with a call to this function. Also note that this checks wheter
%% the system, not the user session, is controlled by systemd. However other
%% functions will work for both - user and system services.
%%
%% @returns `{ok, true}' if system was booted with systemd, `{ok,false}' if not,
%% and `{error, Reason}' on error.
%%
%% @since 0.1.0
%% @end
-spec booted() -> {ok, boolean()} | {error, file:posix()}.
booted() ->
    case file:read_file_info("/run/systemd/system/", [{time, posix}]) of
        {ok, _} -> {ok, true};
        {error, enoent} -> {ok, false};
        Error -> Error
    end.

is_journal(Type) ->
    case get_journal_stream() of
        {Dev, Inode} ->
            case file_info(Type) of
                {ok, #file_info{major_device=Dev, inode=Inode}} -> true;
                _ -> false
            end;
        _ ->
            false
    end.

get_journal_stream() ->
    Env = os:getenv("JOURNAL_STREAM", ""),
    case string:split(Env, ":") of
        [DevStr, InodeStr | _] ->
            try {list_to_integer(DevStr), list_to_integer(InodeStr)}
            catch error:badarg:_ -> error
            end;
        _ -> error
    end.

file_info(standard_io) ->
    file:read_file_info("/dev/stdout");
file_info(standard_error) ->
    file:read_file_info("/dev/stderr");
file_info(_) -> {error, unknown_device}.


%% ----------------------------------------------------------------------------

%% @doc
%% Returns list of file descriptors passed to the application by systemd.
%%
%% @returns List of passed file descriptors. If descriptor have name defined
%% then it will be returned as 2nd value in tuple. Order of returned descriptors
%% is the same as passed in environment.
%%
%% @since 0.2.0
%% @end
-spec listen_fds() -> [fd()].
listen_fds() ->
    case check_listen_pid() of
        true ->
            Count = listen_fds_count(),
            Names = listen_names(),
            generate_fds(Count, Names);
        false ->
            []
    end.

%% @doc
%% Send given file descriptors to supervisor for storage between VM restarts.
%%
%% == Warning ==
%% This currently assumes that the currently used ABI is using 32-bit `int's.
%% This is true at least for IA-32, x86-64, AArch64, AArch32, SPARC, OpenRISC,
%% and RISC-V.
%%
%% @returns `ok' on success, `{error, bad_descriptor}' if any file descriptor in
%% the passed list is invalid (isn't file desciptor or is closed).
%%
%% @since 0.6.0
%% @end
-spec store_fds([fd()]) -> ok | {error, term()}.
store_fds(List) when is_list(List) ->
    {Names, Fds} = build_fds(List),
    systemd_socket:send(
        [
            {"FDSTORE", "1"},
            {"FDNAMES", lists:join($:, Names)}
        ],
        0,
        Fds
    ).

%% @doc
%% Removes given named filedescriptors from the store.
%%
%% @since 0.6.0
%% @end
-spec clear_fds([unicode:chardata()]) -> ok.
clear_fds(Names) ->
    systemd_socket:send(
        [
            {"FDSTOREREMOVE", "1"},
            {"FDNAMES", lists:join($:, Names)}
        ],
        0,
        []
    ).

check_listen_pid() ->
    os:getenv(?LISTEN_PID) == os:getpid().

listen_fds_count() ->
    case os:getenv(?LISTEN_FDS) of
        false ->
            0;
        Env ->
            case string:to_integer(Env) of
                {Value, ""} -> Value;
                _ -> 0
            end
    end.

listen_names() ->
    case os:getenv(?LISTEN_FDNAMES) of
        false -> [];
        Env -> string:split(Env, ":", all)
    end.

generate_fds(Count, Names) ->
    generate_fds(?LISTEN_FDS_START, Count, Names, []).

generate_fds(_, 0, _, Agg) ->
    lists:reverse(Agg);
generate_fds(Fd, Count, [Name | Names], Agg) ->
    generate_fds(Fd + 1, Count - 1, Names, [decode_fd(Fd, Name) | Agg]);
generate_fds(Fd, Count, [], Agg) ->
    generate_fds(Fd + 1, Count - 1, [], [Fd | Agg]).

decode_fd(Fd, "") -> Fd;
decode_fd(Fd, Name) -> {Fd, Name}.

build_fds(List) ->
    {Names, FDs} = build_fds(List, {[], []}),
    {lists:reverse(Names), lists:reverse(FDs)}.

build_fds([], Acc) ->
    Acc;
build_fds([{Fd, Name} | Rest], {ANames, AFds}) when is_integer(Fd) ->
    build_fds(Rest, {[Name | ANames], [Fd | AFds]});
build_fds([Fd | Rest], {ANames, AFds}) when is_integer(Fd) ->
    build_fds(Rest, {["" | ANames], [Fd | AFds]}).
