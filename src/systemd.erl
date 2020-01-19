-module(systemd).

-type state() ::
    ready |
    stopping |
    reloading |
    watchdog |
    watchdog_trigger |
    {status, unicode:chardata()} |
    {errno, non_neg_integer()} |
    {buserror, unicode:chardata()} |
    {extend_timeout, {non_neg_integer(), erlang:time_unit()}} |
    unicode:chardata().
-type sd_timeout() :: pos_integer().
-type fd() :: integer() | {integer(), unicode:chardata()}.

-export_type([state/0,
              fd/0,
              sd_timeout/0]).

-include_lib("kernel/include/file.hrl").
-include("systemd_internal.hrl").

-export([notify/1,
         notify/2,
         watchdog/1,
         listen_fds/0,
         listen_fds/1,
         booted/0]).

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
%%      <dt>`notify(watchdog)'</dt>
%%      <dd>Equivalent of `watchdog(ping)'.
%%
%%      See {@link watchdog/1. `watchdog/1'}.</dd>
%%      <dt>`notify(watchdog_trigger)'</dt>
%%      <dd>Equivalent of `watchdog(trigger)'.
%%
%%      See {@link watchdog/1. `watchdog/1'}.</dd>
%%      <dt>`notify({errno, Errno :: integer()})'</dt>
%%      <dd>Notify that application encountered `Errno' in C's `errno'
%%      format.
%%
%%      Implemented only for feature parity.</dd>
%%      <dt>`notify({buserror, Error :: unicode:chardata()})'</dt>
%%      <dd>Notify about DBus error.</dd>
%%      <dt>`notify({extend_timeout, {Time :: integer(), Unit :: erlang:time_unit()}})'</dt>
%%      <dd>Request extension of timeout for sending `notify(ready)'. This is
%%      useful in case of setups that are taking more time than originally
%%      expected, for example because of retries in connecting to external
%%      service.
%%
%%      This message must be sent within original timeout.</dd>
%% </dl>
%% @end
-spec notify(State :: state()) -> ok.
notify(State) ->
    systemd_socket:send(normalize_state(State)).

%% @doc
%% Send notification to the systemd socket.
%%
%% This function takes `Format' and `Data' that will be formatted in the same
%% way as `io:fwrite/2'.
%% @end
-spec notify(Format :: io:format(), Data :: [term()]) -> ok.
notify(Format, Data) ->
    systemd_socket:send(io_lib:fwrite(Format, Data)).

%% TODO: Add support for passing FDs to the supervisor
normalize_state(ready) -> "READY=1";
normalize_state(stopping) -> "STOPPING=1";
normalize_state(reloading) -> "RELOADING=1";
normalize_state(watchdog) -> "WATCHDOG=1";
normalize_state(watchdog_trigger) -> "WATCHDOG=trigger";
normalize_state({status, Status}) -> ["STATUS=", Status];
normalize_state({errno, Errno}) -> io_lib:fwrite("ERRNO=~B", [Errno]);
normalize_state({buserror, Error}) -> ["BUSERROR=", Error];
normalize_state({extend_timeout, {Time, Unit}}) ->
    Microsecs = erlang:convert_time_unit(Time, Unit, microsecond),
    io_lib:fwrite("EXTEND_TIMEOUT_USEC=~B", [Microsecs]);
normalize_state(Msg)
  when is_list(Msg); is_binary(Msg) ->
    Msg.

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
%%      <dt>`watchdog(trigger) -> ok'</dt>
%%      <dd>Trigger Watchdog timeout right despite current state.</dd>
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
%% </dl>
%% @end
-spec watchdog(state) -> sd_timeout()
                         ; (trigger) -> ok
                         ; (enable) -> ok
                         ; (disable) -> ok
                         ; (ping) -> ok.
watchdog(state) ->
    gen_server:call(?WATCHDOG, state);
watchdog(trigger) ->
    gen_server:call(?WATCHDOG, trigger);
watchdog(enable) ->
    gen_server:call(?WATCHDOG, enable);
watchdog(disable) ->
    gen_server:call(?WATCHDOG, disable);
watchdog(ping) ->
    gen_server:call(?WATCHDOG, ping).

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
%% @end
-spec booted() -> {ok, boolean()} | {error, file:posix()}.
booted() ->
    case file:read_file_info("/run/systemd/system/", [{time, posix}]) of
        {ok, _} -> {ok, true};
        {error, enoent} -> {ok, false};
        Error -> Error
    end.

%% ----------------------------------------------------------------------------

-define(LISTEN_PID, "LISTEN_PID").
-define(LISTEN_FDS, "LISTEN_FDS").
-define(LISTEN_FDNAMES, "LISTEN_FDNAMES").

%% @equiv listen_fds(false)
-spec listen_fds() -> [fd()].
listen_fds() ->
    listen_fds(false).

%% @doc
%% Returns list of file descriptors passed to the application by systemd.
%%
%% @param `Unset' if true then will unset all environment variables and all
%% consecutive calls will return empty list.
%%
%% @returns List of passed file descriptors. If descriptor have name defined
%% then it will be returned as 2nd value in tuple. Order of returned descriptors
%% is the same as passed in environment.
%% @end
-spec listen_fds(Unset :: boolean()) -> [fd()].
listen_fds(Unset) ->
    Fds = case check_listen_pid() of
              true ->
                  Count = listen_fds_count(),
                  Names = listen_names(),
                  generate_fds(Count, Names);
              false ->
                  []
          end,
    unsetenv_all(Unset),
    Fds.

unsetenv_all(false) -> true;
unsetenv_all(true) ->
    os:unsetenv(?LISTEN_PID),
    os:unsetenv(?LISTEN_FDS),
    os:unsetenv(?LISTEN_FDNAMES).

check_listen_pid() ->
    os:getenv(?LISTEN_PID) == os:getpid().

listen_fds_count() ->
    case os:getenv(?LISTEN_FDS) of
        false -> 0;
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

generate_fds(_, 0, _, Agg) -> lists:reverse(Agg);
generate_fds(Fd, Count, [Name | Names], Agg) ->
    generate_fds(Fd + 1, Count - 1, Names, [fd(Fd, Name) | Agg]);
generate_fds(Fd, Count, [], Agg) ->
    generate_fds(Fd + 1, Count - 1, [], [Fd | Agg]).

fd(Fd, "") -> Fd;
fd(Fd, Name) -> {Fd, Name}.
