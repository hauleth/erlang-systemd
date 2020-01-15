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
    unicode:chardata().
-type sd_timeout() :: pos_integer().
-type fd() :: integer() | {integer(), unicode:chardata()}.

-export_type([state/0,
              fd/0,
              sd_timeout/0]).

-include_lib("kernel/include/file.hrl").
-include("systemd.hrl").

-export([notify/1,
         notify/2,
         watchdog/1,
         listen_fds/0,
         listen_fds/1,
         booted/0]).

%% @doc
%% Send notification to the systemd socket.
%%
%% @param `State'
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
normalize_state(Msg) -> Msg.

%% ----------------------------------------------------------------------------

%% @doc
%% Manage watchdog process.
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
    generate_fds(Fd + 1, Count - 1, [], [fd(Fd, "") | Agg]).

fd(Fd, "") -> Fd;
fd(Fd, Name) -> {Fd, Name}.
