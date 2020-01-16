%% @hidden

-module(systemd_sup).

-behaviour(supervisor).

-include("systemd_internal.hrl").

-export([start_link/1, init/1]).

-define(PID, "WATCHDOG_PID").
-define(TIMEOUT, "WATCHDOG_USEC").
-define(NOTIFY_SOCKET, "NOTIFY_SOCKET").

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
    Timeout = case {watchdog_pid(), watchdog_timeout()} of
                   {Pid, TimeoutUS} when TimeoutUS > 0 ->
                       erlang:convert_time_unit(TimeoutUS, microsecond, millisecond);
                   _ -> infinity
               end,
    Watchdog = #{id => watchdog,
                 start => {systemd_watchdog, start_link, [Timeout]}},

    {ok, {SupFlags, [SocketServer, Watchdog]}}.

watchdog_pid() ->
    Return = case os:getenv(?PID) of
                 false -> os:getpid();
                 Env -> Env
             end,
    os:unsetenv(?PID),
    Return.

watchdog_timeout() ->
    Return = case os:getenv(?TIMEOUT) of
                 false -> -1;
                 Env ->
                     case string:to_integer(Env) of
                         {Timeout, ""} -> Timeout;
                         _ -> -1
                     end
             end,
    os:unsetenv(?TIMEOUT),
    Return.

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
    os:unsetenv(?NOTIFY_SOCKET),
    State.
