-module(systemd_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [notify, watchdog, listen_fds, socket].

init_per_testcase(Name, Config0) ->
    PrivDir = ?config(priv_dir, Config0),
    Path = mock_systemd:socket_path(PrivDir),
    {ok, Pid} = mock_systemd:start_link(Path),
    Config1 = [{mock_pid, Pid}, {socket, Path} | Config0],

    case erlang:function_exported(?MODULE, Name, 2) of
        true -> ?MODULE:Name(init, Config1);
        false -> Config1
    end.

end_per_testcase(Name, Config0) ->
    Config = case erlang:function_exported(?MODULE, Name, 2) of
                 true -> ?MODULE:Name(finish, Config0);
                 false -> Config0
             end,

    _ = application:stop(systemd),
    ok = gen_server:stop(?config(mock_pid, Config)),

    Config.

notify(init, Config) ->
    ok = start_with_socket(?config(socket, Config)),
    Config;
notify(_, Config) -> Config.

notify(Config) ->
    Pid = ?config(mock_pid, Config),

    systemd:notify(ready),
    systemd:notify(stopping),
    systemd:notify(reloading),
    systemd:notify({status, "example status"}),
    systemd:notify({errno, 10}),
    systemd:notify({buserror, "test.example.bus.service.Error"}),
    systemd:notify("CUSTOM=message"),
    systemd:notify("FORMATTED=~.16b", [16#deadbeef]),

    ct:sleep(10),

    ?assertEqual(["READY=1\n",
                  "STOPPING=1\n",
                  "RELOADING=1\n",
                  "STATUS=example status\n",
                  "ERRNO=10\n",
                  "BUSERROR=test.example.bus.service.Error\n",
                  "CUSTOM=message\n",
                  "FORMATTED=deadbeef\n"], mock_systemd:messages(Pid)),

    ct:log("Connection address persists between process restarts"),
    gen_server:stop(systemd_socket, error, 100),
    ct:sleep(10),
    systemd:notify(ready),
    ct:sleep(10),
    ?assertEqual(["READY=1\n"], mock_systemd:messages(Pid)),
    ok.

watchdog(init, Config) -> Config;
watchdog(finish, Config) ->
    os:unsetenv("WATCHDOG_USEC"),
    os:unsetenv("WATCHDOG_PID"),
    Config.

watchdog(Config) ->
    Pid = ?config(mock_pid, Config),
    Socket = ?config(socket, Config),

    Timeout = erlang:convert_time_unit(200,
                                       millisecond,
                                       microsecond),
    TimeoutList = integer_to_list(Timeout),

    % -------------------------------------------------------------------------
    ct:log("Watchdog sends messages when WATCHDOG_USEC is set"),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    ct:sleep(300),

    Messages0 = mock_systemd:messages(Pid),
    ?assertEqual(["WATCHDOG=1\n", "WATCHDOG=1\n", "WATCHDOG=1\n"], Messages0),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("Watchdog do not send messages when WATCHDOG_PID mismatch"),
    os:putenv("WATCHDOG_PID", "foo"),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    ct:sleep(300),

    Messages1 = mock_systemd:messages(Pid),
    ?assertMatch([], Messages1),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("Watchdog send messages when WATCHDOG_PID match"),
    os:putenv("WATCHDOG_PID", os:getpid()),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    ct:sleep(300),

    Messages2 = mock_systemd:messages(Pid),
    ?assertEqual(["WATCHDOG=1\n", "WATCHDOG=1\n", "WATCHDOG=1\n"], Messages2),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("Watchdog do not send messages when WATCHDOG_USEC is zero"),
    os:putenv("WATCHDOG_USEC", "0"),
    ok = start_with_socket(Socket),
    ct:sleep(300),

    Messages3 = mock_systemd:messages(Pid),
    ?assertEqual([], Messages3),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("Watchdog do not send messages when WATCHDOG_USEC is non integer"),
    os:putenv("WATCHDOG_USEC", "foo"),
    ok = start_with_socket(Socket),
    ct:sleep(300),

    Messages4 = mock_systemd:messages(Pid),
    ?assertEqual([], Messages4),
    ok = stop(Config),

    os:putenv("WATCHDOG_USEC", "1.0"),
    ok = start_with_socket(Socket),
    ct:sleep(300),

    Messages5 = mock_systemd:messages(Pid),
    ?assertEqual([], Messages5),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("Watchdog control functions"),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    ct:sleep(10),
    Messages6 = mock_systemd:messages(Pid),
    ?assertMatch(["WATCHDOG=1\n"], Messages6),

    ct:log("-> state enabled"),
    ?assertEqual(200, systemd:watchdog(state)),
    ct:sleep(300),
    Messages7 = mock_systemd:messages(Pid),
    ?assertEqual(["WATCHDOG=1\n", "WATCHDOG=1\n", "WATCHDOG=1\n"], Messages7),

    ct:log("-> disable"),
    ok = systemd:watchdog(disable),
    ct:sleep(300),
    Messages8 = mock_systemd:messages(Pid),
    ?assertMatch([], Messages8),

    ct:log("-> state disabled"),
    ?assertEqual(false, systemd:watchdog(state)),
    ct:sleep(300),
    Messages9 = mock_systemd:messages(Pid),
    ?assertMatch([], Messages9),

    ct:log("-> ping disabled"),
    ok = systemd:watchdog(ping),
    ct:sleep(300),
    Messages10 = mock_systemd:messages(Pid),
    ?assertMatch(["WATCHDOG=1\n"], Messages10),

    ct:log("-> enable"),
    ok = systemd:watchdog(enable),
    ct:sleep(300),
    Messages11 = mock_systemd:messages(Pid),
    ?assertEqual(["WATCHDOG=1\n", "WATCHDOG=1\n", "WATCHDOG=1\n"], Messages11),

    ct:log("-> trigger"),
    ok = systemd:watchdog(trigger),
    ct:sleep(10),
    Messages12 = mock_systemd:messages(Pid),
    ?assertMatch(["WATCHDOG=trigger\n"], Messages12),

    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("Watchdog process send messages after restart"),
    os:putenv("WATCHDOG_PID", os:getpid()),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    ct:sleep(10),

    ?assertEqual(["WATCHDOG=1\n"], mock_systemd:messages(Pid)),
    gen_server:stop(systemd_watchdog, error, 100),
    ct:sleep(10),
    ?assertEqual(["WATCHDOG=1\n"], mock_systemd:messages(Pid)),

    ok = stop(Config),

    ok.

listen_fds(init, Config) -> Config;
listen_fds(finish, Config) ->
    os:unsetenv("LISTEN_PID"),
    os:unsetenv("LISTEN_FDS"),
    os:unsetenv("LISTEN_FDNAMES"),
    Config.

listen_fds(_Config) ->
    % -------------------------------------------------------------------------
    ct:log("When no environment variables it returns empty list"),
    ?assertEqual([], systemd:listen_fds(true)),

    % -------------------------------------------------------------------------
    ct:log("When no LISTEN_PID environment variable is set it returns empty list"),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "3"),
    ?assertEqual([], systemd:listen_fds(true)),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_PID mismatch it returns empty list"),
    os:putenv("LISTEN_PID", "foo"),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar:baz"),
    ?assertEqual([], systemd:listen_fds(true)),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_PID match returned list has LISTEN_FDS amount of entries"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    ?assertEqual(3, length(systemd:listen_fds(true))),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_FDS is not set, it is assumed to be 0"),
    os:putenv("LISTEN_PID", os:getpid()),
    ?assertEqual([], systemd:listen_fds(true)),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_FDS is not integer, it is assumed to be 0"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "foo"),
    ?assertEqual([], systemd:listen_fds(true)),

    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "1.0"),
    ?assertEqual([], systemd:listen_fds(true)),

    % -------------------------------------------------------------------------
    ct:log("Returned list has PIDs with respective names"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar:baz"),
    ?assertMatch([{_, "foo"}, {_, "bar"}, {_, "baz"}], systemd:listen_fds(true)),

    % -------------------------------------------------------------------------
    ct:log("When false passed next call returns the same data"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar:baz"),
    First = systemd:listen_fds(),
    First = systemd:listen_fds(false),
    First = systemd:listen_fds(true),

    % -------------------------------------------------------------------------
    ct:log("When name is empty it returns raw FD"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo::baz"),
    ?assertMatch([{_, "foo"}, _, {_, "baz"}], systemd:listen_fds(true)),

    % -------------------------------------------------------------------------
    ct:log("When names list is shorter it returns raw FDs"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar"),
    ?assertMatch([{_, "foo"}, {_, "bar"}, _], systemd:listen_fds(true)),

    ok.

socket(Config) ->
    Pid = ?config(mock_pid, Config),

    % -------------------------------------------------------------------------
    ct:log("When started without NOTIFY_SOCKET it is noop"),
    {ok, _} = application:ensure_all_started(systemd),
    ok = systemd:notify(ready),
    ?assertEqual([], mock_systemd:messages(Pid)),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("When started with invalid NOTIFY_SOCKET it is noop"),
    ok = start_with_socket("/non/existent"),
    ok = systemd:notify(ready),
    ?assertEqual([], mock_systemd:messages(Pid)),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("When started with empty NOTIFY_SOCKET it is noop"),
    ok = start_with_socket(""),
    ok = systemd:notify(ready),
    ?assertEqual([], mock_systemd:messages(Pid)),
    ok = stop(Config),

    ok.

start_with_socket(Socket) ->
    ct:log("Start app with socket at ~p", [Socket]),
    os:putenv("NOTIFY_SOCKET", Socket),
    {ok, _} = application:ensure_all_started(systemd),
    ok.

stop(Config) ->
    Pid = ?config(mock_pid, Config),
    ok = application:stop(systemd),
    ct:sleep(10),
    _ = mock_systemd:messages(Pid),
    ok.
