-module(systemd_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [notify, watchdog, ready, listen_fds, socket, fds].

init_per_testcase(Name, Config0) ->
    PrivDir = ?config(priv_dir, Config0),
    Path = socket_path(PrivDir),
    {ok, Socket} = socket:open(local, dgram, default),
    {ok, _Port} = socket:bind(Socket, #{family => local, path => Path}),
    Config1 = [{socket, Socket}, {path, Path} | Config0],

    case erlang:function_exported(?MODULE, Name, 2) of
        true -> ?MODULE:Name(init, Config1);
        false -> Config1
    end.

%% This hack is needed to reduce length of the socket path which is limited to
%% 104-108 characters (depends on the OS).
-spec socket_path(PrivDir :: file:name()) -> string().
socket_path(PrivDir0) ->
    {ok, Cwd} = file:get_cwd(),
    PrivDir = string:prefix(PrivDir0, Cwd),
    erlang:binary_to_list(iolist_to_binary([".", PrivDir, "systemd.sock"])).

end_per_testcase(Name, Config0) ->
    Config = case erlang:function_exported(?MODULE, Name, 2) of
                 true -> ?MODULE:Name(finish, Config0);
                 false -> Config0
             end,

    _ = application:stop(systemd),
    _ = socket:close(?config(socket, Config)),
    _ = file:delete(?config(path, Config)),

    systemd:unset_env(notify),
    systemd:unset_env(watchdog),
    systemd:unset_env(listen_fds),

    Config.

notify(init, Config) ->
    Socket = ?config(socket, Config),
    ok = start_with_socket(Socket),
    Config;
notify(_, Config) ->
    Config.

notify(Config) ->
    Socket = ?config(socket, Config),

    systemd:notify(ready),
    {ok, <<"READY=1\n">>} = recv(Socket),
    systemd:notify(stopping),
    {ok, <<"STOPPING=1\n">>} = recv(Socket),
    systemd:notify(reloading),
    {ok, <<"RELOADING=1\n">>} = recv(Socket),
    systemd:notify({status, "example status"}),
    {ok, <<"STATUS=example status\n">>} = recv(Socket),
    systemd:notify({errno, 10}),
    {ok, <<"ERRNO=10\n">>} = recv(Socket),
    systemd:notify({buserror, "test.example.bus.service.Error"}),
    {ok, <<"BUSERROR=test.example.bus.service.Error\n">>} = recv(Socket),
    systemd:notify({extend_timeout, {5, microsecond}}),
    {ok, <<"EXTEND_TIMEOUT_USEC=5\n">>} = recv(Socket),
    systemd:notify({extend_timeout, {5, millisecond}}),
    {ok, <<"EXTEND_TIMEOUT_USEC=5000\n">>} = recv(Socket),
    systemd:notify({extend_timeout, {5, second}}),
    {ok, <<"EXTEND_TIMEOUT_USEC=5000000\n">>} = recv(Socket),
    systemd:notify({custom, "message"}),
    {ok, <<"CUSTOM=message\n">>} = recv(Socket),

    ct:log("Connection address persists between process restarts"),
    gen_server:stop(systemd_socket, error, 100),
    systemd:notify(ready),
    {ok, <<"READY=1\n">>} = recv(Socket),
    ok.

watchdog(init, Config) -> Config;
watchdog(finish, Config) ->
    os:unsetenv("WATCHDOG_USEC"),
    os:unsetenv("WATCHDOG_PID"),
    Config.

watchdog(Config) ->
    Socket = ?config(socket, Config),

    Timeout = 200,
    Timeout0 = erlang:convert_time_unit(Timeout,
                                        millisecond,
                                        microsecond),
    TimeoutList = integer_to_list(Timeout0),

    % -------------------------------------------------------------------------
    ct:log("Watchdog sends messages when WATCHDOG_USEC is set"),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    ?assertEqual(Timeout, systemd:watchdog(state)),
    ct:sleep(300),

    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    ok = stop(Config),

    % % -------------------------------------------------------------------------
    ct:log("Watchdog do not send messages when WATCHDOG_PID mismatch"),
    os:putenv("WATCHDOG_PID", "foo"),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    false = systemd:watchdog(state),
    ct:sleep(300),

    ok = empty(Socket),
    ok = stop(Config),

    % % -------------------------------------------------------------------------
    ct:log("Watchdog send messages when WATCHDOG_PID match"),
    os:putenv("WATCHDOG_PID", os:getpid()),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    Timeout = systemd:watchdog(state),
    ct:sleep(300),

    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    ok = stop(Config),

    % % -------------------------------------------------------------------------
    ct:log("Enabling invalid Watchdog does nothing"),
    os:putenv("WATCHDOG_USEC", "0"),
    ok = start_with_socket(Socket),
    false = systemd:watchdog(state),
    ok = empty(Socket),

    systemd:watchdog(enable),
    false = systemd:watchdog(state),
    ok = empty(Socket),
    ok = stop(Config),

    % % -------------------------------------------------------------------------
    ct:log("Watchdog do not send messages when WATCHDOG_USEC is zero"),
    os:putenv("WATCHDOG_USEC", "0"),
    ok = start_with_socket(Socket),
    false = systemd:watchdog(state),
    ct:sleep(300),
    ok = empty(Socket),
    ok = stop(Config),

    % % -------------------------------------------------------------------------
    ct:log("Watchdog do not send messages when WATCHDOG_USEC is non integer"),
    os:putenv("WATCHDOG_USEC", "foo"),
    ok = start_with_socket(Socket),
    false = systemd:watchdog(state),
    ct:sleep(300),
    ok = empty(Socket),
    ok = stop(Config),

    os:putenv("WATCHDOG_USEC", "1.0"),
    ok = start_with_socket(Socket),
    false = systemd:watchdog(state),
    ct:sleep(300),
    ok = empty(Socket),
    ok = stop(Config),

    % % -------------------------------------------------------------------------
    ct:log("Watchdog control functions"),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    flush(Socket),

    ct:log("-> state enabled"),
    ?assertEqual(Timeout, systemd:watchdog(state)),
    ct:sleep(300),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    flush(Socket),

    ct:log("-> disable"),
    ok = systemd:watchdog(disable),
    ct:sleep(300),
    ok = empty(Socket),

    ct:log("-> state disabled"),
    ?assertEqual(false, systemd:watchdog(state)),
    ct:sleep(300),
    ok = empty(Socket),

    ct:log("-> ping disabled"),
    ok = systemd:watchdog(ping),
    ct:sleep(300),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    flush(Socket),

    ct:log("-> enable"),
    ok = systemd:watchdog(enable),
    ct:sleep(300),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    flush(Socket),

    ct:log("-> trigger"),
    ok = systemd:watchdog(trigger),
    {ok, <<"WATCHDOG=trigger\n">>} = recv(Socket),
    flush(Socket),

    ok = stop(Config),

    % % -------------------------------------------------------------------------
    ct:log("Watchdog process send messages after restart"),
    os:putenv("WATCHDOG_PID", os:getpid()),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    ok = empty(Socket),
    gen_server:stop(systemd_watchdog, error, 100),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    ok = empty(Socket),

    ok = stop(Config),

    % % -------------------------------------------------------------------------
    ct:log("By default unsets variables"),
    os:putenv("WATCHDOG_PID", os:getpid()),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = start_with_socket(Socket),
    false = os:getenv("WATCHDOG_PID"),
    false = os:getenv("WATCHDOG_USEC"),

    ok = stop(Config),

    % % -------------------------------------------------------------------------
    ct:log("Do not unset env when unset_env is false"),
    os:putenv("WATCHDOG_PID", os:getpid()),
    os:putenv("WATCHDOG_USEC", TimeoutList),
    ok = application:set_env(systemd, unset_env, false),
    ok = start_with_socket(Socket),
    ?assertEqual(os:getpid(), os:getenv("WATCHDOG_PID")),
    TimeoutList = os:getenv("WATCHDOG_USEC"),
    ok = application:set_env(systemd, unset_env, true),

    ok = stop(Config),

    ok.

ready(init, Config) ->
    ok = start_with_socket(?config(socket, Config)),
    Config;
ready(_, Config) ->
    stop(Config),
    Config.

ready(Config) ->
    Socket = ?config(socket, Config),
    {ok, _Pid} = mock_supervisor:start_link([systemd:ready()]),
    {ok, <<"READY=1\n">>} = recv(Socket),

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
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When no LISTEN_PID environment variable is set it returns empty list"),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "3"),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_PID mismatch it returns empty list"),
    os:putenv("LISTEN_PID", "foo"),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar:baz"),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_PID match returned list has LISTEN_FDS amount of entries"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    ?assertEqual(3, length(systemd:listen_fds())),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_FDS is not set, it is assumed to be 0"),
    os:putenv("LISTEN_PID", os:getpid()),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_FDS is not integer, it is assumed to be 0"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "foo"),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "1.0"),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("Returned list has PIDs with respective names"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar:baz"),
    ?assertMatch([{_, "foo"}, {_, "bar"}, {_, "baz"}], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When subsequent calls will return the same data"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar:baz"),
    First = systemd:listen_fds(),
    First = systemd:listen_fds(),
    systemd:unset_env(listen_fds),
    [] = systemd:listen_fds(),
    [] = systemd:listen_fds(),

    % -------------------------------------------------------------------------
    ct:log("When name is empty it returns raw FD"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo::baz"),
    ?assertMatch([{_, "foo"}, _, {_, "baz"}], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When names list is shorter it returns raw FDs"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar"),
    ?assertMatch([{_, "foo"}, {_, "bar"}, _], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    ok.

socket(Config) ->
    ct:log("~p", [Config]),
    Socket = ?config(socket, Config),

    % -------------------------------------------------------------------------
    ct:log("When started without NOTIFY_SOCKET it is noop"),
    {ok, _} = application:ensure_all_started(systemd),
    ok = systemd:notify(ready),
    ok = empty(Socket),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("When started with invalid NOTIFY_SOCKET it is noop"),
    ok = start_with_path("/non/existent"),
    ok = systemd:notify(ready),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("When started with empty NOTIFY_SOCKET it is noop"),
    ok = start_with_path(""),
    ok = systemd:notify(ready),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("By default unset NOTIFY_SOCKET"),
    ok = start_with_socket(Socket),
    ok = systemd:notify(ready),
    {ok, <<"READY=1\n">>} = recv(Socket),
    ok = empty(Socket),
    false = os:getenv("NOTIFY_SOCKET"),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("Do not unset NOTIFY_SOCKET when unset_env is false"),
    ok = application:set_env(systemd, unset_env, false),
    ok = start_with_socket(Socket),
    ok = systemd:notify(ready),
    {ok, <<"READY=1\n">>} = recv(Socket),
    ok = empty(Socket),
    ?assertEqual(?config(path, Config), os:getenv("NOTIFY_SOCKET")),
    ok = stop(Config),
    ok = application:set_env(systemd, unset_env, true),

    ok.

fds(init, Config) ->
    Socket = ?config(socket, Config),
    ok = start_with_socket(Socket),
    Config;
fds(_, Config) ->
    Config.

fds(Config) ->
    Socket = ?config(socket, Config),
    ok = systemd:store_fds([1]),
    ?assertMatch({ok, #{iov := [<<"FDSTORE=1\n">>],
                        ctrl := [#{type := rights}]}}, recvmsg(Socket)),
    ok = systemd:store_fds([{1, "foo"}]),
    ?assertMatch({ok, #{iov := [<<"FDSTORE=1\nFDNAMES=foo\n">>],
                        ctrl := [#{type := rights}]}}, recvmsg(Socket)),
    ok = systemd:store_fds([1, {2, "foo"}]),
    ?assertMatch({ok, #{iov := [<<"FDSTORE=1\nFDNAMES=:foo\n">>],
                        ctrl := [#{type := rights}]}}, recvmsg(Socket)),
    {error, bad_descriptor} = systemd:store_fds([999]),
    ok = empty(Socket),

    systemd:clear_fds(["foo", "bar"]),
    {ok, <<"FDSTOREREMOVE=1\nFDNAMES=foo:bar\n">>} = recv(Socket),

    ok.

start_with_socket(Socket) ->
    {ok, #{path := Path}} = socket:sockname(Socket),
    start_with_path(Path).

start_with_path(Path) ->
    ct:log("Start app with socket at ~p", [Path]),
    os:putenv("NOTIFY_SOCKET", Path),
    {ok, _} = application:ensure_all_started(systemd),
    ok.

stop(Config) ->
    ok = application:stop(systemd),
    ok = flush(?config(socket, Config)),
    ok.

recv(S) ->
    socket:recv(S, 0, 1000).

recvmsg(S) ->
    socket:recvmsg(S, 0, 1000).

empty(S) ->
    case socket:recv(S, 0, 0) of
        {ok, <<>>} -> ok;
        {error, timeout} -> ok;
        {ok, Msg} -> {error, {received, Msg}};
        {error, _} = Error -> Error
    end.

flush(S) ->
    case socket:recv(S, 0, 0) of
        {ok, _} -> flush(S);
        _ -> ok
    end.
